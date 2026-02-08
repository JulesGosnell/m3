;; Copyright 2025 Julian Gosnell
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns m3.validate
  (:require
   [clojure.string :refer [ends-with? starts-with?]]
   #?(:clj [clojure.java.io :as io])
   [#?(:clj clojure.tools.logging :cljs m3.log) :as log]
   [m3.platform :refer [json-decode]]
   [m3.util :refer [present? concatv make-error make-error-on-failure]]
   [m3.uri :refer [parse-uri inherit-uri uri-base]]
   [m3.type :refer [json-object?]]
   [m3.draft :refer [draft->$schema $schema->draft $schema-uri->draft]]
   [m3.vocabulary :refer [draft->default-dialect make-dialect]]))

;; Architecture: Two-level currying (L2 compile → L1 validate)
;;
;;   L2 (compile time): check-property-*(property, c2, p2, m2, v2) → [c2, m2, f1]
;;   L1 (runtime):      f1(c1, p1, m1) → [c1, m1, errors]
;;
;;   c2 = compile-time context    c1 = runtime context
;;   p2 = schema path             p1 = document path
;;   m2 = schema (being compiled) m1 = document (being validated)
;;   v2 = property value in schema
;;   f1 = compiled checker function
;;
;; c2 carries schema compilation state: draft, dialect, id-uri, uri↔path stash,
;;   root schema, c2-atom (for late-bound $ref), and user options.
;; c1 carries runtime validation state: evaluation tracking (matched/evaluated
;;   sets for unevaluated*), if/then/else results, dynamic anchor scope,
;;   and content decoding state.
;; Both contexts are threaded functionally — each checker receives and returns
;; its context, accumulating state as compilation/validation progresses.

#?(:cljs (def fs (js/require "fs")))

#?(:cljs (defn slurp [path] (.readFileSync fs path "utf8")))

;;------------------------------------------------------------------------------

(def latest-$schema (draft->$schema :latest))

;;------------------------------------------------------------------------------

;; TODO: needs a lot of work !

(declare make-context)

;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------

;; Hand-built marker stashes for the builtin meta-schemas.
;;
;; At the top of the JSON Schema hierarchy, a meta-schema validates itself
;; (e.g. draft2020-12/schema's $schema points to itself). To ground out this
;; recursion without needing json-walk, we provide pre-built stashes here.
;; Normal schemas discover their $id/$anchor entries during compilation, but
;; meta-schemas need their stash to already exist before compilation begins.
;;
;; Each entry maps a meta-schema URI to its {:uri->path, :path->uri} stash.
;; To add support for a new top-level meta-schema, add an entry here with
;; at minimum the root mapping (uri → [], [] → uri) plus any anchors defined
;; at the top level of that meta-schema.
(let [draft3 (parse-uri "http://json-schema.org/draft-03/schema#")
      draft4 (parse-uri "http://json-schema.org/draft-04/schema#")
      draft6 (parse-uri "http://json-schema.org/draft-06/schema#")
      draft7 (parse-uri "http://json-schema.org/draft-07/schema#")
      draft2019-09 (parse-uri "https://json-schema.org/draft/2019-09/schema")
      draft2020-12 (parse-uri "https://json-schema.org/draft/2020-12/schema")]
  (def uri->marker-stash
    {draft3
     {:uri->path {draft3 []},
      :path->uri {[] draft3}},
     draft4
     {:uri->path {draft4 []},
      :path->uri {[] draft4}},
     draft6
     {:uri->path {draft6 []},
      :path->uri {[] draft6}},
     draft7
     {:uri->path {draft7 []},
      :path->uri {[] draft7}},
     draft2019-09
     {:uri->path {draft2019-09 []},
      :path->uri {[] draft2019-09}},
     draft2020-12
     {:uri->path {draft2020-12 [],
                  ;; there is a $dynamicAnchor at top-level - figure it out later...
                  {:type :url, :origin "https://json-schema.org", :path "/draft/2020-12/schema", :fragment "meta"} []},
      :path->uri {[] draft2020-12}}}))

(defn compile-m2 [{dialect :dialect draft :draft :as c2} old-p2 m2]
  (let [effective-draft (or draft :draft2020-12)
        initial-dialect (or dialect (draft->default-dialect effective-draft))
        ;; Save the id-uri at entry — this is the parent schema's scope.
        ;; Old-draft $ref uses this to ignore sibling $id.
        c2 (assoc c2 :schema-parent-id-uri (:id-uri c2))]
    (loop [dialect initial-dialect
           remaining (dialect m2)
           c2 c2
           m2 m2
           acc []
           processed-keys #{}]
      (if (empty? remaining)
        [c2 acc]

        (let [[[k v] cp] (first remaining)]
          (if (or (contains? processed-keys k)
                  (contains? (:skip-keys c2) k))
            (recur dialect
                   (rest remaining)
                   c2
                   m2
                   acc
                   processed-keys)

            (let [new-p2 (conj old-p2 k)
                  parent-id-uri (:id-uri c2)
                  [new-c2 new-m2 f1] (cp k c2 new-p2 m2 v)
                  ;; Only $id/id should change id-uri for sibling properties.
                  ;; All other property checkers may accumulate stash entries
                  ;; but must not leak their sub-schemas' id-uri to siblings.
                  id-key (:id-key c2)
                  new-c2 (if (= k id-key)
                           new-c2
                           (assoc new-c2 :id-uri parent-id-uri))
                  new-dialect (or (:dialect new-c2) dialect)]

              (if (identical? dialect new-dialect)
                (recur dialect
                       (rest remaining)
                       new-c2
                       new-m2
                       (conj acc (list new-p2 f1))
                       (conj processed-keys k))

                (recur new-dialect
                       (new-dialect m2)
                       new-c2
                       new-m2
                       (conj acc (list new-p2 f1))
                       (conj processed-keys k))))))))))

;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------

(defn check-schema [c2 p2 m2]
  (cond
    (true? m2)
    [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])]

    (false? m2)
    [c2 m2 (fn [c1 p1 m1]
             [c1 m1
              (when (present? m1)
                [(make-error "schema is false: nothing will match" p2 m2 p1 m1)])])]

    :else
    (let [c2-atom (or (:c2-atom c2) (atom nil))
          c2 (assoc c2 :c2-atom c2-atom)
          [c2 checkers] (compile-m2 c2 p2 m2)
          _ (reset! c2-atom c2)
          ;; Collect $dynamicAnchors from this schema's compile-time stash
          ;; that belong to the current resource but are NOT at p2 itself.
          ;; These are anchors inside $defs (whose f1 is a no-op at runtime),
          ;; so they would never reach c1 otherwise.
          resource-id-uri (:id-uri c2)
          defs-dynamic-anchors
          (when-let [da (:$dynamic-anchor c2)]
            (let [resource-base (when resource-id-uri (uri-base resource-id-uri))]
              (when resource-base
                (not-empty
                 (into {}
                       (keep (fn [[anchor-uri anchor-path]]
                               (let [anchor-name (:fragment anchor-uri)]
                                 (when (and anchor-name
                                            (= (uri-base anchor-uri) resource-base)
                                            (not= anchor-path p2))
                                   (let [schema (get-in (:root c2) anchor-path)]
                                     (when (and (map? schema)
                                                (= (get schema "$dynamicAnchor") anchor-name))
                                       [anchor-name {:schema schema :c2-atom c2-atom}]))))))
                       da)))))]
      [c2
       m2
       (fn [c1 p1 m1]
         (if (present? m1)
           (let [;; Pre-stash resource-local $dynamicAnchors into c1.
                 ;; "First wins": only set if not already present (outermost scope wins).
                 c1 (if defs-dynamic-anchors
                      (reduce-kv
                       (fn [c anchor-name anchor-info]
                         (update-in c [:$dynamic-anchor-schema anchor-name]
                                    (fn [existing] (or existing anchor-info))))
                       c1
                       defs-dynamic-anchors)
                      c1)
                 [new-c1 m1 es]
                 (reduce
                  (fn [[c1 m1 acc] [new-p2 cp]]
                    (let [[c1 m1 [{m :message} :as es]] (cp c1 p1 m1)]
                      [c1 m1 (concatv acc es)]))
                  [c1 m1 []]
                  checkers)]
             [new-c1
              m1
              (make-error-on-failure "schema: document did not conform" p2 m2 p1 m1 es)])
           [c1 m1 []]))])))


;;------------------------------------------------------------------------------

(def uri-base->dir
  {"http://json-schema.org" "resources/schemas"
   "https://json-schema.org" "resources/schemas"})

;; TODO: uris in sub-schema must inherit from uris in super-schema...
(defn uri->schema [uri-base->dir _c _p {origin :origin path :path}]
  (if-let [dir (uri-base->dir origin)]
    (let [f (str dir path (if (ends-with? path ".json") "" ".json"))
          s (try
              #?(:clj (if-let [resource (io/resource
                                         ;; Strip "resources/" prefix for resource loading
                                         (if (starts-with? f "resources/")
                                           (subs f 10)
                                           f))]
                        ;; Resource found on classpath (e.g., schemas in jar)
                        (json-decode (slurp resource))
                        ;; Fall back to file system (e.g., test-resources or running from source)
                        (json-decode (slurp f)))
                 :cljs (json-decode (slurp f)))
              (catch #?(:cljs js/Error :clj Exception) _))]
      ;; TODO: ref resolution needs to be done in new context...
      s)))

(def uri->schema* (partial uri->schema uri-base->dir))

(defn uri->continuation [uri-base->dir]
  (let [uri->schema (partial uri->schema uri-base->dir)]
    (fn [c p uri]
      (when-let [m (uri->schema c p uri)] ;; TODO: what if schema is 'false'
        (let [base (uri-base uri)
              compiling (or (:compiling c) #{})
              ctx (-> (make-context
                       (-> c
                           (select-keys [:uri->schema :draft :id-key :quiet?])
                           (assoc :id-uri base :compiling compiling))
                       m)
                      (assoc :id-uri base)
                      (update :uri->path assoc base [])
                      (update :path->uri assoc [] base))
              ;; Compile the remote schema to populate uri->path and path->uri
              ;; with any $id/$anchor entries found inside it.
              ;; Guard against re-entrant compilation (e.g. metaschema $schema
              ;; pointing to itself) to prevent infinite recursion.
              ctx (if (contains? compiling base)
                    ctx
                    (let [compile-ctx (update ctx :compiling conj base)
                          [compiled-ctx _m2 _f1] (check-schema compile-ctx [] m)]
                      (assoc ctx
                             :uri->path (:uri->path compiled-ctx)
                             :path->uri (:path->uri compiled-ctx))))]
          [ctx [] m])))))

(defn make-context [{draft :draft u->s :uri->schema :as c2} {s "$schema" :as m2}]
  (let [draft (let [d (or draft
                        (when s ($schema-uri->draft (uri-base (parse-uri s))))
                        :draft2020-12)]
                (if (= d :latest) :draft2020-12 d))
        id-key (if (#{:draft3 :draft4} draft) "id" "$id")
        sid (get m2 id-key)
        c2 (if-not u->s (assoc c2 :uri->schema (uri->continuation uri-base->dir)) c2) ;; TODO
        c2 (assoc c2 :draft draft)
        c2 (assoc c2 :id-key id-key)]
    (assoc
     c2
     :id-uri (or (:id-uri c2) (when sid (parse-uri sid))) ;; should be receiver uri - but seems to default to id/$id - yeugh
     :uri->path (or (:uri->path c2) {})
     :path->uri (or (:path->uri c2) {})
     :original-root m2
     :recursive-anchor []
     :root m2
     :strict-integer? (let [f? (get c2 :strict-integer?)] (if (nil? f?) false f?)) ;; pull this out into some default fn
     )))

;; TODO: rename :root to ?:expanded?
(defn validate* [c2 schema]
  (let [{draft :draft id-key :id-key :as c2} (make-context c2 schema)
        [c2 m2 cs] (check-schema c2 [] schema)]
    (fn [c1 {did id-key _dsid "$schema" :as document}]
      (let [c1 (assoc
                c1
                :id-key id-key
                :id-uri (when did (parse-uri did))
                :original-root document
                :recursive-anchor []
                :root document
                :draft draft
                :melder (:melder c2)
                ;; Store root compilation c2-atom for $dynamicRef resolution.
                ;; Remote schemas' $dynamicRef can fall back to the root c2
                ;; to find $dynamicAnchors that are in $defs (whose f1 is
                ;; a no-op at runtime, so they never reach c1).
                :$root-c2-atom (:c2-atom c2))
            [c1 _m1 es] (cs c1 [] document)]
        [c1 es]))))

(defn $schema->m2 [{u->s :uri->schema} s]
  (let [uri (parse-uri s)]
    (or (uri->schema* {} [] uri)
        (when u->s
          (let [result (u->s {} [] uri)]
            (when result (nth result 2)))))))

(declare validate-m2)

;; TODO: simplify dialect code
;; - recurse to top of schema hierarchy
;; - make a [default] dialect
;; - on way back down hierarchy
;;  - if $vocabulary present build new dialect otherwise inherit from meta-schema
;; - we should probably be recalculating draft at each level as well
;; I think this function needs decomposing and unit testing - it's too complicated...
;; and we need to be able to reuse the code when switching drafts or schema contexts...
;; switching context will affect dialect, marker stash, draft etc... - consider
(defn validate-m2-impl [{draft :draft :as c2} m1]
  (let [s (or (and (json-object? m1) (get m1 "$schema")) (draft->$schema draft))]
    (if-let [{$vocabulary "$vocabulary" :as m2} ($schema->m2 c2 s)]
      (if (= m2 m1)
        ;; we are at the top
        (let [draft ($schema->draft s)
              c2 (assoc c2
                        :dialect (if $vocabulary (make-dialect draft $vocabulary) (draft->default-dialect draft))) ;; handle drafts that are too early to know about $vocabulary
              uri (parse-uri s) ;; duplicate work
              stash (uri->marker-stash uri)
              _ (when-not stash (prn "NO STASH FOR:" s))
              ;; initialise c2`
              ;; only a meta-schema defines a dialect;; this is inherited by its instances
              c2 (assoc
                  (merge c2 stash)
                  :id-uri uri)
              v (validate* c2 m2)
              [_ es :as r] (v {} m1)]
          (if (empty? es)
            ;; return a validator (f1) that will build its own dialect or inherit its meta-schema's
            (fn [c1 {$vocabulary "$vocabulary" :as m1}]
              (v
               (assoc c1
                      ;; think about draft
                      :dialect (or (and $vocabulary (make-dialect draft $vocabulary)) (c2 :dialect)))
               m1))
            (constantly r)))
        ;; keep going - c1 from meta-schema validation becomes c2 for next level
        (let [[{vs :dialect u->p :uri->path p->u :path->uri} es :as r] ((validate-m2 c2 m2) {} m1)]
          (if (empty? es)
            (validate* (assoc c2
                               :uri->path (or u->p {})
                               :path->uri (or p->u {})
                               :dialect vs) m1)
            (constantly r))))
      (constantly [c2 [(str "could not resolve $schema: " s)]]))))

;; Unbounded memoization: cache grows with distinct [c2, schema] pairs.
;; In practice bounded by the number of unique schemas validated (not documents).
;; Replace with LRU cache if memory pressure is observed in long-running processes.
(def validate-m2 (memoize validate-m2-impl))

(defn reformat [[_ es]]
  {:valid? (empty? es) :errors es})

(defn validate
  ([c2 m2 c1 m1]
   (reformat ((validate-m2 (assoc c2 :m2? true) m2) c1 m1))))
