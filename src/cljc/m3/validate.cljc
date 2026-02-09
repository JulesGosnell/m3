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
   [m3.uri :refer [parse-uri uri-base]]
   [m3.type :refer [json-object?]]
   [m3.draft :refer [draft->$schema $schema->draft $schema-uri->draft]]
   [m3.vocabulary :refer [draft->default-dialect make-dialect]]))

;; consider https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html - other time types...

#?(:cljs (def fs (js/require "fs")))

#?(:cljs (defn slurp [path] (.readFileSync fs path "utf8")))

;;------------------------------------------------------------------------------

(def latest-$schema (draft->$schema :latest))

;;------------------------------------------------------------------------------

;; TODO: needs a lot of work !

(declare make-context)

;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------

;; marker-stashes for the builtin meta-schemas
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
        ;; Stash path->uri: map this schema path to its inherited id-uri
        c2 (update c2 :path->uri assoc old-p2 (:id-uri c2))
        ;; Capture parent's id-uri before $id processing (used by old-draft $ref)
        c2 (assoc c2 :inherited-id-uri (:id-uri c2))]
    (loop [dialect initial-dialect
           remaining (dialect m2)
           c2 c2
           m2 m2
           acc []
           processed-keys #{}]
      (if (empty? remaining)
        ;; Done - return accumulated checkers and final c2
        [acc c2]

        ;; Process next property
        (let [[[k v] cp] (first remaining)]
          ;; Skip if already processed or if a checker marked this key to skip
          ;; (e.g., $ref in old drafts sets :skip-keys to suppress sibling processing)
          (if (or (contains? processed-keys k)
                  (contains? (:skip-keys c2) k))
            (recur dialect
                   (rest remaining)
                   c2
                   m2
                   acc
                   processed-keys)

            ;; Haven't processed this key yet
            (let [new-p2 (conj old-p2 k)
                  [new-c2 new-m2 f1] (cp k c2 new-p2 m2 v)
                  new-dialect (or (:dialect new-c2) dialect)]

              ;; Check if dialect changed
              (if (identical? dialect new-dialect)
                ;; Same dialect - continue with remaining properties
                (recur dialect
                       (rest remaining)
                       new-c2
                       new-m2
                       (conj acc (list new-p2 f1))
                       (conj processed-keys k))

                ;; Dialect changed! Re-evaluate remaining properties with new dialect
                (recur new-dialect
                       (new-dialect m2)
                       new-c2
                       new-m2
                       (conj acc (list new-p2 f1))
                       (conj processed-keys k))))))))))

;;------------------------------------------------------------------------------

(defn check-schema-2 [{t? :trace? :as c2} p2 m2]
  (cond
    (true? m2)
    [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])]

    (false? m2)
    [c2 m2
     (fn [c1 p1 m1]
       [c1
        m1
        (when (present? m1)
          [(make-error "schema is false: nothing will match" p2 m2 p1 m1)])])]

    :else
    ;; Eagerly compile to build uri->path/path->uri.
    ;; Propagate those maps but preserve original id-uri to prevent scope leaking.
    (let [[checkers final-c2] (compile-m2 c2 p2 m2)
          updated-c2 (-> c2
                         (assoc :uri->path (:uri->path final-c2))
                         (assoc :path->uri (:path->uri final-c2)))]
      [updated-c2
       m2
       (fn [c1 p1 m1]
         (if (present? m1)
           (let [[new-c1 m1 es]
                 (reduce
                  (fn [[c1 m1 acc] [new-p2 cp]]
                    (let [[c1
                           m1
                           [{m :message} :as es]] (cp c1 p1 m1)]
                      (when t? (println (pr-str new-p2) (pr-str p1) (if (seq es) ["❌" m] "✅")))
                      [c1 m1 (concatv acc es)]))
                  [c1 m1 []]
                  checkers)]
             [new-c1
              m1
              (make-error-on-failure "schema: document did not conform" p2 m2 p1 m1 es)])
           [c1 m1 []]))])))

;; At the moment a validation is only a reduction of c1, not c2.
;; Since a draft switch is something that happens at c2-level, I think we need an interceptor...
;; if m2-fn returned [new-c2 m2] perhaps we would not need interceptors !
;; but we would have to thread m2 as well - can we do it - consider...

;; if validation was also a c2 reduction we could use that for vocabularies and maybe the marker-stash
;; investigate...

(def check-schema check-schema-2)

;;------------------------------------------------------------------------------

;; TODO: rename
(def uri-base->dir
  {"http://json-schema.org" "resources/schemas"
   "https://json-schema.org" "resources/schemas"
   "http://localhost:1234" "test-resources/JSON-Schema-Test-Suite/remotes" ;; TODO: should not be in production code
   })

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

(def uri->schema-2 (partial uri->schema uri-base->dir))

(defn uri->continuation [uri-base->dir]
  (let [uri->schema (partial uri->schema uri-base->dir)]
    (fn [c p uri]
      (when-let [m (uri->schema c p uri)] ;; TODO: what if schema is 'false'
        (let [ctx (-> (make-context
                       (-> c
                           (select-keys [:uri->schema :trace? :draft :id-key])
                           (assoc :id-uri (uri-base uri)))
                       m)
                      (assoc :id-uri (uri-base uri))
                      (update :uri->path assoc (uri-base uri) []))
              ;; Eagerly compile remote schema to build uri->path/path->uri.
              ;; Guard against infinite recursion (metaschema loading metaschema).
              ctx (if (:compiling-remote? c)
                    ctx
                    (let [[ctx _m2 _f1] (check-schema (assoc ctx :compiling-remote? true) [] m)]
                      ctx))]
          [ctx [] m])))))

(defn make-context [{draft :draft u->s :uri->schema :as c2} {s "$schema" :as m2}]
  (let [draft (or draft
                  (when s ($schema-uri->draft (uri-base (parse-uri s))))
                  :latest)
        id-key (if (#{:draft3 :draft4} draft) "id" "$id")
        sid (get m2 id-key)
        c2 (if-not u->s (assoc c2 :uri->schema (uri->continuation uri-base->dir)) c2) ;; TODO
        c2 (assoc c2 :draft draft)
        c2 (assoc c2 :id-key id-key)
        ;; Initialize uri->path and path->uri if not already set
        c2 (if (:uri->path c2) c2
               (assoc c2 :uri->path (if sid {(parse-uri sid) []} {})))
        c2 (if (:path->uri c2) c2
               (assoc c2 :path->uri {}))]
    (assoc
     c2
     :id-uri (or (:id-uri c2) (when sid (parse-uri sid))) ;; should be receiver uri - but seems to default to id/$id - yeugh
     :original-root m2
     :recursive-anchor []
     :root m2
     :strict-integer? (let [f? (get c2 :strict-integer?)] (if (nil? f?) false f?)) ;; pull this out into some default fn
     )))

;; TODO: rename :root to ?:expanded?
(defn validate-2 [c2 schema]
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
                :uri->path (merge (:uri->path c1) (:uri->path c2))
                :path->uri (merge (:path->uri c1) (:path->uri c2)))
            [c1 _m1 es] (cs c1 [] document)]
        [c1 es]))))

(defn $schema->m2 [s]
  (uri->schema-2 {} [] (parse-uri s)))

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
(defn validate-m2-2 [{draft :draft :as c2} m1]
  (let [s (or (and (json-object? m1) (m1 "$schema")) (draft->$schema draft))]
    (if-let [{$vocabulary "$vocabulary" :as m2} ($schema->m2 s)]
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
              v (validate-2 c2 m2)
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
        ;; keep going - inheriting relevant parts of c1
        (let [[{vs :dialect u->p :uri->path p->u :path->uri} es :as r] ((validate-m2 c2 m2) {} m1)]
          (if (empty? es)
            (validate-2 (assoc c2
                               :uri->path (or u->p {})
                               :path->uri (or p->u {})
                               :dialect vs) m1)
            (constantly r))))
      (constantly [c2 [(str "could not resolve $schema: " s)]]))))

(def validate-m2 (memoize validate-m2-2))

(defn reformat [[_ es]]
  {:valid? (empty? es) :errors es})

(defn validate
  ([c2 m2 c1 m1]
   (reformat ((validate-m2 (assoc c2 :m2? true) m2) c1 m1))))
