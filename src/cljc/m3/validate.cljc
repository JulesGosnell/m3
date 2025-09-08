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
   [clojure.string :refer [ends-with?]]
   [#?(:clj clojure.tools.logging :cljs m3.log) :as log]
   [m3.platform :refer [json-decode]]
   [m3.util :refer [present? concatv make-error make-error-on-failure]]
   [m3.uri :refer [parse-uri inherit-uri uri-base]]
   [m3.type :refer [json-object?]]
   [m3.ref :refer [meld resolve-uri try-path]]
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

(defn merge-helper [old-c parent [c p child :as x]]
  (when x
    [c p (meld old-c parent child)]))

;;------------------------------------------------------------------------------
;; d/$id/$anchor/$ref

(defn stash-$id [{t? :trace? :as c} _p _m id]
  (update
   c
   :id-uri
   (fn [old new]
     (let [uri (inherit-uri old new)]
       (when t? (prn "base-uri:" old "->" uri))
       uri))
   (parse-uri id)))

(defn expand-$ref [{id-uri :id-uri :as old-c} old-p old-m r]
  (when-let [[new-c new-p new-m] (merge-helper old-c old-m (resolve-uri old-c old-p (inherit-uri id-uri (parse-uri r)) r))]
    [(if (= old-c new-c) (assoc-in old-c (concat [:root] old-p) new-m) new-c)
     new-p
     new-m]))

;;------------------------------------------------------------------------------
;; $recursiveAnchor/Ref - 2019-09 only - sigh

(defn stash-$recursive-anchor [c p _m a]
  (if a
    (update c :$recursive-anchor (fn [[uris top] uri] (if top [(conj uris uri) top] [#{} p])) (c :id-uri))
    (log/warn "$recursiveAnchor: unexpected value:" (pr-str a))))

(defn expand-$recursive-ref [{[uris top] :$recursive-anchor :as c} p m r]
  (if (= "#" r)
    (let [uri (:id-uri c)]
      (merge-helper
       c
       m
       (or
        (try-path c p (and uris (uris uri) top) (c :original-root))
        (resolve-uri c p uri r))))
    (log/warn "$recursiveRef: unexpected value:" (pr-str r))))

;;------------------------------------------------------------------------------
;; $dynamicAnchor/Ref - replaces $recursive-... in 2020-12

(defn stash-$dynamic-anchor [c p _m a]
  (let [uri (inherit-uri (c :id-uri) (parse-uri (str "#" a)))]
    (update-in c [:$dynamic-anchor uri] (fn [old new] (if old old new)) p)))

(defn expand-$dynamic-ref [{da :$dynamic-anchor :as c} p m r]
  (let [uri (inherit-uri (:id-uri c) (parse-uri r))]
    (merge-helper
     c
     m
     (or
      (try-path c p (get da uri) (c :original-root))
      (resolve-uri c p uri r)))))

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
      :path->uri (constantly draft3)},
     draft4
     {:uri->path {draft4 []},
      :path->uri (constantly draft4)},
     draft6
     {:uri->path {draft6 []},
      :path->uri (constantly draft6)},
     draft7
     {:uri->path {draft7 []},
      :path->uri (constantly draft7)},
     draft2019-09
     {:uri->path {draft2019-09 []},
      :path->uri (constantly draft2019-09)},
     draft2020-12
     {:uri->path {draft2020-12 [],
                  ;; there is a $dynamicAnchor at top-level - figure it out later...
                  {:type :url, :origin "https://json-schema.org", :path "/draft/2020-12/schema", :fragment "meta"} []},
      ;; should this be returning the dynamic anchor?
      :path->uri (constantly draft2020-12)}}))

(defn compile-m2 [{vs :dialect d :draft :as c2} old-p2 m2]
  (let [vs (if vs vs (draft->default-dialect d))]
    (let [[_c2 _m2 p2-and-f1s]
          (reduce
           (fn [[c2 m2 acc] [[k v] cp]]
             (let [new-p2 (conj old-p2 k)
                   [c2 m2 f1] (cp k c2 new-p2 m2 v)]
               [c2 m2 (conj acc (list new-p2 f1))]))
           [c2 m2 []]
           (vs m2))]
      p2-and-f1s)))

;;------------------------------------------------------------------------------
;; tmp solution - does not understand about schema structure

;; acc travels around and gets returned
;; stuff travels down and does not
(defn json-walk [f acc stuff path tree]
  (let [[acc stuff] (if (map? tree) (f acc stuff tree path) [acc stuff])]
    (if (or (map? tree) (vector? tree))
      (reduce-kv
       (fn [acc k v]
         (json-walk f acc stuff (conj path k) v))
       acc
       tree)
      acc)))

(defn stash-anchor [[acc {id-uri :id-uri :as stuff} :as x] path fragment? id? anchor]
  (if (string? anchor)
    (let [anchor-uri (inherit-uri id-uri (parse-uri (str (when fragment? "#") anchor)))]
      [(update acc :uri->path assoc anchor-uri path)
       (if id? (assoc stuff :id-uri anchor-uri) stuff)])
    x))

(defn get-id [{d :draft} m]
  (case d
    (:draft3 :draft4) (get m "id")
    (or (get m "$id") (get m "id"))))

(defn stash [acc stuff {a "$anchor" da "$dynamicAnchor" :as m} path]
  (-> [acc stuff]
      ((fn [[acc {id-uri :id-uri :as stuff}]] [(update acc :path->uri assoc path id-uri) stuff]))
      (stash-anchor path false true (get-id acc m))
      (stash-anchor path true false a)
      (stash-anchor path true false da)))

;;------------------------------------------------------------------------------

(defn check-schema-2 [{t? :trace? :as c2} p2 m2]
  ;; TODO; this needs to be simplified
  [c2
   m2
  (cond
    (true? m2)
    (fn [c1 _p1 m1]
      [c1 m1 nil])

    (false? m2)
    (fn [c1 p1 m1]
      [c1
       m1
       (when (present? m1)
         [(make-error "schema is false: nothing will match" p2 m2 p1 m1)])])

    :else
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
               (compile-m2 c2 p2 m2))]
          [new-c1 ;; (first (stash new-c1 {} m1 p1))
           m1
           (make-error-on-failure "schema: document did not conform" p2 m2 p1 m1 es)])
        [c1 m1 []])))])

;; quicker than actual 'apply' [?]
(defn apply3 [f [c p m]]
  (f c p m))

;; At the moment a validation is only a reduction of c1, not c2.
;; Since a draft switch is something that happens at c2-level, I think we need an interceptor...
;; if m2-fn returned [new-c2 m2] perhaps we would not need interceptors !
;; but we would have to thread m2 as well - can we do it - consider...

;; if validation was also a c2 reduction we could use that for vocabularies and maybe the marker-stash
;; investigate...

;; TODO: replace with check-property-$schema...
(defn make-draft-interceptor []
  ;; TODO:
  ;; we should be recursing up our own schema hierarchy to reet our marker stash, dialect etc
  (fn [delegate]
    (fn [c2 p2 {s "$schema" :as m2}]
      (delegate
       ;; c2
       (if-let [d (and s ($schema->draft s))]
         (update
          c2
          :draft
          (fn [old-d new-d]
            ;; (when (not= old-d new-d)
            ;;   (log/info (str "switching draft: " old-d " -> " new-d)))
            new-d)
          d)
         c2)
       p2
       m2))))

(defn make-ref-interceptor [k merger]
  (fn [delegate]
    (fn this [c2 p2 {r k :as m2}]
      (if r
        (fn [c1 p1 m1]
          (when (present? m1)
            (let [new-m2 (dissoc m2 k)]
              ((apply3 this (or (merger c2 p2 new-m2 r) [c2 p2 new-m2])) c1 p1 m1))))
        (delegate c2 p2 m2)))))

(defn make-anchor-interceptor [kf stasher]
  (fn [delegate]
    (fn this [c2 p2 old-m2]
      (let [k (kf c2)]
        (if-let [a (get old-m2 k)] ;; TODO - how can m2 be nil ?
          ;; TODO: not sure we should be deleting anchor...
          (let [new-m2
                old-m2 ;;(dissoc old-m2 k)
                ]
            (delegate (or (stasher c2 p2 new-m2 a) c2) p2 new-m2))
          (delegate c2 p2 old-m2))))))

;;------------------------------------------------------------------------------
;; interceptor stack will be decommissioned rather than migrated...

(defn new->old [cs]
  (fn [c2 p2 m2]
    (let [[c2 m2 f1] (cs c2 p2 m2)]
      (fn [c1 p1 m1]
        (let [[c1 m1 es] (f1 c1 p1 m1)]
          [c1 es])))))

(defn old->new [cs]
  (fn [c2 p2 m2]
    (let [f1 (cs c2 p2 m2)]
      [c2
       m2
       (fn [c1 p1 m1]
         (let [[c1 es] (f1 c1 p1 m1)]
            -           [c1 m1 es]))])))

(def check-schema
  (old->new
   ((make-ref-interceptor "$dynamicRef" expand-$dynamic-ref)
    ((make-ref-interceptor "$recursiveRef" expand-$recursive-ref)
     ((make-ref-interceptor "$ref" expand-$ref)
      ((make-anchor-interceptor (constantly "$dynamicAnchor") stash-$dynamic-anchor)
       ((make-anchor-interceptor (constantly "$recursiveAnchor") stash-$recursive-anchor)
        ((make-anchor-interceptor :id-key stash-$id)
         ((make-draft-interceptor)
          (new->old check-schema-2))))))))))

;;------------------------------------------------------------------------------

;; TODO: rename
(def uri-base->dir
  {"http://json-schema.org" "resources/schemas"
   "https://json-schema.org" "resources/schemas"
   "http://localhost:1234" "test-resources/JSON-Schema-Test-Suite/remotes" ;; TODO: should not be in production code
   })

;; TODO: uris in sub-schema must inherit fromuris in super-schema...
(defn uri->schema [uri-base->dir _c _p {origin :origin path :path}]
  (if-let [dir (uri-base->dir origin)]
    (let [f (str dir path (if (ends-with? path ".json") "" ".json"))
          s (try (json-decode (slurp f)) (catch #?(:cljs js/Error :clj Exception) _))]
      ;;(log/info "uri->schema: loaded: " (pr-str (str origin path)) " -> " (pr-str f))
      ;; TODO: ref resolution needs to be done in new context...
      s)
    ;;(log/info "uri->schema: could not load: " (pr-str (str origin path)))
    ))

(def uri->schema-2 (partial uri->schema uri-base->dir))

(defn uri->continuation [uri-base->dir]
  (let [uri->schema (partial uri->schema uri-base->dir)]
    (fn [c p uri]
      (when-let [m (uri->schema c p uri)] ;; TODO: what if schema is 'false'
        [(-> (make-context
              (-> c
                  (select-keys [:uri->schema :trace? :draft :id-key])
                  (assoc :id-uri (uri-base uri)))
              m)
             (assoc :id-uri (uri-base uri))
             (update :uri->path assoc (uri-base uri) []))
         []
         m]))))

(defn marker-stash [{pu :path->uri up :uri->path}]
  {:path->uri pu :uri->path up})

(defn add-marker-stash [c m sid]
  (if (or (:path->uri c) (:uri->path c))
    c
    (let [tmp (json-walk stash (assoc c :uri->path (if sid {(parse-uri sid) []} {})) {} [] m)]
      ;; (prn "M2:" m)
      ;; (prn "OLD:" (marker-stash tmp))
      ;; (prn "NEW:" (:marker-stash c))
      tmp)))

(defn make-context [{draft :draft u->s :uri->schema :as c2} {s "$schema" :as m2}]
  (let [draft (or draft
                  (when s ($schema-uri->draft (uri-base (parse-uri s))))
                  :latest)
        id-key (if (#{:draft3 :draft4} draft) "id" "$id")
        sid (get m2 id-key)
        c2 (if-not u->s (assoc c2 :uri->schema (uri->continuation uri-base->dir)) c2) ;; TODO
        c2 (assoc c2 :draft draft)
        c2 (assoc c2 :id-key id-key)
        c2 (add-marker-stash c2 m2 sid)]
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
      ;;(log/info "validate:" sid "/" did)
      ;;(when (and dsid (not (= sid dsid))) (log/warn (pformat "document schema id not consistent with schema id: %s != %s" dsid sid)))
      (let [c1 (assoc
                c1
                :id-key id-key
                :id-uri (when did (parse-uri did))
                :original-root document
                :recursive-anchor []
                :root document
                :draft draft
                :melder (:melder c2))
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
                        :dialect (if $vocabulary (make-dialect draft $vocabulary) (draft->default-dialect draft))
                        ) ;; handle drafts that are too early to know about $vocabulary
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
                      :dialect (or (and $vocabulary (make-dialect draft $vocabulary)) (c2 :dialect))
                      )
               m1))
            (constantly r)))
        ;; keep going - inheriting relevant parts of c1
        (let [[{vs :dialect u->p :uri->path p->u :path->uri} es :as r] ((validate-m2 c2 m2) {} m1)]
          ;;(prn "STASH:" u->p p->u)
          (if (empty? es)
            (validate-2 (assoc c2
                               :marker-stash {:uri->path (or u->p {}) :path->uri (or p->u {})}
                               :dialect vs
                               ) m1)
            (constantly r))))
      (constantly [c2 [(str "could not resolve $schema: " s)]]))))

(def validate-m2 (memoize validate-m2-2))

(defn reformat [[_ es]]
  {:valid? (empty? es) :errors es})

;; TODO: handle non object docs - arrays, strings, booleans...
;; (defn validate-m1 [c2 {s "$schema" :as m1}]
;;   (if s
;;     (if-let [m2 ($schema->m2 s)]
;;       ((validate-m2 c2 m2) {} m1)
;;       [{} {:errors (str "could not resolve $schema: " s)}])
;;     [{} {:errors "no $schema given"}]))

;; this should work but doesn't
(defn validate
  ;; ([c2 m1]
  ;;  (validate-m1 c2 m1))
  ([c2 m2 c1 m1]
   (reformat ((validate-m2 (assoc c2 :m2? true) m2) c1 m1))))
