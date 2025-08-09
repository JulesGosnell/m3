
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
   #?(:cljs [goog.string :as gstring])
   #?(:cljs [goog.string.format])
   #?(:clj [cheshire.core :as cheshire]
      :cljs [cljs.core :as cljs])
   [cljc.java-time.local-date :refer [parse] :rename {parse local-date-parse}]
   [cljc.java-time.offset-date-time :refer [parse] :rename {parse offset-date-time-parse}]
   [cljc.java-time.offset-time :refer [parse] :rename {parse offset-time-parse}]
   [clojure.string :refer [starts-with? ends-with? replace] :rename {replace string-replace}]
   [#?(:clj clojure.tools.logging :cljs m3.log) :as log]
   [m3.util :refer [absent present? concatv into-set conj-set seq-contains?]]
   [m3.uri :refer [parse-uri inherit-uri uri-base]]
   [m3.ref :refer [meld resolve-uri try-path]]
   [m3.pattern :refer [email-pattern ipv4-pattern ipv6-pattern hostname-pattern json-pointer-pattern relative-pointer-pattern uri-pattern uri-reference-pattern uri-template-pattern idn-email-pattern iri-pattern iri-reference-pattern uuid-pattern json-duration-pattern time-pattern ip-address-pattern color-pattern]]
   [m3.idn-hostname :refer [json-idn-hostname?]])
  #?(:clj
     (:import
      [org.graalvm.polyglot Context Value])))

;; consider https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html - other time types...

#?(:cljs
   (def Exception js/Error))

#?(:cljs
   (def Big (js/require "big.js")))

#?(:cljs
   (defn bigdec [x]
     (Big. (str x)))) ;; Big constructor takes a string or number

(def big-mod
  #?(:cljs (fn [^js/Big l ^js/Big r] (.mod l r))
     :clj mod))

(def big-zero?
  #?(:cljs (fn [^js/Big b] (.eq b 0))
     :clj zero?))

#?(:cljs (def fs (js/require "fs")))

#?(:cljs (defn slurp [path] (.readFileSync fs path "utf8")))

#?(:cljs (defn format [fmt & args] (apply gstring/format fmt args)))

;;------------------------------------------------------------------------------

#?(:cljs
   (defn deep-js->clj [x]
     (cond
       (.isArray js/Array x) (into [] (map deep-js->clj) x)
       (= (goog.typeOf x) "object") (into {} (map #(vector (aget % 0) (deep-js->clj (aget % 1))))
                                          (.entries js/Object x))
       :else x)))

(defn json-decode [s]
  #?(:clj (cheshire/decode s)
     :cljs (deep-js->clj (js/JSON.parse s))))

;;------------------------------------------------------------------------------

(def draft->draft?
  {:draft3       #{:draft3}
   :draft4       #{:draft3 :draft4}
   :draft6       #{:draft3 :draft4 :draft6}
   :draft7       #{:draft3 :draft4 :draft6 :draft7}
   :draft2019-09 #{:draft3 :draft4 :draft6 :draft7 :draft2019-09}
   :draft2020-12 #{:draft3 :draft4 :draft6 :draft7 :draft2019-09 :draft2020-12}
   :latest       #{:draft3 :draft4 :draft6 :draft7 :draft2019-09 :draft2020-12}
   :draft-next   #{:draft3 :draft4 :draft6 :draft7 :draft2019-09 :draft2020-12 :draft-next}})

(def draft->$schema
  {:draft3       "http://json-schema.org/draft-03/schema"
   :draft4       "http://json-schema.org/draft-04/schema"
   :draft6       "http://json-schema.org/draft-06/schema"
   :draft7       "http://json-schema.org/draft-07/schema"
   :draft2019-09 "https://json-schema.org/draft/2019-09/schema"
   :draft2020-12 "https://json-schema.org/draft/2020-12/schema"
   :latest       "https://json-schema.org/draft/2020-12/schema"
   :draft-next   "https://json-schema.org/draft/next/schema"})

(def $schema->draft
  (reduce-kv (fn [acc k v] (conj (conj acc [v k]) [(str v "#") k])) {} (dissoc draft->$schema :latest)))

(def $schema-uri->draft
  (reduce-kv (fn [acc k v] (conj acc [(parse-uri k) v])) {} $schema->draft))

(def latest-$schema (draft->$schema :latest))

;;------------------------------------------------------------------------------

;; use Graal/JavaScript to acquire an ECMA-262 compliant RegeExp engine
;; now we can use the same code at both back and front end...

#?(:clj (defonce ^Context js-context (Context/create (into-array ["js"]))))
#?(:clj (defonce ^Value RegExp (.getMember (.getBindings js-context "js") "RegExp")))

#?(:clj (defn ecma-pattern [^String s] (.newInstance RegExp (into-array Object [s "u"])))
   :cljs (defn ecma-pattern [s] (js/RegExp. s "u")))

#?(:clj (defn ecma-match [^Value r ^String s] (.asBoolean (.invokeMember r "test" (into-array Object [s]))))
   :cljs (defn ecma-match [r s] (.test r s)))

;;------------------------------------------------------------------------------
;; utils

(defn json-integer? [i]
  (or
   (integer? i)
   (and (number? i)
        (zero? (mod i 1)))))

(defn json-number? [n]
  (number? n))

(defn json-string? [v]
  (string? v))

(defn json-array? [v]
  (vector? v))

(defn json-object? [v]
  (map? v))

(defn make-type-checker [predicate? m1-function]
  (fn [c1 p1 m1]
    (if (predicate? m1)
      (m1-function c1 p1 m1)
      [c1 []])))

(defn make-error [message schema-path schema document-path document]
  {:schema-path schema-path :message (str message " - " (pr-str document)) :document-path document-path :document document :schema schema})

(defn make-error-on [message schema-path schema document-path document failed? errors]
  (when (failed? errors)
    [(assoc (make-error message schema-path schema document-path document) :errors errors)]))

(defn make-error-on-failure [message schema-path schema document-path document errors]
  (make-error-on message schema-path schema document-path document seq errors))

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

(defn json-= [l r]
  (cond
    (and (json-number? l) (json-number? r)) (zero? (- l r))
    ;; TODO: could be more efficient...
    (and (json-array? l) (json-array? r) (= (count l) (count r))) (every? (partial apply json-=) (map vector l r))
    ;; TODO: json-objects
    :else
    (= l r)))

;;------------------------------------------------------------------------------

(defmulti check-format (fn [format _c2 _p2 _m2] format))

(defn match [format pattern _c2 p2 m2 p1 m1]
  (when-not (re-find pattern m1)
    [(make-error (str "format: not a valid " format) p2 m2 p1 m1)]))

(defn check-pattern [pattern format c2 p2 m2]
  (fn [_c1 p1 m1]
    (match format pattern c2 p2 m2 p1 m1)))

(defn check-parse [p f _c2 p2 m2]
  (fn [_c1 p1 m1]
    (try
      (p m1)
      nil
      (catch Exception e
        [(make-error (str "format: not a valid " f ": " (ex-message e)) p2 m2 p1 m1)]))))

;; standard formats

(defmethod check-format "host-name" [f c2 p2 m2]
  (check-pattern hostname-pattern f c2 p2 m2))

(defmethod check-format "ip-address" [f c2 p2 m2]
  (check-pattern ip-address-pattern f c2 p2 m2))

(defmethod check-format "color" [f c2 p2 m2]
  ;; TODO:
  ;; need to or this with a complete set of CSS2/CSS3 color names
  (check-pattern color-pattern f c2 p2 m2))

(defmethod check-format "email" [f c2 p2 m2]
  (check-pattern email-pattern f c2 p2 m2))

(defmethod check-format "ipv4" [f c2 p2 m2]
  (check-pattern ipv4-pattern f c2 p2 m2))

(defmethod check-format "ipv6" [f c2 p2 m2]
  (check-pattern ipv6-pattern f c2 p2 m2))

(defmethod check-format "hostname" [f c2 p2 m2]
  (check-pattern hostname-pattern f c2 p2 m2))

(defmethod check-format "date-time" [f c2 p2 m2]
  (check-parse offset-date-time-parse f c2 p2 m2))

(defmethod check-format "date" [f c2 p2 m2]
  (check-parse local-date-parse f c2 p2 m2))

(defmethod check-format "time" [f {d :draft :as c2} p2 m2]
  (if (= :draft3 d)
    (check-pattern time-pattern f c2 p2 m2)
    (check-parse offset-time-parse f c2 p2 m2)))

(defmethod check-format "json-pointer" [f c2 p2 m2]
  (check-pattern json-pointer-pattern f c2 p2 m2))

(defmethod check-format "relative-json-pointer" [f c2 p2 m2]
  (check-pattern relative-pointer-pattern f c2 p2 m2))

(defmethod check-format "uri" [f c2 p2 m2]
  (check-pattern uri-pattern f c2 p2 m2))

(defmethod check-format "uri-reference" [f c2 p2 m2]
  (check-pattern uri-reference-pattern f c2 p2 m2))

(defmethod check-format "uri-template" [f c2 p2 m2]
  (check-pattern uri-template-pattern f c2 p2 m2))

(defmethod check-format "idn-email" [f c2 p2 m2]
  (check-pattern idn-email-pattern f c2 p2 m2))

(defmethod check-format "idn-hostname" [_format _c2 p2 m2]
  (fn [_c1 p1 m1]
    (when-not (json-idn-hostname? m1)
      [(make-error "format: not a valid idn-hostname:" p2 m2 p1 m1)])))

(defmethod check-format "iri" [f c2 p2 m2]
  (check-pattern iri-pattern f c2 p2 m2))

(defmethod check-format "iri-reference" [f c2 p2 m2]
  (check-pattern iri-reference-pattern f c2 p2 m2))

(defmethod check-format "uuid" [f c2 p2 m2]
  (check-pattern uuid-pattern f c2 p2 m2))

(defn json-duration? [s]
  (boolean
   (when-let [[_p-t-or-w _ymdthms-or-w ymd _y _m _d thms _h _m _s w] (re-find json-duration-pattern s)]
     (not
      (or
       (and (empty? ymd) (empty? thms) (empty? w))
       (= "T" thms))))))

(defmethod check-format "duration" [_format _c2 p2 m2]
  (fn [_c1 p1 m1]
    (when-not (json-duration? m1)
      [(make-error "format: not a valid duration:" p2 m2 p1 m1)])))

(defmethod check-format "regex" [f c2 p2 m2]
  (check-parse ecma-pattern f c2 p2 m2))

(defmethod check-format "unknown" [_format _c2 _p2 _m2]
  (constantly nil))

(defmethod check-format :default [f _c2 _p2 _m2]
  (fn [_c1 _p1 _m1]
    (log/warn "format: not recognised:" (pr-str f))
    nil))

;;------------------------------------------------------------------------------

(declare check-schema)

(defmulti check-type (fn [type _c2 _p2 _m2]
                       ;;(println "check-type" type document)
                       type))

(defn check-type-3 [p? t _c2 p2 m2]
  (fn [c1 p1 m1]
    [c1 (when-not (p? m1) [(make-error (str "type: not a[n] " t) p2 m2 p1 m1)])]))

(defmethod check-type "object" [t c2 p2 m2]
  (check-type-3 json-object? t c2 p2 m2))

(defmethod check-type "array" [t c2 p2 m2]
  (check-type-3 json-array? t c2 p2 m2))

(defmethod check-type "string" [t c2 p2 m2]
  (check-type-3 json-string? t c2 p2 m2))

(defmethod check-type "integer" [t {si? :strict-integer? :as c2} p2 m2]
  (check-type-3 (if si? integer? json-integer?) t c2 p2 m2))

(defmethod check-type "number" [t c2 p2 m2]
  (check-type-3 json-number? t c2 p2 m2))

(defmethod check-type "boolean" [t c2 p2 m2]
  (check-type-3 boolean? t c2 p2 m2))

(defmethod check-type "null" [t c2 p2 m2]
  (check-type-3 nil? t c2 p2 m2))

(defmethod check-type "any" [_t _c2 _p2 _m2]
  (fn [c1 _p1 _m1] [c1 nil]))

(defmethod check-type :default [ts c2 p2 m2]
  (if (json-array? ts) ;; it could be an array of elements that are:
    ;; - either a string type
    ;; - a schema
    (let [checkers
          (vec
           (map-indexed
            (fn [i t]
              (cond
                (json-string? t)
                (check-type t c2 (conj p2 i) t)
                (json-object? t)
                (check-schema c2 (conj p2 i) t)
                :else
                (throw (ex-info "hmmm" {:types ts :type t}))))
            ts))]

      (fn [c1 p1 m1]
        ;; TODO:
        ;; we should report all the errors...
        ;; then we cn implement disallow on to f this code
        ;; we should consider marking items as evaluated ?
        ;; we need to decide whether we are going to nest or flatten errors...
        ;; I guess some errors rely on their context to be errors ?
        [c1
         (make-error-on
          (format "type: none matched: %s" ts)
          p2 m2 p1 m1
          (fn [es] (not (some nil? es)))
          (mapv (fn [checker] (second (checker c1 p1 m1))) checkers))]))
    (fn [c1 p1 m1]
      [c1 [(make-error (format "type: unrecognised: %s" ts) p2 m2 p1 m1)]])))

;;------------------------------------------------------------------------------

;; standard common properties

(defn check-property-extends [property c2 p2 _m2 v2]
  (check-schema c2 (conj p2 property) v2))

(defn check-property-disallow [_property c2 p2 m2 v2]
  (let [ct (check-type v2 c2 p2 m2)]
    (fn [c1 p1 m1]
      (let [[c1 es] (ct c1 p1 m1)]
        [c1
         (when (nil? es) [(make-error "disallow: type matched" p2 m2 p1 m1)])]))))

(defn check-property-type [_property c2 p2 m2 v2]
  (check-type v2 c2 p2 m2))

(defn check-property-const [_property _c2 p2 m2 v2]
  (fn [c1 p1 m1]
    [c1
     (when (not (json-= v2 m1))
       [(make-error (format "const: document does not contain schema value: %s != %s" m1 v2) p2 m2 p1 m1)])]))

(defn check-property-enum [_property _c2 p2 m2 v2]
  (fn [c1 p1 m1]
     ;; we could check that the m2's enum contained any const or default
     ;; value here - but thus should be done somehow during validation of
     ;; the m2 as an m1 and not waste time whilst we are validating all
     ;; it's m1s...
     ;; TODO: how about some injectable consistency checking fns which can be used when validating m2s ?
    [c1
     (when-not (seq-contains? v2 json-= m1)
       [(make-error "enum: does not contain value" p2 m2 p1 m1)])]))

(defn check-property-id [_property _c2 _p2 _m2 _v2]
  (fn [{old-id-uri :id-uri :as c1} p1 {id "id"}]
    [(if-let [new-id-uri (and id (inherit-uri old-id-uri (parse-uri id)))]
       (do
         ;;(prn "ID:" old-id-uri "+" id "->" new-id-uri)
         (-> c1
             (update :path->uri assoc p1 new-id-uri)
             (update :uri->path assoc new-id-uri p1)
             (assoc :id-uri new-id-uri)))
       (-> c1
           (update :path->uri assoc p1 old-id-uri)))
     nil]))

(defn check-property-$id [_property _c2 _p2 _m2 _v2]
  (fn [{old-id-uri :id-uri :as c1} p1 {id "$id"}]
    [(if-let [new-id-uri (and id (inherit-uri old-id-uri (parse-uri id)))]
       (do
         ;;(prn "$ID:" old-id-uri "+" id "->" new-id-uri)
         (-> c1
             (update :path->uri assoc p1 new-id-uri)
             (update :uri->path assoc new-id-uri p1)
             (assoc :id-uri new-id-uri)))
       (-> c1
           (update :path->uri assoc p1 old-id-uri)))
     nil]))

;; Anchors only need :uri->path (no change, but included for completeness)
(defn check-property-$anchor [_property _c2 _p2 _m2 v2]
  (fn [c1 p1 _m1]
    (let [anchor-uri (inherit-uri (c1 :id-uri) (parse-uri (str "#" v2)))]
      [(update c1 :uri->path assoc anchor-uri p1) nil])))

(defn check-property-$recursiveAnchor [_property _c2 _p2 _m2 v2]
  (fn [c1 p1 _m1]
    (if (true? v2)
      (let [[uris top] (c1 :$recursive-anchor [#{} nil])]
        [(assoc c1 :$recursive-anchor [(conj uris (c1 :id-uri)) (or top p1)]) nil])
      [c1 nil])))

(defn check-property-$dynamicAnchor [_property _c2 _p2 _m2 v2]
  (fn [c1 p1 _m1]
    (let [anchor-uri (inherit-uri (c1 :id-uri) (parse-uri (str "#" v2)))]
      [(update c1 :$dynamic-anchor assoc anchor-uri p1) nil])))

(defn check-property-$comment [_property _c2 _p2 _m2 _v2]
  (fn [c1 _p1 _m1]
    ;;(log/info (str "$comment:" v2 " : " _p1))
    [c1 nil]))

;; hopefully during the f1 of an m2 we can precompile the $ref...
;; will never be called because a $ref in the m2 is intercepted and expanded...
;; TODO: think again...
(defn check-property-$ref [_property _c2 _p2 _m2 _v2]
  (fn [c1 _p1 _m1]
    ;;(prn "$REF:")
    [c1 nil]))

(defn check-property-$recursiveRef    [_property _c2 _p2 _m2 _v2] (fn [c1 _p1 _m1] [c1 nil]))
(defn check-property-$dynamicRef      [_property _c2 _p2 _m2 _v2] (fn [c1 _p1 _m1] [c1 nil]))
(defn check-property-description      [_property _c2 _p2 _m2 _v2] (fn [c1 _p1 _m1] [c1 nil]))
(defn check-property-readOnly         [_property _c2 _p2 _m2 _v2] (fn [c1 _p1 _m1] [c1 nil]))
(defn check-property-writeOnly        [_property _c2 _p2 _m2 _v2] (fn [c1 _p1 _m1] [c1 nil]))
(defn check-property-title            [_property _c2 _p2 _m2 _v2] (fn [c1 _p1 _m1] [c1 nil]))
(defn check-property-default          [_property _c2 _p2 _m2 _v2] (fn [c1 _p1 _m1] [c1 nil]))
(defn check-property-$schema          [_property _c2 _p2 _m2 _v2] (fn [c1 _p1 _m1] [c1 nil]))
(defn check-property-examples         [_property _c2 _p2 _m2 _v2] (fn [c1 _p1 _m1] [c1 nil]))

(declare draft->vocab-and-group-and-property-and-semantics)

(defn make-vocabularies [d v->b]
  (reduce
   (fn [acc [v g p f]]
     (let [b (v->b v)]
       (if (nil? b)
         acc
         (conj acc [p f]))))
   []
   ;; N.B. important to preserve evaluation order of vocabulary
   ;; properties...
   (draft->vocab-and-group-and-property-and-semantics d)))

(defn check-property-$vocabulary [_property {d :draft} _p2 _m2 v2]
  (let [vocabularies (make-vocabularies d v2)]
    (fn [c1 _p1 _m1]
      [(assoc c1 :vocabularies vocabularies) nil])))

;; TODO: issue a warning somehow
(defn check-property-deprecated [_property _c2 _p2 _m2 _v2] (fn [_c1 _p1 _m1])) ;; TODO: issue a warning or error ?

;; standard number properties

(defn check-property-minimum-old [_property _c2 p2 m2 v2]
  (let [e? (m2 "exclusiveMinimum")
        p? (if e? < <=)]
    (make-type-checker
     json-number?
     (fn [c1 p1 m1]
       [c1
        (when-not (p? v2 m1)
          [(make-error (str "minimum" (when e? "(with exclusiveMinimum)") ": value to low") p2 m2 p1 m1)])]))))

(defn check-property-minimum-new [_property _c2 p2 m2 v2]
  (make-type-checker
   json-number?
   (fn [c1 p1 m1]
     [c1
      (when-not (<= v2 m1)
        [(make-error "minimum: value to low" p2 m2 p1 m1)])])))

(defn check-property-exclusiveMinimum-old [_property _c2 _p2 {m "minimum"} _v2]
  (fn [c1 _p1 _m1]
    (when-not m (log/warn "exclusiveMinimum: no minimum present to modify"))
    [c1 []]))

(defn check-property-exclusiveMinimum-new [_property _c2 p2 m2 v2]
  (make-type-checker
   json-number?
   (fn [c1 p1 m1]
     [c1
      (when-not (< v2 m1)
        [(make-error "minimum: value to low" p2 m2 p1 m1)])])))

(defn check-property-maximum-old [_property _c2 p2 m2 v2]
  (let [e? (m2 "exclusiveMaximum")
        p? (if e? > >=)]
    (make-type-checker
     json-number?
     (fn [c1 p1 m1]
       [c1
        (when-not (p? v2 m1)
          [(make-error (str "maximum" (when e? "(with exclusiveMaximum)") ": value too high") p2 m2 p1 m1)])]))))

(defn check-property-maximum-new [_property _c2 p2 m2 v2]
  (make-type-checker
   json-number?
   (fn [c1 p1 m1]
     [c1
      (when-not (>= v2 m1)
        [(make-error "maximum: value too high" p2 m2 p1 m1)])])))

(defn check-property-exclusiveMaximum-old [_property _c2 _p2 {m "maximum"} _v2]
  (fn [c1 _p1 _m1]
    (when-not m (log/warn "exclusiveMaximum: no maximum present to modify"))
    [c1 []]))

(defn check-property-exclusiveMaximum-new [_property _c2 p2 m2 v2]
  (make-type-checker
   json-number?
   (fn [c1 p1 m1]
     [c1
      (when-not (> v2 m1)
        [(make-error "maximum: value too high" p2 m2 p1 m1)])])))

(defn check-property-divisibleBy [_property _c2 p2 m2 v2]
  (let [v2-bd (bigdec v2)]
    (make-type-checker
     json-number?
     (fn [c1 p1 m1]
       [c1
        (when (not (big-zero? (big-mod (bigdec m1) v2-bd)))
          [(make-error (format "%s is not divisible by of %s" m1 v2) p2 m2 p1 m1)])]))))

(defn check-property-multipleOf [_property _c2 p2 m2 v2]
  (let [v2-bd (bigdec v2)]
    (make-type-checker
     json-number?
     (fn [c1 p1 m1]
       [c1
        (when (not (big-zero? (big-mod (bigdec m1) v2-bd)))
          [(make-error (format "%s is not a multiple of %s" m1 v2) p2 m2 p1 m1)])]))))

;; standard string properties

(defn char-code-at [str pos]
  #?(:clj (.charAt str pos)
     :cljs (.charCodeAt str pos)))

;; we don't need to get to the end of the string to know it is too big...
(defn json-length
  ;; adapted from: https://lambdaisland.com/blog/2017-06-12-clojure-gotchas-surrogate-pairs
  "string length - but handles emojis - counts their two chars as a single grapheme"
  ([s]
   (json-length s 0 0))
  ([s i acc]
   (if (>= i (count s))
     acc
     (json-length
      s
      (if (<= 0xD800 (int (char-code-at s i)) 0xDBFF) (+ i 2) (inc i))
      (inc acc)))))

(defn check-property-minLength [_property _c2 p2 m2 v2]
  (let [ml2 (quot v2 2)]
    (make-type-checker
     json-string?
     (fn [c1 p1 m1]
       [c1
        (when (or
               (< (count m1) ml2) ;; precheck before using expensive json-length
               (< (json-length m1) v2))
          [(make-error "minLength: string too short" p2 m2 p1 m1)])]))))

(defn check-property-maxLength [_property _c2 p2 m2 v2]
  (let [ml2 (* v2 2)]
    (make-type-checker
     json-string?
     (fn [c1 p1 m1]
       [c1
        (when (or
               (> (count m1) ml2) ;; precheck before using expensive json-length
               (> (json-length m1) v2))
          [(make-error "maxLength: string too long" p2 m2 p1 m1)])]))))

(defn make-check-property-format [strict?]
  (fn [_property {cfs :check-format :or {cfs {}} strict-format? :strict-format? :as c2} p2 m2 v2]
    (let [f (if (or strict? strict-format?)
              (fn [f2] (make-type-checker json-string? (fn [c p m] [c (f2 c p m)])))
              (fn [f2] (make-type-checker json-string? (fn [c p m] (when-let [[{m :message}] (f2 c p m)] [c (log/warn m)])))))]
    ;; we do this here so that user may override default format checkers...
      (f ((or (cfs v2) check-format) v2 c2 p2 m2)))))

(defn check-property-pattern [_property c2 p2 m2 v2]
  (if (starts-with? v2 "$format:")
    ;; N.B.
    ;; this is an extension to allow patternProperties to
    ;; leverage formats since the spec does not provide a
    ;; formatProperties...
    ((make-check-property-format false) "format" c2 p2 m2 (subs v2 (count "$format:"))) ;; TODO: decide strictness from context somehow
    (let [p (ecma-pattern v2)]
      (make-type-checker
       json-string?
       (fn [c1 p1 m1]
         [c1
          (when (false? (ecma-match p m1))
            [(make-error "pattern: doesn't match" p2 m2 p1 m1)])])))))

#?(:clj
   (let [^java.util.Base64 decoder (java.util.Base64/getDecoder)]
     (defn base64-decode [^String s] (String. (.decode decoder (.getBytes s "UTF-8")) "UTF-8")))

   :cljs
   (defn base64-decode [s] (.toString (js/Buffer.from s "base64") "utf8")))

(def ce->decoder
  {absent identity
   "quoted-printable" identity
   "base16" (fn [_] (throw (ex-info "base16/decode: NYI" {})))
   "base32" (fn [_] (throw (ex-info "base32/decode: NYI" {})))
   "base64" base64-decode})

(def cmt->decoder
  {"application/json" json-decode})

(defn make-check-property-contentEncoding [strict?]
  (fn [_property _c2 p2 m2 v2]
    (let [ce-decoder (ce->decoder v2)
          pp2 (butlast p2)]
      (make-type-checker
       json-string?
       (fn [c1 p1 old-m1]
         (let [[new-m1 es]
               (try
                 [(ce-decoder old-m1) nil]
                 (catch Exception e
                   [nil
                    (let [m (str "contentEncoding: could not " v2 " decode: " (pr-str old-m1) " - " (ex-message e))]
                      (if strict?
                        [(make-error m p2 m2 p1 old-m1)]
                        (do
                          (log/warn (string-replace m #"\n" " - "))
                          [])))]))]
           (if new-m1
             [(update c1 :content assoc pp2 new-m1) nil]
             [c1 es])))))))

(defn make-check-property-contentMediaType [strict?]
  (fn [_property _c2 p2 m2 v2]
    (let [cmt v2
          cmt-decoder (cmt->decoder cmt)
          pp2 (butlast p2)]
      (make-type-checker
       json-string?
       (fn [c1 p1 m1]
         (let [old-m1 (or (get (get c1 :content) pp2) m1)
               [new-m1 es]
               (try
                 [(cmt-decoder old-m1) nil]
                 (catch Exception e
                   [nil
                    (let [m (str "contentMediaType: could not " v2 " decode: " (pr-str old-m1) " - " (string-replace (ex-message e) #"\n" " \\\\n "))]
                      (if strict?
                        [(make-error m p2 m2 p1 old-m1)]
                        (do
                          (log/warn (string-replace m #"\n" " - "))
                          [])))]))]

           (if new-m1
             [(update c1 :content assoc pp2 new-m1) nil]
             [c1 es])))))))

(defn make-check-property-contentSchema [strict?]
  (fn [_property c2 p2 {cmt "contentMediaType"} v2]
    (let [checker (check-schema c2 p2 v2)
          pp2 (butlast p2)]
      (fn [c1 p1 m1]
        (let [old-m1 (or (get (get c1 :content) pp2) m1)
              old-m1 (if cmt old-m1 (json-decode old-m1))] ;; TODO: error handling
          [c1
           (try
             (let [{es :errors :as v} (checker c1 p1 old-m1)]
               (when (seq es)
                 (if strict?
                   es
                   (log/warn "contentSchema: failed validation - " (prn-str v)))))
             (catch Exception e
               (:errors (ex-data e))))])))))

(defn check-property-dependencies [_property c2 p2 m2 v2]
  (let [property->checker
        (reduce
         (fn [acc [k v]]
           (assoc
            acc
            k
            (cond
              (json-string? v) ;; a single property dependency
              (fn [c1 _p1 m1] [c1 (when (not (contains? m1 v)) [v v])])
              (json-array? v) ;; a multiple property dependency
              ;; TODO: this looks very suspect
              (fn [c1 _p1 m1] [c1 (reduce (fn [acc2 k2] (if (contains? m1 k2) acc2 (conj acc2 [k k2]))) [] v)])
              (or (json-object? v) (boolean? v)) ;; a schema dependency
              (check-schema c2 p2 v)
              ;; we should not need to check other cases as m2 should have been validated against m3
              )))
         {}
         v2)]
    (make-type-checker
     json-object?
     (fn [c1 p1 m1]
       (let [[c1 es]
             (reduce
              (fn [[c old-es] [k _v]]
                (if (contains? m1 k)
                  (let [[c new-es] ((property->checker k) c p1 m1)]
                    [c (concatv old-es new-es)])
                  [c old-es]))
              [c1 []]
              v2)]
         [c1
          (when-let [missing (seq es)]
            [(make-error ["dependencies: missing properties (at least):" missing] p2 m2 p1 m1)])])))))

(defn check-property-dependentSchemas [_property c2 p2 m2 v2]
  (let [property->checker
        (reduce
         (fn [acc [k v]]
           (assoc acc k (check-schema c2 p2 v)))
         {}
         v2)]
    (make-type-checker
     json-object?
     (fn [c1 p1 m1]
       (let [[c1 es]
             (reduce
              (fn [[c old-es] [k _v]]
                (if (contains? m1 k)
                  (let [[c new-es] ((property->checker k) c p1 m1)]
                    [c (concatv old-es new-es)])
                  [c old-es]))
              [c1 []]
              v2)]
         [c1
          (when-let [missing (seq es)]
            [(make-error ["dependentSchemas: missing properties (at least):" missing] p2 m2 p1 m1)])])))))

(defn check-property-propertyDependencies [_property c2 p2 _m2 v2]
  (let [checkers (into {} (mapcat (fn [[k1 vs]] (map (fn [[k2 s]] [[k1 k2] (check-schema c2 p2 s)]) vs)) v2))
        ks (keys v2)]
    (make-type-checker
     json-object?
     (fn [c1 p1 m1]
       (reduce
        (fn [[c old-es] k]
          (let [v (m1 k)]
            (if-let [checker (and (json-string? v) (checkers [k v]))]
              (let [[c new-es] (checker c p1 m1)]
                [c (concatv old-es new-es)])
              [c old-es])))
        [c1 []]
        ks)))))

;; TODO: share more code with dependencies
(defn check-property-dependentRequired [_property _c2 p2 m2 v2]
  (let [property->checker
        (reduce
         (fn [acc [k v]]
           (assoc
            acc
            k
            (fn [_c1 _p1 m1] (reduce (fn [acc2 k2] (if (contains? m1 k2) acc2 (conj acc2 [k k2]))) [] v))))
         {}
         v2)]
    (make-type-checker
     json-object?
     (fn [c1 p1 m1]
       [c1
        (when-let [missing
                   (seq
                    (reduce
                     (fn [acc [k _v]]
                       (if (contains? m1 k)
                         (concatv acc ((property->checker k) c1 p1 m1))
                         acc))
                     []
                     v2))]
          [(make-error ["dependentRequired: missing properties (at least):" missing] p2 m2 p1 m1)])]))))

;;------------------------------------------------------------------------------

(defn check-property-if [_property c2 p2 _m2 v2]
  (let [checker (check-schema c2 p2 v2)
        pp2 (butlast p2)]
    (fn [old-c1 p1 m1]
      (let [[new-c1 es] (checker old-c1 p1 m1)
            success? (empty? es)]
        [(update (if success? new-c1 old-c1) :if assoc pp2 success?) []]))))

(defn check-property-then [_property c2 p2 _m2 v2]
  (let [checker (check-schema c2 p2 v2)
        pp2 (butlast p2)]
    (fn [c1 p1 m1]
      (if (true? (get (get c1 :if) pp2))
        (checker c1 p1 m1)
        [c1 []]))))

(defn check-property-else [_property c2 p2 _m2 v2]
  (let [checker (check-schema c2 p2 v2)
        pp2 (butlast p2)]
    (fn [c1 p1 m1]
      (if (false? (get (get c1 :if) pp2))
        (checker c1 p1 m1)
        [c1 []]))))

(defn check-property-definitions [_property c2 p2 _m2 v2]
  (mapv (fn [[k v]] (check-schema c2 (conj p2 k) v)) v2)
  (fn [c1 _p1 _m1] [c1 nil]))

(defn check-property-$defs [_property c2 p2 _m2 v2]
  (mapv (fn [[k v]] (check-schema c2 (conj p2 k) v)) v2)
  (fn [c1 _p1 _m1] [c1 nil]))

(defn check-properties [_c2 p2 m2]
  (let [pp2 (butlast p2)]
    (fn [c1 p1 m1 k-and-css message]
      (let [[c1 es]
            (reduce
             (fn [[c old-es] [[k cs] sub-document]]
               (let [[c new-es] (cs c (conj p1 k) sub-document)]
                 [c (concatv old-es new-es)]))
             [c1 []]
             (map (fn [[k :as k-and-cs]] [k-and-cs (m1 k)]) k-and-css))]
        [(let [ks (map first k-and-css)]
           (-> c1
           ;; TODO: only record matched if additonalProperties needed later ?
               (update :matched update pp2 into-set ks)
           ;; TODO: only record evaluated if unevaluatedProperties needed later ?
               (update :evaluated update p1 into-set ks)))
         (make-error-on-failure message p2 m2 p1 m1 es)]))))

(defn check-property-properties [_property c2 p2 m2 ps]
  (let [k-and-css (mapv (fn [[k v]] [k (check-schema c2 (conj p2 k) v)]) ps)
        cp (check-properties c2 p2 m2)]
    (make-type-checker
     json-object?
     (fn [c1 p1 m1]
       (let [k-and-css (filter (fn [[k]] (contains? m1 k)) k-and-css)]
         (cp c1 p1 m1 k-and-css "properties: at least one property did not conform to respective schema"))))))

;; what is opposite of "additional" - "matched" - used by spec to refer to properties matched by "properties" or "patternProperties"

(defn check-property-patternProperties [_property c2 p2 m2 pps]
  (let [cp-and-pattern-and-ks (mapv (fn [[k v]] [(check-schema c2 (conj p2 k) v) (ecma-pattern k) k]) pps)
        cp (check-properties c2 p2 m2)]
    (make-type-checker
     json-object?
     (fn [c1 p1 m1]
       (let [k-and-css (apply concat (keep (fn [[k]] (keep (fn [[cs p]] (when (ecma-match p k) [k cs])) cp-and-pattern-and-ks)) m1))]
         (cp c1 p1 m1 k-and-css "patternProperties: at least one property did not conform to respective schema"))))))

(defn check-property-additionalProperties [_property c2 p2 m2 v2]
  (let [cs (check-schema c2 p2 v2)
        pp2 (butlast p2)
        cp (check-properties c2 p2 m2)]
    (make-type-checker
     json-object?
     (fn [c1 p1 m1]
       (let [mps (get (get c1 :matched) pp2 #{})
             aps (remove (fn [[k]] (contains? mps k)) m1) ;; k might be nil
             p-and-css (mapv (fn [[k]] [k cs]) aps)] ; TODO: feels inefficient
         (cp c1 p1 m1 p-and-css "additionalProperties: at least one property did not conform to schema"))))))

(defn check-property-unevaluatedProperties [_property c2 p2 m2 v2]
  (let [cs (check-schema c2 p2 v2)
        cp (check-properties c2 p2 m2)]
    (make-type-checker
     json-object?
     (fn [c1 p1 m1]
       (let [eps (get (get c1 :evaluated) p1 #{})
             ups (remove (fn [[k]] (contains? eps k)) m1) ;; k might be nil
             p-and-css (mapv (fn [[k]] [k cs]) ups)] ; TODO: feels inefficient
         (cp c1 p1 m1 p-and-css "unevaluatedProperties: at least one property did not conform to schema"))))))

(defn check-property-propertyNames [_property c2 p2 m2 v2]
  (fn [c1 p1 m1]
    [c1
     (when (json-object? m1)
       (make-error-on-failure
        "propertyNames: at least one property's name failed to conform to relevant schema"
        p2 m2 p1 m1
        (reduce
         (fn [acc [k]]
           (let [[_new-c1 es] ((check-schema c2 (conj p2 k) v2) c1 (conj p1 k) k)]
             (concatv acc es)))
         []
         m1)))]))

(defn check-property-required [_property _c2 p2 m2 v2]
  (make-type-checker
   json-object?
   (fn [c1 p1 m1]
     [c1
      (when-let [missing (seq (reduce (fn [acc k] (if (contains? m1 k) acc (conj acc k))) [] v2))]
        [(make-error ["required: missing properties (at least):" missing] p2 m2 p1 m1)])])))

(defn check-property-minProperties [_property _c2 p2 m2 v2]
  (make-type-checker
   json-object?
   (fn [c1 p1 m1]
     [c1
      (when (< (count m1) v2)
        [(make-error "minProperties: document contains too few properties" p2 m2 p1 m1)])])))

(defn check-property-maxProperties [_property _c2 p2 m2 v2]
  (make-type-checker
   json-object?
   (fn [c1 p1 m1]
     [c1
      (when (> (count m1) v2)
        [(make-error "maxProperties: document has too many properties" p2 m2 p1 m1)])])))

;; standard array properties

;; we could save time by only maintaining :matched and :evaluated
;; context if required (additional and evaluated items)...
(defn check-items [_c2 p2 m2]
  (let [pp2 (butlast p2)]
    (fn [c1 p1 m1 i-and-css message]
      (let [old-local-c1
            (-> c1
                (update :matched assoc pp2 #{})
                (update :evaluated assoc p1 #{}))
            [c1 es]
            (reduce
             (fn [[old-c old-es] [[i cs] sub-document]]
               (let [[_ new-es] (cs old-local-c1 (conj p1 i) sub-document)
                     new-c (if (empty? new-es)
                             (-> old-c
                                 (update :matched update pp2 conj-set i)
                                 (update :evaluated update p1 conj-set i))
                             old-c)]
                 [new-c (concatv old-es new-es)]))
             [c1 []]
             (map vector i-and-css m1))]
        [c1 (make-error-on-failure message p2 m2 p1 m1 es)]))))

(defn check-property-prefixItems [_property c2 p2 m2 v2]
  (let [i-and-css (vec (map-indexed (fn [i sub-schema] [i (check-schema c2 (conj p2 i) sub-schema)]) v2))
        ci (check-items c2 p2 m2)]
    (make-type-checker
     json-array?
     (fn [c1 p1 m1]
       (ci c1 p1 m1 i-and-css "prefixItems: at least one item did not conform to respective schema")))))

(defn check-property-items [_property {d :draft :as c2} p2 m2 v2]
  (let [n (count (m2 "prefixItems")) ;; TODO: achieve this by looking at c1 ?
        [m css] (if (json-array? v2)
                  (do
                    (case d
                      (:draft3 :draft4 :draft6 :draft7 :draft2019-09)
                      nil
                      (:draft2020-12 :draft-next)
                      (log/info (str "prefixItems: was introduced in draft2020-12 to handle tuple version of items - you are using: " d)))
                    ["respective " (map-indexed (fn [i v] (check-schema c2 (conj p2 i) v)) v2)])
                  ["" (repeat (check-schema c2 p2 v2))])
        ci (check-items c2 p2 m2)]
    (make-type-checker
     json-array?
     (fn [c1 p1 m1]
       (let [items (drop n m1)
             i-and-css (mapv (fn [i cs _] [(+ i n) cs]) (range) css items)]
         (ci c1 p1 items i-and-css (str "items: at least one item did not conform to " m "schema")))))))

(defn check-property-additionalItems [_property c2 p2 {is "items" :as m2} v2]
  (if (json-array? is) ;; additionalItems is only used when items is a tuple
    (let [cs (check-schema c2 p2 v2)
          ;;pp2 (butlast p2)
          ci (check-items c2 p2 m2)]
      (make-type-checker
       json-array?
       (fn [c1 p1 m1]
         (let [;; this is how it should be done, but cheaper to just look at items (must be array for additionalItems to be meaningful) in m2 time
                 ;;mis (get (get c1 :matched) pp2 #{})
                 ;;ais  (remove (fn [[k]] (contains? mis k)) (map-indexed vector m1))
                 ;;i-and-css (mapv (fn [[k]] [k cs]) ais) ; TODO: feels inefficient
               n (count is)
               ais (drop n m1)
               i-and-css (mapv (fn [i cs _] [(+ i n) cs]) (range) (repeat cs) ais)]
           (ci c1 p1 ais i-and-css "additionalItems: at least one item did not conform to schema")))))
    (fn [c1 _p1 _m1]
      [c1 nil])))

(defn check-property-unevaluatedItems [_property c2 p2 m2 v2]
  (let [css (repeat (check-schema c2 p2 v2))
        ci (check-items c2 p2 m2)]
    (make-type-checker
     json-array?
     (fn [{p->eis :evaluated :as c1} p1 m1]
       (let [eis (or (get p->eis p1) #{})
             index-and-items (filter (fn [[k]] (not (eis k))) (map-indexed (fn [i v] [i v]) m1))
             i-and-css (mapv (fn [cs [i]] [i cs]) css index-and-items)] ;; TODO: item not used
         (ci c1 p1 (map second index-and-items) i-and-css "unevaluatedItems: at least one item did not conform to schema"))))))

(defn check-property-contains [_property c2 p2 {mn "minContains" :as m2} v2]
  (let [cs (check-schema c2 p2 v2)
        base (if mn mn 1)
        ci (check-items c2 p2 v2)]
    (make-type-checker
     json-array?
     (fn [c1 p1 m1]
       (let [i-and-css (map (fn [i _] [i cs]) (range) m1)
             [new-c1 [{es :errors}]]
             (ci c1 p1 m1 i-and-css "contains: at least one item did not conform to schema")
             matches (- (count m1) (count es))]
         (if (<= (min base 1) matches)
           [new-c1 nil]
           [c1 [(make-error "contains: document has no matches" p2 m2 p1 m1)]]))))))

(defn check-property-minContains [_property _c2 p2 m2 v2]
  (let [pp2 (butlast p2)]
    (make-type-checker
     json-array?
     (fn [{matched :matched :as c1} p1 m1]
       (if-let [matches (get matched pp2)]
         (let [n (count matches)]
           (if (and
                matches
                (<= v2 n))
             [c1 nil]
             [c1 [(make-error (str "minContains: document has too few matches - " n) p2 m2 p1 m1)]]))
         [c1 nil])))))

(defn check-property-maxContains [_property _c2 p2 m2 v2]
  (let [pp2 (butlast p2)]
    (make-type-checker
     json-array?
     (fn [{matched :matched :as c1} p1 m1]
       (if-let [matches (get matched pp2)]
         (let [n (count matches)]
           (if (<= n v2)
             [c1 nil]
             [c1 [(make-error (str "maxContains: document has too many matches - " n) p2 m2 p1 m1)]]))
         [c1 nil])))))

(defn check-property-minItems [_property _c2 p2 m2 v2]
  (make-type-checker
   json-array?
   (fn [c1 p1 m1]
     [c1
      (when (< (count m1) v2)
        [(make-error "minItems: document contains too few items" p2 m2 p1 m1)])])))

(defn check-property-maxItems [_property _c2 p2 m2 v2]
  (make-type-checker
   json-array?
   (fn [c1 p1 m1]
     [c1
      (when (> (count m1) v2)
        [(make-error "maxItems: document contains too many items" p2 m2 p1 m1)])])))

;; TODO: should be unique according to json equality
;; TODO: could be more efficient - only needs to find one duplicate before it bails..
;; TODO: use sorted-set-by and put items in 1 at a time until one is rejected then bail - reduced
(defn check-property-uniqueItems [_property _c2 p2 m2 v2]
  (if v2
    (make-type-checker
     json-array?
     (fn [c1 p1 m1]
       [c1
        (when (not (= (count m1) (count (distinct m1))))
          [(make-error "uniqueItems: document contains duplicate items" p2 m2 p1 m1)])]))
    (fn [c1 _p1 _m1]
      [c1 nil])))

;; TODO: merge code with check-items...
(defn check-of [c2 p2 m2 v2]
  (let [i-and-css (vec (map-indexed (fn [i sub-schema] [i (check-schema c2 (conj p2 i) sub-schema)]) v2))]
    (fn [c1 p1 m1 message failed?]
      (let [old-local-c1 (update c1 :evaluated dissoc p1)
            [c1 es]
            (reduce
             (fn [[old-c old-es] [_i cs]]
               (let [[new-local-c1 new-es] (cs old-local-c1 p1 m1)
                     new-c (if (empty? new-es) (update old-c :evaluated update p1 into-set (get (get new-local-c1 :evaluated) p1)) old-c)
                     es (concatv old-es new-es)]
                 [new-c es]))
             [c1 []]
             i-and-css)]
        [c1
         (make-error-on message p2 m2 p1 m1 failed? es)]))))

(defn check-property-oneOf [_property c2 p2 m2 v2]
  (let [co (check-of c2 p2 m2 v2)
        m2-count (count v2)]
    (fn [c1 p1 m1]
      (co
       c1 p1 m1
       "oneOf: document failed to conform to one and only one sub-schema"
       (fn [es] (not= 1 (- m2-count (count es))))))))

(defn check-property-anyOf [_property c2 p2 m2 v2]
  (let [co (check-of c2 p2 m2 v2)
        m2-count (count v2)]
    (fn [c1 p1 m1]
      (co
       c1 p1 m1
       "anyOf: document failed to conform to at least one sub-schema"
       (fn [es] (not (< (count es) m2-count)))))))

(defn check-property-allOf [_property c2 p2 m2 v2]
  (let [co (check-of c2 p2 m2 v2)]
    (fn [c1 p1 m1]
      (co
       c1 p1 m1
       "allOf: document failed to conform to all sub-schemas"
       seq))))

;; TODO: share check-of
(defn check-property-not [_property c2 p2 m2 v2]
  (let [c (check-schema c2 p2 v2)]
    (fn [c1 p1 m1]
      (let [old-local-c1 (update c1 :evaluated dissoc p1)
            [new-local-c1 es] (c old-local-c1 p1 m1)
            [c1 failed?] (if (seq es)
                           [(update c1 :evaluated update p1 into-set (get (get new-local-c1 :evaluated) p1)) true]
                           [c1 false])]
        [c1
         (when-not failed?
           [(make-error "not: document conformed to sub-schema" p2 m2 p1 m1)])]))))

;;------------------------------------------------------------------------------

(def draft->vocab-and-group-and-property-and-semantics
  {:draft3
   [["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "extends"                 check-property-extends]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "disallow"                check-property-disallow]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "divisibleBy"             check-property-divisibleBy]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "type"                    check-property-type]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "const"                   check-property-const]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "minLength"               check-property-minLength]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "maxLength"               check-property-maxLength]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "multipleOf"              check-property-multipleOf]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "format"                  (make-check-property-format false)]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "enum"                    check-property-enum]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "pattern"                 check-property-pattern]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "$ref"                    check-property-$ref]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "$schema"                 check-property-$schema]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "$id"                     check-property-$id]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "id"                      check-property-id]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "$anchor"                 check-property-$anchor]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "$recursiveRef"           check-property-$recursiveRef]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "$recursiveAnchor"        check-property-$recursiveAnchor]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "$dynamicRef"             check-property-$dynamicRef]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "$dynamicAnchor"          check-property-$dynamicAnchor]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "$vocabulary"             check-property-$vocabulary]
    ["https://json-schema.org/draft-03/vocab/meta-data"               :meta-data              "title"                   check-property-title]
    ["https://json-schema.org/draft-03/vocab/meta-data"               :meta-data              "description"             check-property-description]
    ["https://json-schema.org/draft-03/vocab/meta-data"               :meta-data              "default"                 check-property-default]
    ["https://json-schema.org/draft-03/vocab/meta-data"               :meta-data              "examples"                check-property-examples]
    ["https://json-schema.org/draft-03/vocab/meta-data"               :meta-data              "deprecated"              check-property-deprecated]
    ["https://json-schema.org/draft-03/vocab/meta-data"               :meta-data              "readOnly"                check-property-readOnly]
    ["https://json-schema.org/draft-03/vocab/meta-data"               :meta-data              "writeOnly"               check-property-writeOnly]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "$comment"                check-property-$comment]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "definitions"             check-property-definitions]
    ["https://json-schema.org/draft-03/vocab/core"                    :core                   "$defs"                   check-property-$defs]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "propertyNames"           check-property-propertyNames]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "propertyDependencies"    check-property-propertyDependencies]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "minProperties"           check-property-minProperties]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "maxProperties"           check-property-maxProperties]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "not"                     check-property-not]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "anyOf"                   check-property-anyOf]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "oneOf"                   check-property-oneOf]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "allOf"                   check-property-allOf]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "required"                check-property-required]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "dependencies"            check-property-dependencies]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "dependentSchemas"        check-property-dependentSchemas]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "contains"                check-property-contains]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "minContains"             check-property-minContains]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "maxContains"             check-property-maxContains]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "minimum"                 check-property-minimum-old]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "exclusiveMinimum"        check-property-exclusiveMinimum-old]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "maximum"                 check-property-maximum-old]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "exclusiveMaximum"        check-property-exclusiveMaximum-old]
    ["https://json-schema.org/draft-03/vocab/content"                 :content                "contentEncoding"         (make-check-property-contentEncoding false)]
    ["https://json-schema.org/draft-03/vocab/content"                 :content                "contentMediaType"        (make-check-property-contentMediaType false)]
    ["https://json-schema.org/draft-03/vocab/content"                 :content                "contentSchema"           (make-check-property-contentSchema false)]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "if"                      check-property-if]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "then"                    check-property-then]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "else"                    check-property-else]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "minItems"                check-property-minItems]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "maxItems"                check-property-maxItems]
    ["https://json-schema.org/draft-03/vocab/validation"              :validation             "uniqueItems"             check-property-uniqueItems]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "items"                   check-property-items]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "additionalItems"         check-property-additionalItems]
    ["https://json-schema.org/draft-03/vocab/unevaluated"             :unevaluated            "unevaluatedItems"        check-property-unevaluatedItems]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "properties"              check-property-properties]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "patternProperties"       check-property-patternProperties]
    ["https://json-schema.org/draft-03/vocab/applicator"              :applicator             "additionalProperties"    check-property-additionalProperties]
    ["https://json-schema.org/draft-03/vocab/unevaluated"             :unevaluated            "unevaluatedProperties"   check-property-unevaluatedProperties]]
   :draft4
   [["https://json-schema.org/draft-04/vocab/validation"              :validation             "type"                    check-property-type]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "const"                   check-property-const]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "minLength"               check-property-minLength]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "maxLength"               check-property-maxLength]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "multipleOf"              check-property-multipleOf]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "format"                  (make-check-property-format true)]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "enum"                    check-property-enum]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "pattern"                 check-property-pattern]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "$ref"                    check-property-$ref]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "$schema"                 check-property-$schema]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "$id"                     check-property-$id]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "id"                      check-property-id]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "$anchor"                 check-property-$anchor]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "$recursiveRef"           check-property-$recursiveRef]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "$recursiveAnchor"        check-property-$recursiveAnchor]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "$dynamicRef"             check-property-$dynamicRef]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "$dynamicAnchor"          check-property-$dynamicAnchor]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "$vocabulary"             check-property-$vocabulary]
    ["https://json-schema.org/draft-04/vocab/meta-data"               :meta-data              "title"                   check-property-title]
    ["https://json-schema.org/draft-04/vocab/meta-data"               :meta-data              "description"             check-property-description]
    ["https://json-schema.org/draft-04/vocab/meta-data"               :meta-data              "default"                 check-property-default]
    ["https://json-schema.org/draft-04/vocab/meta-data"               :meta-data              "examples"                check-property-examples]
    ["https://json-schema.org/draft-04/vocab/meta-data"               :meta-data              "deprecated"              check-property-deprecated]
    ["https://json-schema.org/draft-04/vocab/meta-data"               :meta-data              "readOnly"                check-property-readOnly]
    ["https://json-schema.org/draft-04/vocab/meta-data"               :meta-data              "writeOnly"               check-property-writeOnly]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "$comment"                check-property-$comment]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "definitions"             check-property-definitions]
    ["https://json-schema.org/draft-04/vocab/core"                    :core                   "$defs"                   check-property-$defs]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "propertyNames"           check-property-propertyNames]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "propertyDependencies"    check-property-propertyDependencies]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "minProperties"           check-property-minProperties]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "maxProperties"           check-property-maxProperties]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "not"                     check-property-not]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "anyOf"                   check-property-anyOf]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "oneOf"                   check-property-oneOf]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "allOf"                   check-property-allOf]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "required"                check-property-required]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "dependencies"            check-property-dependencies]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "dependentSchemas"        check-property-dependentSchemas]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "contains"                check-property-contains]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "minContains"             check-property-minContains]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "maxContains"             check-property-maxContains]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "minimum"                 check-property-minimum-old]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "exclusiveMinimum"        check-property-exclusiveMinimum-old]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "maximum"                 check-property-maximum-old]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "exclusiveMaximum"        check-property-exclusiveMaximum-old]
    ["https://json-schema.org/draft-04/vocab/content"                 :content                "contentEncoding"         (make-check-property-contentEncoding false)]
    ["https://json-schema.org/draft-04/vocab/content"                 :content                "contentMediaType"        (make-check-property-contentMediaType false)]
    ["https://json-schema.org/draft-04/vocab/content"                 :content                "contentSchema"           (make-check-property-contentSchema false)]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "if"                      check-property-if]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "then"                    check-property-then]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "else"                    check-property-else]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "minItems"                check-property-minItems]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "maxItems"                check-property-maxItems]
    ["https://json-schema.org/draft-04/vocab/validation"              :validation             "uniqueItems"             check-property-uniqueItems]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "items"                   check-property-items]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "additionalItems"         check-property-additionalItems]
    ["https://json-schema.org/draft-04/vocab/unevaluated"             :unevaluated            "unevaluatedItems"        check-property-unevaluatedItems]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "properties"              check-property-properties]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "patternProperties"       check-property-patternProperties]
    ["https://json-schema.org/draft-04/vocab/applicator"              :applicator             "additionalProperties"    check-property-additionalProperties]
    ["https://json-schema.org/draft-04/vocab/unevaluated"             :unevaluated            "unevaluatedProperties"   check-property-unevaluatedProperties]]
   :draft6
   [["https://json-schema.org/draft-06/vocab/validation"              :validation             "type"                    check-property-type]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "const"                   check-property-const]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "minLength"               check-property-minLength]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "maxLength"               check-property-maxLength]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "multipleOf"              check-property-multipleOf]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "format"                  (make-check-property-format true)]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "enum"                    check-property-enum]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "pattern"                 check-property-pattern]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "$ref"                    check-property-$ref]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "$schema"                 check-property-$schema]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "$id"                     check-property-$id]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "id"                      check-property-id]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "$anchor"                 check-property-$anchor]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "$recursiveRef"           check-property-$recursiveRef]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "$recursiveAnchor"        check-property-$recursiveAnchor]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "$dynamicRef"             check-property-$dynamicRef]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "$dynamicAnchor"          check-property-$dynamicAnchor]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "$vocabulary"             check-property-$vocabulary]
    ["https://json-schema.org/draft-06/vocab/meta-data"               :meta-data              "title"                   check-property-title]
    ["https://json-schema.org/draft-06/vocab/meta-data"               :meta-data              "description"             check-property-description]
    ["https://json-schema.org/draft-06/vocab/meta-data"               :meta-data              "default"                 check-property-default]
    ["https://json-schema.org/draft-06/vocab/meta-data"               :meta-data              "examples"                check-property-examples]
    ["https://json-schema.org/draft-06/vocab/meta-data"               :meta-data              "deprecated"              check-property-deprecated]
    ["https://json-schema.org/draft-06/vocab/meta-data"               :meta-data              "readOnly"                check-property-readOnly]
    ["https://json-schema.org/draft-06/vocab/meta-data"               :meta-data              "writeOnly"               check-property-writeOnly]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "$comment"                check-property-$comment]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "definitions"             check-property-definitions]
    ["https://json-schema.org/draft-06/vocab/core"                    :core                   "$defs"                   check-property-$defs]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "propertyNames"           check-property-propertyNames]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "propertyDependencies"    check-property-propertyDependencies]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "minProperties"           check-property-minProperties]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "maxProperties"           check-property-maxProperties]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "not"                     check-property-not]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "anyOf"                   check-property-anyOf]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "oneOf"                   check-property-oneOf]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "allOf"                   check-property-allOf]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "required"                check-property-required]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "dependencies"            check-property-dependencies]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "dependentSchemas"        check-property-dependentSchemas]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "contains"                check-property-contains]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "minContains"             check-property-minContains]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "maxContains"             check-property-maxContains]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "minimum"                 check-property-minimum-new]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "exclusiveMinimum"        check-property-exclusiveMinimum-new]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "maximum"                 check-property-maximum-new]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "exclusiveMaximum"        check-property-exclusiveMaximum-new]
    ["https://json-schema.org/draft-06/vocab/content"                 :content                "contentEncoding"         (make-check-property-contentEncoding false)]
    ["https://json-schema.org/draft-06/vocab/content"                 :content                "contentMediaType"        (make-check-property-contentMediaType false)]
    ["https://json-schema.org/draft-06/vocab/content"                 :content                "contentSchema"           (make-check-property-contentSchema false)]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "if"                      check-property-if]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "then"                    check-property-then]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "else"                    check-property-else]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "minItems"                check-property-minItems]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "maxItems"                check-property-maxItems]
    ["https://json-schema.org/draft-06/vocab/validation"              :validation             "uniqueItems"             check-property-uniqueItems]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "items"                   check-property-items]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "additionalItems"         check-property-additionalItems]
    ["https://json-schema.org/draft-06/vocab/unevaluated"             :unevaluated            "unevaluatedItems"        check-property-unevaluatedItems]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "properties"              check-property-properties]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "patternProperties"       check-property-patternProperties]
    ["https://json-schema.org/draft-06/vocab/applicator"              :applicator             "additionalProperties"    check-property-additionalProperties]
    ["https://json-schema.org/draft-06/vocab/unevaluated"             :unevaluated            "unevaluatedProperties"   check-property-unevaluatedProperties]]
   :draft7
   [["https://json-schema.org/draft-07/vocab/validation"              :validation             "type"                    check-property-type]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "const"                   check-property-const]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "minLength"               check-property-minLength]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "maxLength"               check-property-maxLength]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "multipleOf"              check-property-multipleOf]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "format"                  (make-check-property-format true)]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "enum"                    check-property-enum]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "pattern"                 check-property-pattern]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "$ref"                    check-property-$ref]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "$schema"                 check-property-$schema]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "$id"                     check-property-$id]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "id"                      check-property-id]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "$anchor"                 check-property-$anchor]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "$recursiveRef"           check-property-$recursiveRef]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "$recursiveAnchor"        check-property-$recursiveAnchor]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "$dynamicRef"             check-property-$dynamicRef]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "$dynamicAnchor"          check-property-$dynamicAnchor]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "$vocabulary"             check-property-$vocabulary]
    ["https://json-schema.org/draft-07/vocab/meta-data"               :meta-data              "title"                   check-property-title]
    ["https://json-schema.org/draft-07/vocab/meta-data"               :meta-data              "description"             check-property-description]
    ["https://json-schema.org/draft-07/vocab/meta-data"               :meta-data              "default"                 check-property-default]
    ["https://json-schema.org/draft-07/vocab/meta-data"               :meta-data              "examples"                check-property-examples]
    ["https://json-schema.org/draft-07/vocab/meta-data"               :meta-data              "deprecated"              check-property-deprecated]
    ["https://json-schema.org/draft-07/vocab/meta-data"               :meta-data              "readOnly"                check-property-readOnly]
    ["https://json-schema.org/draft-07/vocab/meta-data"               :meta-data              "writeOnly"               check-property-writeOnly]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "$comment"                check-property-$comment]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "definitions"             check-property-definitions]
    ["https://json-schema.org/draft-07/vocab/core"                    :core                   "$defs"                   check-property-$defs]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "propertyNames"           check-property-propertyNames]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "propertyDependencies"    check-property-propertyDependencies]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "minProperties"           check-property-minProperties]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "maxProperties"           check-property-maxProperties]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "not"                     check-property-not]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "anyOf"                   check-property-anyOf]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "oneOf"                   check-property-oneOf]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "allOf"                   check-property-allOf]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "required"                check-property-required]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "dependencies"            check-property-dependencies]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "dependentSchemas"        check-property-dependentSchemas]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "contains"                check-property-contains]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "minContains"             check-property-minContains]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "maxContains"             check-property-maxContains]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "minimum"                 check-property-minimum-new]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "exclusiveMinimum"        check-property-exclusiveMinimum-new]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "maximum"                 check-property-maximum-new]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "exclusiveMaximum"        check-property-exclusiveMaximum-new]
    ["https://json-schema.org/draft-07/vocab/content"                 :content                "contentEncoding"         (make-check-property-contentEncoding true)]
    ["https://json-schema.org/draft-07/vocab/content"                 :content                "contentMediaType"        (make-check-property-contentMediaType true)]
    ["https://json-schema.org/draft-07/vocab/content"                 :content                "contentSchema"           (make-check-property-contentSchema true)]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "if"                      check-property-if]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "then"                    check-property-then]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "else"                    check-property-else]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "minItems"                check-property-minItems]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "maxItems"                check-property-maxItems]
    ["https://json-schema.org/draft-07/vocab/validation"              :validation             "uniqueItems"             check-property-uniqueItems]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "items"                   check-property-items]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "additionalItems"         check-property-additionalItems]
    ["https://json-schema.org/draft-07/vocab/unevaluated"             :unevaluated            "unevaluatedItems"        check-property-unevaluatedItems]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "properties"              check-property-properties]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "patternProperties"       check-property-patternProperties]
    ["https://json-schema.org/draft-07/vocab/applicator"              :applicator             "additionalProperties"    check-property-additionalProperties]
    ["https://json-schema.org/draft-07/vocab/unevaluated"             :unevaluated            "unevaluatedProperties"   check-property-unevaluatedProperties]]
   :draft2019-09
   [["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "type"                    check-property-type]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "const"                   check-property-const]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "minLength"               check-property-minLength]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "maxLength"               check-property-maxLength]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "multipleOf"              check-property-multipleOf]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "format"                  (make-check-property-format true)]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "enum"                    check-property-enum]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "pattern"                 check-property-pattern]
    ["https://json-schema.org/draft/2019-09/vocab/core"               :core              "$ref"                    check-property-$ref]
    ["https://json-schema.org/draft/2019-09/vocab/core"               :core              "$schema"                 check-property-$schema]
    ["https://json-schema.org/draft/2019-09/vocab/core"               :core              "$id"                     check-property-$id]
    ["https://json-schema.org/draft/2019-09/vocab/core"               :core              "$anchor"                 check-property-$anchor]
    ["https://json-schema.org/draft/2019-09/vocab/core"               :core              "$recursiveRef"           check-property-$recursiveRef]
    ["https://json-schema.org/draft/2019-09/vocab/core"               :core              "$recursiveAnchor"        check-property-$recursiveAnchor]
    ["https://json-schema.org/draft/2019-09/vocab/core"               :core              "$dynamicRef"             check-property-$dynamicRef]
    ["https://json-schema.org/draft/2019-09/vocab/core"               :core              "$dynamicAnchor"          check-property-$dynamicAnchor]
    ["https://json-schema.org/draft/2019-09/vocab/core"               :core              "$vocabulary"             check-property-$vocabulary]
    ["https://json-schema.org/draft/2019-09/vocab/meta-data"          :meta-data         "title"                   check-property-title]
    ["https://json-schema.org/draft/2019-09/vocab/meta-data"          :meta-data         "description"             check-property-description]
    ["https://json-schema.org/draft/2019-09/vocab/meta-data"          :meta-data         "default"                 check-property-default]
    ["https://json-schema.org/draft/2019-09/vocab/meta-data"          :meta-data         "examples"                check-property-examples]
    ["https://json-schema.org/draft/2019-09/vocab/meta-data"          :meta-data         "deprecated"              check-property-deprecated]
    ["https://json-schema.org/draft/2019-09/vocab/meta-data"          :meta-data         "readOnly"                check-property-readOnly]
    ["https://json-schema.org/draft/2019-09/vocab/meta-data"          :meta-data         "writeOnly"               check-property-writeOnly]
    ["https://json-schema.org/draft/2019-09/vocab/core"               :core              "$comment"                check-property-$comment]
    ["https://json-schema.org/draft/2019-09/vocab/core"               :core              "$defs"                   check-property-$defs]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "propertyNames"           check-property-propertyNames]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "propertyDependencies"    check-property-propertyDependencies]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "minProperties"           check-property-minProperties]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "maxProperties"           check-property-maxProperties]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "not"                     check-property-not]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "anyOf"                   check-property-anyOf]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "oneOf"                   check-property-oneOf]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "allOf"                   check-property-allOf]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "required"                check-property-required]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "dependentRequired"       check-property-dependentRequired]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "dependencies"            check-property-dependencies]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "dependentSchemas"        check-property-dependentSchemas]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "contains"                check-property-contains]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "minContains"             check-property-minContains]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "maxContains"             check-property-maxContains]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "minimum"                 check-property-minimum-new]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "exclusiveMinimum"        check-property-exclusiveMinimum-new]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "maximum"                 check-property-maximum-new]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "exclusiveMaximum"        check-property-exclusiveMaximum-new]
    ["https://json-schema.org/draft/2019-09/vocab/content"            :content           "contentEncoding"         (make-check-property-contentEncoding false)]
    ["https://json-schema.org/draft/2019-09/vocab/content"            :content           "contentMediaType"        (make-check-property-contentMediaType false)]
    ["https://json-schema.org/draft/2019-09/vocab/content"            :content           "contentSchema"           (make-check-property-contentSchema false)]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "if"                      check-property-if]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "then"                    check-property-then]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "else"                    check-property-else]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "minItems"                check-property-minItems]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "maxItems"                check-property-maxItems]
    ["https://json-schema.org/draft/2019-09/vocab/validation"         :validation        "uniqueItems"             check-property-uniqueItems]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "items"                   check-property-items]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "additionalItems"         check-property-additionalItems]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "unevaluatedItems"        check-property-unevaluatedItems]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "properties"              check-property-properties]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "patternProperties"       check-property-patternProperties]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "additionalProperties"    check-property-additionalProperties]
    ["https://json-schema.org/draft/2019-09/vocab/applicator"         :applicator        "unevaluatedProperties"   check-property-unevaluatedProperties]]
   :draft2020-12
   [["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "type"                    check-property-type]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "const"                   check-property-const]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "minLength"               check-property-minLength]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "maxLength"               check-property-maxLength]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "multipleOf"              check-property-multipleOf]
    ["https://json-schema.org/draft/2020-12/vocab/format-annotation"  :format-annotation "format"                  (make-check-property-format false)]
    ["https://json-schema.org/draft/2020-12/vocab/format-assertion"   :format-assertion  "format"                  (make-check-property-format true)]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "enum"                    check-property-enum]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "pattern"                 check-property-pattern]
    ["https://json-schema.org/draft/2020-12/vocab/core"               :core              "$ref"                    check-property-$ref]
    ["https://json-schema.org/draft/2020-12/vocab/core"               :core              "$schema"                 check-property-$schema]
    ["https://json-schema.org/draft/2020-12/vocab/core"               :core              "$id"                     check-property-$id]
    ["https://json-schema.org/draft/2020-12/vocab/core"               :core              "$anchor"                 check-property-$anchor]
    ["https://json-schema.org/draft/2020-12/vocab/core"               :core              "$recursiveRef"           check-property-$recursiveRef]
    ["https://json-schema.org/draft/2020-12/vocab/core"               :core              "$recursiveAnchor"        check-property-$recursiveAnchor]
    ["https://json-schema.org/draft/2020-12/vocab/core"               :core              "$dynamicRef"             check-property-$dynamicRef]
    ["https://json-schema.org/draft/2020-12/vocab/core"               :core              "$dynamicAnchor"          check-property-$dynamicAnchor]
    ["https://json-schema.org/draft/2020-12/vocab/core"               :core              "$vocabulary"             check-property-$vocabulary]
    ["https://json-schema.org/draft/2020-12/vocab/meta-data"          :meta-data         "title"                   check-property-title]
    ["https://json-schema.org/draft/2020-12/vocab/meta-data"          :meta-data         "description"             check-property-description]
    ["https://json-schema.org/draft/2020-12/vocab/meta-data"          :meta-data         "default"                 check-property-default]
    ["https://json-schema.org/draft/2020-12/vocab/meta-data"          :meta-data         "examples"                check-property-examples]
    ["https://json-schema.org/draft/2020-12/vocab/meta-data"          :meta-data         "deprecated"              check-property-deprecated]
    ["https://json-schema.org/draft/2020-12/vocab/meta-data"          :meta-data         "readOnly"                check-property-readOnly]
    ["https://json-schema.org/draft/2020-12/vocab/meta-data"          :meta-data         "writeOnly"               check-property-writeOnly]
    ["https://json-schema.org/draft/2020-12/vocab/core"               :core              "$comment"                check-property-$comment]
    ["https://json-schema.org/draft/2020-12/vocab/core"               :core              "$defs"                   check-property-$defs]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "propertyNames"           check-property-propertyNames]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "propertyDependencies"    check-property-propertyDependencies]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "minProperties"           check-property-minProperties]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "maxProperties"           check-property-maxProperties]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "not"                     check-property-not]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "anyOf"                   check-property-anyOf]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "oneOf"                   check-property-oneOf]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "allOf"                   check-property-allOf]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "required"                check-property-required]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "dependentRequired"       check-property-dependentRequired]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "dependencies"            check-property-dependencies]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "dependentSchemas"        check-property-dependentSchemas]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "contains"                check-property-contains]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "minContains"             check-property-minContains]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "maxContains"             check-property-maxContains]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "minimum"                 check-property-minimum-new]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "exclusiveMinimum"        check-property-exclusiveMinimum-new]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "maximum"                 check-property-maximum-new]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "exclusiveMaximum"        check-property-exclusiveMaximum-new]
    ["https://json-schema.org/draft/2020-12/vocab/content"            :content           "contentEncoding"         (make-check-property-contentEncoding false)]
    ["https://json-schema.org/draft/2020-12/vocab/content"            :content           "contentMediaType"        (make-check-property-contentMediaType false)]
    ["https://json-schema.org/draft/2020-12/vocab/content"            :content           "contentSchema"           (make-check-property-contentSchema false)]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "if"                      check-property-if]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "then"                    check-property-then]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "else"                    check-property-else]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "minItems"                check-property-minItems]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "maxItems"                check-property-maxItems]
    ["https://json-schema.org/draft/2020-12/vocab/validation"         :validation        "uniqueItems"             check-property-uniqueItems]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "prefixItems"             check-property-prefixItems]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "items"                   check-property-items]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "additionalItems"         check-property-additionalItems]
    ["https://json-schema.org/draft/2020-12/vocab/unevaluated"        :unevaluated       "unevaluatedItems"        check-property-unevaluatedItems]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "properties"              check-property-properties]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "patternProperties"       check-property-patternProperties]
    ["https://json-schema.org/draft/2020-12/vocab/applicator"         :applicator        "additionalProperties"    check-property-additionalProperties]
    ["https://json-schema.org/draft/2020-12/vocab/unevaluated"        :unevaluated       "unevaluatedProperties"   check-property-unevaluatedProperties]]
   :draft-next
   [["https://json-schema.org/draft/next/vocab/validation"            :validation           "type"                    check-property-type]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "const"                   check-property-const]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "minLength"               check-property-minLength]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "maxLength"               check-property-maxLength]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "multipleOf"              check-property-multipleOf]
    ["https://json-schema.org/draft/next/vocab/format-annotation"     :format-annotation    "format"                  (make-check-property-format false)]
    ["https://json-schema.org/draft/next/vocab/format-assertion"      :format-assertion     "format"                  (make-check-property-format true)]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "enum"                    check-property-enum]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "pattern"                 check-property-pattern]
    ["https://json-schema.org/draft/next/vocab/core"                  :core                 "$ref"                    check-property-$ref]
    ["https://json-schema.org/draft/next/vocab/core"                  :core                 "$schema"                 check-property-$schema]
    ["https://json-schema.org/draft/next/vocab/core"                  :core                 "$id"                     check-property-$id]
    ["https://json-schema.org/draft/next/vocab/core"                  :core                 "$anchor"                 check-property-$anchor]
    ["https://json-schema.org/draft/next/vocab/core"                  :core                 "$recursiveRef"           check-property-$recursiveRef]
    ["https://json-schema.org/draft/next/vocab/core"                  :core                 "$recursiveAnchor"        check-property-$recursiveAnchor]
    ["https://json-schema.org/draft/next/vocab/core"                  :core                 "$dynamicRef"             check-property-$dynamicRef]
    ["https://json-schema.org/draft/next/vocab/core"                  :core                 "$dynamicAnchor"          check-property-$dynamicAnchor]
    ["https://json-schema.org/draft/next/vocab/core"                  :core                 "$vocabulary"             check-property-$vocabulary]
    ["https://json-schema.org/draft/next/vocab/meta-data"             :meta-data            "title"                   check-property-title]
    ["https://json-schema.org/draft/next/vocab/meta-data"             :meta-data            "description"             check-property-description]
    ["https://json-schema.org/draft/next/vocab/meta-data"             :meta-data            "default"                 check-property-default]
    ["https://json-schema.org/draft/next/vocab/meta-data"             :meta-data            "examples"                check-property-examples]
    ["https://json-schema.org/draft/next/vocab/meta-data"             :meta-data            "deprecated"              check-property-deprecated]
    ["https://json-schema.org/draft/next/vocab/meta-data"             :meta-data            "readOnly"                check-property-readOnly]
    ["https://json-schema.org/draft/next/vocab/meta-data"             :meta-data            "writeOnly"               check-property-writeOnly]
    ["https://json-schema.org/draft/next/vocab/core"                  :core                 "$comment"                check-property-$comment]
    ["https://json-schema.org/draft/next/vocab/core"                  :core                 "$defs"                   check-property-$defs]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "propertyNames"           check-property-propertyNames]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "propertyDependencies"    check-property-propertyDependencies]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "minProperties"           check-property-minProperties]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "maxProperties"           check-property-maxProperties]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "not"                     check-property-not]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "anyOf"                   check-property-anyOf]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "oneOf"                   check-property-oneOf]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "allOf"                   check-property-allOf]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "required"                check-property-required]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "dependentRequired"       check-property-dependentRequired]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "dependencies"            check-property-dependencies]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "dependentSchemas"        check-property-dependentSchemas]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "contains"                check-property-contains]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "minContains"             check-property-minContains]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "maxContains"             check-property-maxContains]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "minimum"                 check-property-minimum-new]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "exclusiveMinimum"        check-property-exclusiveMinimum-new]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "maximum"                 check-property-maximum-new]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "exclusiveMaximum"        check-property-exclusiveMaximum-new]
    ["https://json-schema.org/draft/next/vocab/content"               :content              "contentEncoding"         (make-check-property-contentEncoding false)]
    ["https://json-schema.org/draft/next/vocab/content"               :content              "contentMediaType"        (make-check-property-contentMediaType false)]
    ["https://json-schema.org/draft/next/vocab/content"               :content              "contentSchema"           (make-check-property-contentSchema false)]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "if"                      check-property-if]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "then"                    check-property-then]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "else"                    check-property-else]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "minItems"                check-property-minItems]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "maxItems"                check-property-maxItems]
    ["https://json-schema.org/draft/next/vocab/validation"            :validation           "uniqueItems"             check-property-uniqueItems]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "prefixItems"             check-property-prefixItems]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "items"                   check-property-items]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "additionalItems"         check-property-additionalItems]
    ["https://json-schema.org/draft/next/vocab/unevaluated"           :unevaluated          "unevaluatedItems"        check-property-unevaluatedItems]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "properties"              check-property-properties]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "patternProperties"       check-property-patternProperties]
    ["https://json-schema.org/draft/next/vocab/applicator"            :applicator           "additionalProperties"    check-property-additionalProperties]
    ["https://json-schema.org/draft/next/vocab/unevaluated"           :unevaluated          "unevaluatedProperties"   check-property-unevaluatedProperties]]})

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

(defn make-property->index-and-check-2 [vs d]
  (into
   {}
   (map-indexed
    (fn [i [p c]] [p [i c]])
    (or
     vs
     ;; TODO - inject into initial context ?
     ;; how do we decide vocabulary for a self-descriptive schema ?
     (map (fn [l] (drop 2 l)) (draft->vocab-and-group-and-property-and-semantics d))))))

(def make-property->index-and-check (memoize make-property->index-and-check-2))

(defn compile-m2 [{vs :vocabularies d :draft :as c2} old-p2 m2]
  (map
   rest
   (sort-by
    first
    (reduce-kv
     (fn [acc k v]
       (if-let [[i c] ((make-property->index-and-check vs d) k)]
         (let [new-p2 (conj old-p2 k)]
           (conj acc (list i new-p2 (c k c2 new-p2 m2 v))))
         (do
           (log/warn (str "property: unexpected property encountered: " (pr-str k)))
           acc)))
     nil
     m2))))

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
  (cond
    (true? m2)
    (fn [c1 _p1 _m1]
      [c1 nil])

    (false? m2)
    (fn [c1 p1 m1]
      [c1
       (when (present? m1)
         [(make-error "schema is false: nothing will match" p2 m2 p1 m1)])])

    :else
    (fn [c1 p1 m1]
      (if (present? m1)
        (let [[new-c1 es]
              (reduce
               (fn [[old-c1 acc] [new-p2 cp]]
                 (let [[new-c1 [{m :message} :as es]] (cp old-c1 p1 m1)]
                   (when t? (println (pr-str new-p2) (pr-str p1) (if (seq es) ["❌" m] "✅")))
                   [new-c1 (concatv acc es)]))
               [c1 []]
               (compile-m2 c2 p2 m2))]
          [new-c1 ;; (first (stash new-c1 {} m1 p1))
           (make-error-on-failure "schema: document did not conform" p2 m2 p1 m1 es)])
        [c1 []]))))

;; quicker than actual 'apply' [?]
(defn apply3 [f [c p m]]
  (f c p m))

;; At the moment a validation is only a reduction of c1, not c2.
;; Since a draft switch is something that happens at c2-level, I think we need an interceptor...
;; if m2-fn returned [new-c2 m2] perhaps we would not need interceptors !
;; but we would have to thread m2 as well - can we do it - consider...

;; if validation was also a c2 reduction we could use that for vocabularies and maybe the marker-stash
;; investigate...

(defn make-draft-interceptor []
  ;; TODO: should we be resetting :vocabularies here ?
  (fn [delegate]
    (fn [c2 p2 {s "$schema" :as m2}]
      (delegate
       (if-let [d (and s ($schema->draft s))]
         (update
          c2
          :draft
          (fn [old-d new-d]
            (when (not= old-d new-d)
              (log/info (str "switching draft: " old-d " -> " new-d)))
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

(def check-schema
  ((make-ref-interceptor "$dynamicRef" expand-$dynamic-ref)
   ((make-ref-interceptor "$recursiveRef" expand-$recursive-ref)
    ((make-ref-interceptor "$ref" expand-$ref)
     ((make-anchor-interceptor (constantly "$dynamicAnchor") stash-$dynamic-anchor)
      ((make-anchor-interceptor (constantly "$recursiveAnchor") stash-$recursive-anchor)
       ((make-anchor-interceptor :id-key stash-$id)
        ((make-draft-interceptor)
         check-schema-2))))))))

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
          s (try (json-decode (slurp f)) (catch Exception _))]
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
        cs (check-schema c2 [] schema)]
    (fn [c1 {did id-key _dsid "$schema" :as document}]
      ;;(log/info "validate:" sid "/" did)
      ;;(when (and dsid (not (= sid dsid))) (log/warn (format "document schema id not consistent with schema id: %s != %s" dsid sid)))
      (let [c1 (assoc
                c1
                :id-key id-key
                :id-uri (when did (parse-uri did))
                :original-root document
                :recursive-anchor []
                :root document
                :draft draft
                :melder (:melder c2))] (cs c1 [] document)))))

(defn $schema->m2 [s]
  (uri->schema-2 {} [] (parse-uri s)))

(declare validate-m2)

(defn validate-m2-2 [{draft :draft :as c2} m1]
  (let [s (or (and (json-object? m1) (m1 "$schema")) (draft->$schema draft))]
    (if-let [m2 ($schema->m2 s)]
      (if (= m2 m1)
        ;; we are at the top
        (let [uri (parse-uri s) ;; duplicate work
              stash (uri->marker-stash uri)
              _ (when-not stash (prn "NO STASH FOR:" s))
              draft ($schema->draft s)
              ;; initialise c2`
              c2 (assoc
                  (merge c2 stash)
                  :id-uri uri
                  :vocabularies
                  (make-vocabularies
                   draft
                   (or (m2 "$vocabulary")
                       (into {} (map (fn [[v]] [v true]) (draft->vocab-and-group-and-property-and-semantics draft))))))
              v (validate-2 c2 m2)
              [_ es :as r] (v {} m1)]
          (if (empty? es)
            v
            (constantly r)))
        ;; keep going - inheriting relevant parts of c1
        (let [[{vs :vocabularies u->p :uri->path p->u :path->uri} es :as r] ((validate-m2 c2 m2) {} m1)]
          ;;(prn "STASH:" u->p p->u)
          (if (empty? es)
            (validate-2 (assoc c2
                               :marker-stash {:uri->path (or u->p {}) :path->uri (or p->u {})}
                               :vocabularies vs) m1)
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
