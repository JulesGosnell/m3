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
   #?(:clj  [cheshire.core :as cheshire]
      :cljs [cljs.core :as cljs])
   [cljc.java-time.local-date :refer [parse] :rename {parse local-date-parse}]
   [cljc.java-time.offset-date-time :refer [parse] :rename {parse offset-date-time-parse}]
   [cljc.java-time.offset-time :refer [parse] :rename {parse offset-time-parse}]
   [clojure.string :refer [starts-with? ends-with? replace] :rename {replace string-replace}]
   [#?(:clj clojure.tools.logging :cljs m3.log) :as log]
   [m3.util :refer [absent present? concatv into-set conj-set seq-contains? when-assoc]]
   [m3.uri :refer [parse-uri inherit-uri uri-base]]
   [m3.ref :refer [meld resolve-uri try-path]]
   [m3.pattern :refer [email-pattern ipv4-pattern ipv6-pattern hostname-pattern json-pointer-pattern relative-pointer-pattern uri-pattern uri-reference-pattern uri-template-pattern idn-email-pattern iri-pattern iri-reference-pattern uuid-pattern json-duration-pattern]]
   [m3.idn-hostname :refer [json-idn-hostname?]]
   )
  #?(:clj
     (:import
      [org.graalvm.polyglot Context Value]))
  )

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
     :clj  mod))

(def big-zero?
  #?(:cljs (fn [^js/Big b] (.eq b 0))
     :clj  zero?))

(def long-max-value
  #?(:clj Long/MAX_VALUE
     :cljs js/Number.MAX_SAFE_INTEGER))

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
  {"draft3"       #{"draft3"}
   "draft4"       #{"draft3" "draft4"}
   "draft6"       #{"draft3" "draft4" "draft6"}
   "draft7"       #{"draft3" "draft4" "draft6" "draft7"}
   "draft2019-09" #{"draft3" "draft4" "draft6" "draft7" "draft2019-09"}
   "draft2020-12" #{"draft3" "draft4" "draft6" "draft7" "draft2019-09" "draft2020-12"}
   "draft2021-12" #{"draft3" "draft4" "draft6" "draft7" "draft2019-09" "draft2020-12" "draft2021-12"}
   "latest"       #{"draft3" "draft4" "draft6" "draft7" "draft2019-09" "draft2020-12" "draft2021-12"}
   "draft-next"   #{"draft3" "draft4" "draft6" "draft7" "draft2019-09" "draft2020-12" "draft2021-12" "draft-next"}})

(def draft->$schema
  {"draft3"       "http://json-schema.org/draft-03/schema"
   "draft4"       "http://json-schema.org/draft-04/schema"
   "draft6"       "http://json-schema.org/draft-06/schema"
   "draft7"       "http://json-schema.org/draft-07/schema"
   "draft2019-09" "https://json-schema.org/draft/2019-09/schema"
   "draft2020-12" "https://json-schema.org/draft/2020-12/schema"
   "latest"       "https://json-schema.org/draft/2020-12/schema"
   "draft-next"   "https://json-schema.org/draft/next/schema"})

(def $schema->draft
  (reduce-kv (fn [acc k v] (conj (conj acc [v k]) [(str v "#") k])) {} (dissoc draft->$schema "latest")))

(def $schema-uri->draft
  (reduce-kv (fn [acc k v] (conj acc [(parse-uri k) v])) {} $schema->draft))

(def latest-$schema (draft->$schema "latest"))

;;------------------------------------------------------------------------------

;; use Graal/JavaScript to acquire an ECMA-262 compliant RegeExp engine
;; now we can use the same code at both back and front end...

#?(:clj (defonce ^Context js-context (Context/create (into-array ["js"]))))
#?(:clj (defonce ^Value  RegExp (.getMember (.getBindings js-context "js") "RegExp")))

#?(:clj  (defn ecma-pattern [^String s] (.newInstance RegExp (into-array Object [s "u"])))
   :cljs (defn ecma-pattern [s] (js/RegExp. s "u")))

#?(:clj  (defn ecma-match [^Value r ^String s] (.asBoolean (.invokeMember r "test" (into-array Object [s]))))
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
    (when (string? m1)
      (match format pattern c2 p2 m2 p1 m1))))

(defn check-parse [p f _c2 p2 m2]
  (fn [_c1 p1 m1]
    (when (string? m1)
      (try
        (p m1)
        nil
        (catch Exception e
          [(make-error (str "format: not a valid " f ": " (ex-message e)) p2 m2 p1 m1)])))))

;; standard formats

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

(defmethod check-format "time" [f c2 p2 m2]
  (check-parse offset-time-parse f c2 p2 m2))

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

;; this is really difficult.
;; I can't find a java, javascript, clojure or clojurescript library which comes close
;; writing my own seems like an unreasable amount of work just to pass this one part of the spec
;; wait for someone else to do it or AI to get good enough to generate the code....
(defmethod check-format "idn-hostname" [_format _c2 p2 m2]
  (fn [_c1 p1 m1]
    (when (and (string? m1) (not (json-idn-hostname? m1)))
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
    (when (and (string? m1) (not (json-duration? m1)))
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
  (check-type-3 map? t c2 p2 m2))

(defmethod check-type "array" [t c2 p2 m2]
  (check-type-3 vector? t c2 p2 m2))

(defmethod check-type "string" [t c2 p2 m2]
  (check-type-3 string? t c2 p2 m2))

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
         ;; TODO: we should report all the errors...
        [c1
         (when-not (some (fn [checker] (nil? (second (checker c1 p1 m1)))) checkers)
           [(make-error (format "type: none matched: %s" ts) p2 m2 p1 m1)])]))

    (fn [c1 p1 m1]
      [c1 [(make-error (format "type: unrecognised: %s" ts) p2 m2 p1 m1)]])))

;;------------------------------------------------------------------------------

(defmulti check-property (fn [property _c2 _p2 _m2 _v2s]
                           ;;(println "check-property:" p2 p1)
                           property))

;; standard common properties

(defmethod check-property "type" [_property c2 p2 m2 [v2]]
  (check-type v2 c2 p2 m2))

(defmethod check-property "const" [_property _c2 p2 m2 [v2]]
  (fn [c1 p1 m1]
    [c1
     (when (not (json-= v2 m1))
       [(make-error (format "const: document does not contain schema value: %s != %s" m1 v2) p2 m2 p1 m1)])]))

(defmethod check-property "enum" [_property _c2 p2 m2 [v2]]
  (fn [c1 p1 m1]
     ;; we could check that the m2's enum contained any const or default
     ;; value here - but thus should be done somehow during validation of
     ;; the m2 as an m1 and not waste time whilst we are validating all
     ;; it's m1s...
     ;; TODO: how about some injectable consistency checking fns which can be used when validating m2s ?
    [c1
     (when-not (seq-contains? v2 json-= m1)
       [(make-error "enum: does not contain value" p2 m2 p1 m1)])]))

(defmethod check-property "id" [_property {draft :draft} _p2 _m2 [v2]]
  (fn [c1 p1 _m1]
    (if (#{"draft3" "draft4"} draft)
      (let [uri (inherit-uri (c1 :id-uri) (parse-uri v2))]
        [(-> c1
             (update :path->uri assoc p1 uri)  ; Stash path to uri
             (update :uri->path assoc uri p1)  ; Stash uri to path
             (assoc :id-uri uri))
         nil])
      (do
        (log/warn (str "id: ignored in unsupported draft: " draft))
        [c1 nil]))))

(defmethod check-property "$id" [_property {draft :draft} _p2 _m2 [v2]]
  (fn [c1 p1 _m1]
    (if (contains? (draft->draft? draft) "draft6")
      (let [uri (inherit-uri (c1 :id-uri) (parse-uri v2))]
        [(-> c1
             (update :path->uri assoc p1 uri)  ; Stash path to uri
             (update :uri->path assoc uri p1)  ; Stash uri to path
             (assoc :id-uri uri))
         nil])
      (do
        (log/warn (str "$id: ignored in unsupported draft: " draft))
        [c1 nil]))))

;; Anchors only need :uri->path (no change, but included for completeness)
(defmethod check-property "$anchor" [_property {draft :draft} _p2 _m2 [v2]]
  (fn [c1 p1 _m1]
    (if (contains? (draft->draft? draft) "draft2019-09")
      (let [anchor-uri (inherit-uri (c1 :id-uri) (parse-uri (str "#" v2)))]
        [(update c1 :uri->path assoc anchor-uri p1) nil])
      (do
        (log/warn (str "$anchor: ignored in unsupported draft: " draft))
        [c1 nil]))))

(defmethod check-property "$recursiveAnchor" [_property {draft :draft} _p2 _m2 [v2]]
  (fn [c1 p1 _m1]
    (if (= "draft2019-09" draft)
      (if (true? v2)
        (let [[uris top] (c1 :$recursive-anchor [#{} nil])]
          [(assoc c1 :$recursive-anchor [(conj uris (c1 :id-uri)) (or top p1)]) nil])
        [c1 nil])
      (do
        (log/warn (str "$recursiveAnchor: ignored in unsupported draft: " draft))
        [c1 nil]))))

(defmethod check-property "$dynamicAnchor" [_property {draft :draft} _p2 _m2 [v2]]
  (fn [c1 p1 _m1]
    (if (contains? (draft->draft? draft) "draft2020-12")
      (let [anchor-uri (inherit-uri (c1 :id-uri) (parse-uri (str "#" v2)))]
        [(update c1 :$dynamic-anchor assoc anchor-uri p1) nil])
      (do
        (log/warn (str "$dynamicAnchor: ignored in unsupported draft: " draft))
        [c1 nil]))))

(defmethod check-property "$comment" [_property _c2 _p2 _m2 [v2]]
  (fn [c1 p1 _m1]
    ;;(log/info (str "$comment:" v2 " : " p1))
    [c1 nil]))

(defmethod check-property "description"      [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property "title"            [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property "readOnly"         [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property "writeOnly"        [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))

(defmethod check-property "default"          [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property "$schema"          [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil])) ;; TODO: switch drafts in context...
(defmethod check-property "examples"         [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property "$vocabulary"      [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))

;; TODO: issue a warning somehow
(defmethod check-property "deprecated"  [_property _c2 _p2 _m2 _v2s] (fn [_c1 _p1 _m1])) ;; TODO: issue a warning or error ?

;; standard number properties

(defmethod check-property "minimum" [_property {d :draft} p2 m2 [v2]]
  (case d
    ("draft3" "draft4")
    (let [e? (m2 "exclusiveMinimum")
          p? (if e? < <=)]
      (fn [c1 p1 m1]
        [c1
         (if (or
              (not (number? m1))
              (p? v2 m1))
           []
           [(make-error (str "minimum" (when e? "(with exclusiveMinimum)") ": value to low") p2 m2 p1 m1)])]))
   ;; default
    (fn [c1 p1 m1]
      [c1
       (if (or
            (not (number? m1))
            (<= v2 m1))
         []
         [(make-error "minimum: value to low" p2 m2 p1 m1)])])))

(defmethod check-property "exclusiveMinimum" [_property {d :draft} p2 {m "minimum" :as m2} [v2]]
  (case d
    ("draft3" "draft4")
    (fn [c1 _p1 _m1]
      (when-not m (log/warn "exclusiveMinimum: no minimum present to modify"))
      [c1 []])
     ;; default
    (fn [c1 p1 m1]
      [c1
       (if (or
            (not (number? m1))
            (< v2 m1))
         []
         [(make-error "minimum: value to low" p2 m2 p1 m1)])])))

(defmethod check-property "maximum" [_property {d :draft} p2 m2 [v2]]
  (case d
    ("draft3" "draft4")
    (let [e? (m2 "exclusiveMaximum")
          p? (if e? > >=)]
      (fn [c1 p1 m1]
        [c1
         (if (or
              (not (number? m1))
              (p? v2 m1))
           []
           [(make-error (str "maximum" (when e? "(with exclusiveMaximum)") ": value too high") p2 m2 p1 m1)])]))
   ;; default
    (fn [c1 p1 m1]
      [c1
       (if (or
            (not (number? m1))
            (>= v2 m1))
         []
         [(make-error "maximum: value too high" p2 m2 p1 m1)])])))

(defmethod check-property "exclusiveMaximum" [_property {d :draft} p2 {m "maximum" :as m2} [v2]]
  (case d
    ("draft3" "draft4")
    (fn [c1 _p1 _m1]
      (when-not m (log/warn "exclusiveMaximum: no maximum present to modify"))
      [c1 []])
     ;; default
    (fn [c1 p1 m1]
      [c1
       (if (or
            (not (number? m1))
            (> v2 m1))
         []
         [(make-error "maximum: value too high" p2 m2 p1 m1)])])))

(defmethod check-property "multipleOf" [_property _c2 p2 m2 [v2]]
  (let [v2-bd (bigdec v2)]
    (fn [c1 p1 m1]
      [c1
       (when (and
              (json-number? m1)
              (not (big-zero? (big-mod (bigdec m1) v2-bd))))
         [(make-error (format "%s is not a multiple of %s" m1 v2) p2 m2 p1 m1)])])))

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

(defmethod check-property "minLength" [_property _c2 p2 m2 [v2]]
  (let [ml2 (quot v2 2)]
    (fn [c1 p1 m1]
      [c1
       (when (and
              (string? m1)
              (or
               (< (count m1) ml2) ;; precheck before using expensive json-length
               (< (json-length m1) v2)))
         [(make-error "minLength: string too short" p2 m2 p1 m1)])])))

(defmethod check-property "maxLength" [_property _c2 p2 m2 [v2]]
  (let [ml2 (* v2 2)]
    (fn [c1 p1 m1]
      [c1
       (when (and
              (string? m1)
              (or
               (> (count m1) ml2) ;; precheck before using expensive json-length
               (> (json-length m1) v2)))
         [(make-error "maxLength: string too long" p2 m2 p1 m1)])])))

(defmethod check-property "pattern" [_property c2 p2 m2 [v2]]
  (if (starts-with? v2 "$format:")
    ;; N.B.
    ;; this is an extension to allow patternProperties to
    ;; leverage formats since the spec does not provide a
    ;; formatProperties...
    (check-property "format" c2 p2 m2 [(subs v2 (count "$format:"))])
    (let [p (ecma-pattern v2)]

      (fn [c1 p1 m1]
        [c1
         (when (and
                (json-string? m1)
                (false? (ecma-match p m1)))
           [(make-error "pattern: doesn't match" p2 m2 p1 m1)])]))))

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
   "base64" base64-decode
   })

(def cmt->decoder
  {
   "application/json" json-decode
   })

(defmethod check-property "contentEncoding" [_property {d :draft} p2 m2 [v2]]
  (let [strict? (#{"draft7"} d) ;; TODO: check a context flag aswell
        ce-decoder (ce->decoder v2)
        pp2 (butlast p2)]
    (fn [c1 p1 old-m1]
      (if (string? old-m1)
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
            [c1 es]))
        [old-m1 nil]))))

(defmethod check-property "contentMediaType" [_property {d :draft} p2 m2 [v2]]
  (let [strict? (#{"draft7"} d) ;; TODO: check a context flag aswell
        cmt v2
        cmt-decoder (cmt->decoder cmt)
        pp2 (butlast p2)]
    (fn [c1 p1 m1]
      (let [old-m1 (or (get (get c1 :content) pp2) m1)]
        (if (string? old-m1)
          (let [[new-m1 es]
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
              [c1 es]))
          [old-m1 nil])))))

(defmethod check-property "contentSchema" [_property {d :draft :as c2} p2 {cmt "contentMediaType"} [v2]]
  (let [strict? (#{"draft7"} d) ;; TODO: check a context flag aswell
        checker (check-schema c2 p2 v2)
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
             (:errors (ex-data e))))]))))

(defmethod check-property "format" [_property {strict? :strict-format? cfs :check-format :or {cfs {}} :as c2} p2 m2 [v2]]
  (let [f (if strict?
            (fn [f2] (fn [c p m] [c (f2 c p m)]))
            (fn [f2] (fn [c p m] (when-let [[{m :message}] (f2 c p m)] [c (log/warn m)]))))]
    ;; we do this here so that user may override default format checkers...
    (f ((or (cfs v2) check-format) v2 c2 p2 m2))))

(defmethod check-property "dependencies" [_property {d :draft :as c2} p2 m2 [v2]]
  (case d
    ("draft2019-09" "draft2020-12" "draft2021-12" "draft-next")
    ;; this keyword no longer exists - it was split into "dependentRequired" and "dependentSchemas"...
    (log/info (str "dependencies was split into dependentRequired and dependentSchemas in draft2019-09 - you are using: " d))
    nil)
  (let [property->checker
        (reduce
         (fn [acc [k v]]
           (assoc
            acc
            k
            (cond
              (json-string? v) ;; a single property dependency
              (fn [c1 p1 m1] [c1 (when (not (contains? m1 v)) [v v])])
              (json-array? v) ;; a multiple property dependency
              ;; TODO: this looks very suspect
              (fn [c1 p1 m1] [c1 (reduce (fn [acc2 k2] (if (contains? m1 k2) acc2 (conj acc2 [k k2]))) [] v)])
              (or (json-object? v) (boolean? v)) ;; a schema dependency
              (check-schema c2 p2 v)
              ;; we should not need to check other cases as m2 should have been validated against m3
              )))
         {}
         v2)]
    (fn [c1 p1 m1]
      (if (json-object? m1)
        (let [[c1 es]
              (reduce
               (fn [[c old-es] [k v]]
                 (if (contains? m1 k)
                   (let [[c new-es] ((property->checker k) c p1 m1)]
                     [c (concatv old-es new-es)])
                   [c old-es]))
               [c1 []]
               v2)]
          [c1
           (when-let [missing (seq es)]
             [(make-error ["dependencies: missing properties (at least):" missing] p2 m2 p1 m1)])])
        [c1 []]))))

;; do same for dependencies
(defmethod check-property "dependentSchemas" [_property {d :draft :as c2} p2 m2 [v2]]
  (case d
    ("draft3" "draft4" "draft6" "draft7")
    ;; this keyword didn't exist - it arose from the splitting of "dependencies'
    (fn [c1 p1 m1]
      (log/warn (str "dependentSchemas:  was not introduced until draft2019-09 - you are using: " d " - ignored"))
      [c1 nil])

    ("draft2019-09" "draft2020-12" "draft2021-12" "draft-next")
    (let [property->checker
          (reduce
           (fn [acc [k v]]
             (assoc acc k (check-schema c2 p2 v)))
           {}
           v2)]
      (fn [c1 p1 m1]
        (if (json-object? m1)
          (let [[c1 es]
                (reduce
                 (fn [[c old-es] [k v]]
                   (if (contains? m1 k)
                     (let [[c new-es] ((property->checker k) c p1 m1)]
                       [c (concatv old-es new-es)])
                     [c old-es]))
                 [c1 []]
                 v2)]
            [c1
             (when-let [missing (seq es)]
               [(make-error ["dependentSchemas: missing properties (at least):" missing] p2 m2 p1 m1)])])
          [c1 nil])))))

(defmethod check-property "propertyDependencies" [_property c2 p2 _m2 [v2]]
  (let [checkers (into {} (mapcat (fn [[k1 vs]] (map (fn [[k2 s]] [[k1 k2] (check-schema c2 p2 s)]) vs)) v2))
        ks (keys v2)]
    (fn [c1 p1 m1]
      (if (json-object? m1)
        (reduce
         (fn [[c old-es] k]
           (let [v (m1 k)]
             (if-let [checker (and (json-string? v) (checkers [k v]))]
               (let [[c new-es] (checker c p1 m1)]
                 [c (concatv old-es new-es)])
               [c old-es])))
         [c1 []]
         ks)
        [c1 []]))))

;; TODO: share more code with dependencies
(defmethod check-property "dependentRequired" [_property {d :draft} p2 m2 [v2]]
  (case d
    ("draft3" "draft4" "draft6" "draft7")
    (fn [c1 _p1 _m1]
      (log/warn (str "dependentRequired: was not introduced until draft2019-09 - you are using: " d " - ignored"))
      [c1 nil])

    ("draft2019-09" "draft2020-12" "draft2021-12" "draft-next")
    (let [property->checker
          (reduce
           (fn [acc [k v]]
             (assoc
              acc
              k
              (fn [c1 p1 m1] (reduce (fn [acc2 k2] (if (contains? m1 k2) acc2 (conj acc2 [k k2]))) [] v))))
           {}
           v2)]
      (fn [c1 p1 m1]
        [c1
         (when (json-object? m1)
           (when-let [missing
                      (seq
                       (reduce
                        (fn [acc [k v]]
                          (if (contains? m1 k)
                            (concatv acc ((property->checker k) c1 p1 m1))
                            acc))
                        []
                        v2))]
             [(make-error ["dependentRequired: missing properties (at least):" missing] p2 m2 p1 m1)]))]))))

;;------------------------------------------------------------------------------

(defmethod check-property "if" [_property c2 p2 _m2 [v2]]
  (let [checker (check-schema c2 p2 v2)
        pp2 (butlast p2)]
    (fn [old-c1 p1 m1]
      (let [[new-c1 es] (checker old-c1 p1 m1)
            success? (empty? es)]
        [(update (if success? new-c1 old-c1) :if assoc pp2 success?) []]))))

(defmethod check-property "then" [_property c2 p2 _m2 [v2]]
  (let [checker (check-schema c2 p2 v2)
        pp2 (butlast p2)]
    (fn [c1 p1 m1]
      (if (true? (get (get c1 :if) pp2))
        (checker c1 p1 m1)
        [c1 []]))))

(defmethod check-property "else" [_property c2 p2 _m2 [v2]]
  (let [checker (check-schema c2 p2 v2)
        pp2 (butlast p2)]
    (fn [c1 p1 m1]
      (if (false? (get (get c1 :if) pp2))
        (checker c1 p1 m1)
        [c1 []]))))

(defmethod check-property "definitions" [_property c2 p2 _m2 [v2]]
  (mapv (fn [[k v]] (check-schema c2 (conj p2 k) v)) v2)
  (fn [c1 _p1 _m1] [c1 nil]))

(defmethod check-property "$defs" [_property c2 p2 _m2 [v2]]
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
               (update :matched   update pp2 into-set ks)
           ;; TODO: only record evaluated if unevaluatedProperties needed later ?
               (update :evaluated update p1 into-set ks)))
         (make-error-on-failure message p2 m2 p1 m1 es)]))))

(defmethod check-property "properties" [_property c2 p2 m2 [ps]]
  (let [k-and-css (mapv (fn [[k v]] [k (check-schema c2 (conj p2 k) v)]) ps)
        cp (check-properties c2 p2 m2)]
    (fn [c1 p1 m1]
      (if (json-object? m1)
        (let [k-and-css (filter (fn [[k]] (contains? m1 k)) k-and-css)]
          (cp c1 p1 m1 k-and-css "properties: at least one property did not conform to respective schema"))
        [c1 []]))))

;; what is opposite of "additional" - "matched" - used by spec to refer to properties matched by "properties" or "patternProperties"

(defmethod check-property "patternProperties" [_property c2 p2 m2 [pps]]
  (let [cp-and-pattern-and-ks (mapv (fn [[k v]] [(check-schema c2 (conj p2 k) v) (ecma-pattern k) k]) pps)
        cp (check-properties c2 p2 m2)]
    (fn [c1 p1 m1]
      (if (json-object? m1)
        (let [k-and-css (apply concat (keep (fn [[k]] (keep (fn [[cs p]] (when (ecma-match p k) [k cs])) cp-and-pattern-and-ks)) m1))]
          (cp c1 p1 m1 k-and-css "patternProperties: at least one property did not conform to respective schema"))
        [c1 []]))))


(defmethod check-property "additionalProperties" [_property c2 p2 m2 [v2]]
  (let [cs (check-schema c2 p2 v2)
        pp2 (butlast p2)
        cp (check-properties c2 p2 m2)]
    (fn [c1 p1 m1]
      (if (json-object? m1)
        (let [mps (get (get c1 :matched) pp2 #{})
              aps (remove (fn [[k]] (contains? mps k)) m1) ;; k might be nil
              p-and-css (mapv (fn [[k]] [k cs]) aps)] ; TODO: feels inefficient
          (cp c1 p1 m1 p-and-css "additionalProperties: at least one property did not conform to schema"))
        [c1 []]))))

(defmethod check-property "unevaluatedProperties" [_property c2 p2 m2 [v2]]
  (let [cs (check-schema c2 p2 v2)
        cp (check-properties c2 p2 m2)]
    (fn [c1 p1 m1]
      (if (json-object? m1)
        (let [eps (get (get c1 :evaluated) p1 #{})
              ups (remove (fn [[k]] (contains? eps k)) m1) ;; k might be nil
              p-and-css (mapv (fn [[k]] [k cs]) ups)] ; TODO: feels inefficient
          (cp c1 p1 m1 p-and-css "unevaluatedProperties: at least one property did not conform to schema"))
        [c1 []]))))

(defmethod check-property "propertyNames" [_property c2 p2 m2 [v2]]
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

(defmethod check-property "required" [_property _c2 p2 m2 [v2]]
  (fn [c1 p1 m1]
    [c1
     (when (json-object? m1)
       (when-let [missing (seq (reduce (fn [acc k] (if (contains? m1 k) acc (conj acc k))) [] v2))]
         [(make-error ["required: missing properties (at least):" missing] p2 m2 p1 m1)]))]))

(defmethod check-property "minProperties" [_property _c2 p2 m2 [v2]]
  (fn [c1 p1 m1]
    [c1
     (when (and
            (json-object? m1)
            (< (count m1) v2))
       [(make-error "minProperties: document contains too few properties" p2 m2 p1 m1)])]))

(defmethod check-property "maxProperties" [_property _c2 p2 m2 [v2]]
  (fn [c1 p1 m1]
    [c1
     (when (and
            (json-object? m1)
            (> (count m1) v2))
       [(make-error "maxProperties: document has too many properties" p2 m2 p1 m1)])]))

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

;; TODO: consider warning if using tuple form of items post draft2020-12
(defmethod check-property "prefixItems" [_property {d :draft :as c2}  p2 m2 [v2]]
  (case d
    ("draft3" "draft4" "draft6" "draft7" "draft2019-09")
    (fn [c1 p1 m1]
      (log/warn (str "prefixItems: was not introduced until draft2020-12 - you are using: " d " - ignored"))
      [c1 nil])
    ("draft2020-12" "draft2021-12" "draft-next")
    (let [i-and-css (vec (map-indexed (fn [i sub-schema] [i (check-schema c2 (conj p2 i) sub-schema)]) v2))
          ci (check-items c2 p2 m2)]
      (fn [c1 p1 m1]
        (if (json-array? m1)
          (ci c1 p1 m1 i-and-css "prefixItems: at least one item did not conform to respective schema")
          [c1 []])))))

(defmethod check-property "items" [_property {d :draft :as c2}  p2 m2 [v2]]
  (let [n (count (m2 "prefixItems")) ;; TODO: achieve this by looking at c1 ?
        [m css] (if (json-array? v2)
                  (do
                    (case d
                      ("draft3" "draft4" "draft6" "draft7" "draft2019-09")
                      nil
                      ("draft2020-12" "draft2021-12" "draft-next")
                      (log/info (str "prefixItems: was introduced in draft2020-12 to handle tuple version of items - you are using: " d)))
                    ["respective " (map-indexed (fn [i v] (check-schema c2 (conj p2 i) v)) v2)])
                  ["" (repeat (check-schema c2 p2 v2))])
        ci (check-items c2 p2 m2)]
    (fn [c1 p1 m1]
      (if (json-array? m1)
        (let [items (drop n m1)
              i-and-css (mapv (fn [i cs _] [(+ i n) cs]) (range) css items)]
          (ci c1 p1 items i-and-css (str "items: at least one item did not conform to " m "schema")))
        [c1 []]))))

(defmethod check-property "additionalItems" [_property c2 p2 {is "items" :as m2} [v2]]
  (if (json-array? is) ;; additionalItems is only used when items is a tuple
    (let [cs (check-schema c2 p2 v2)
          ;;pp2 (butlast p2)
          ci (check-items c2 p2 m2)]
      (fn [c1 p1 m1]
        (if (json-array? m1)
          (let [;; this is how it should be done, but cheaper to just look at items (must be array for additionalItems to be meaningful) in m2 time
                 ;;mis (get (get c1 :matched) pp2 #{})
                 ;;ais  (remove (fn [[k]] (contains? mis k)) (map-indexed vector m1))
                 ;;i-and-css (mapv (fn [[k]] [k cs]) ais) ; TODO: feels inefficient
                n (count is)
                ais (drop n m1)
                i-and-css (mapv (fn [i cs _] [(+ i n) cs]) (range) (repeat cs) ais)]
            (ci c1 p1 ais i-and-css "additionalItems: at least one item did not conform to schema"))
          [c1 []])))
    (fn [c1 _p1 _m1]
      [c1 nil])))
    
(defmethod check-property "unevaluatedItems" [_property c2 p2 m2 [v2]]
  (let [css (repeat (check-schema c2 p2 v2))
        ci (check-items c2 p2 m2)]
    (fn [{p->eis :evaluated :as c1} p1 m1]
      (if (json-array? m1)
        (let [eis (or (get p->eis p1) #{})
              index-and-items (filter (fn [[k]] (not (eis k))) (map-indexed (fn [i v] [i v]) m1))
              i-and-css (mapv (fn [cs [i]] [i cs]) css index-and-items)] ;; TODO: item not used
          (ci c1 p1 (map second index-and-items) i-and-css "unevaluatedItems: at least one item did not conform to schema"))
        [c1 []]))))

(defmethod check-property "contains" [_property c2 p2 {mn "minContains" :as m2} [v2]]
  (let [cs (check-schema c2 p2 v2)
        base (if mn mn 1)
        ci (check-items c2 p2 v2)]
    (fn [c1 p1 m1]
      (if (json-array? m1)
        (let [i-and-css (map (fn [i _] [i cs]) (range) m1)
              [new-c1 [{es :errors}]]
              (ci c1 p1 m1 i-and-css "contains: at least one item did not conform to schema")
              matches (- (count m1) (count es))]
          (if (<= (min base 1) matches)
            [new-c1 nil]
            [c1 [(make-error "contains: document has no matches" p2 m2 p1 m1)]]))))))

(defmethod check-property "minContains" [_property _c2 p2 m2 [v2]]
  (let [pp2 (butlast p2)]
    (fn [{matched :matched :as c1} p1 m1]
      (if-let [matches (and (json-array? m1) (get matched pp2))]
        (let [n (count matches)]
          (if (and
               matches
               (json-array? m1)
               (<= v2 n))
            [c1 nil]
            [c1 [(make-error (str "minContains: document has too few matches - " n) p2 m2 p1 m1)]]))
        [c1 nil]))))

(defmethod check-property "maxContains" [_property _c2 p2 m2 [v2]]
  (let [pp2 (butlast p2)]
    (fn [{matched :matched :as c1} p1 m1]
      (if-let [matches (and (json-array? m1) (get matched pp2))]
        (let [n (count matches)]
          (if (<= n v2)
            [c1 nil]
            [c1 [(make-error (str "maxContains: document has too many matches - " n) p2 m2 p1 m1)]]))
        [c1 nil]))))

(defmethod check-property "minItems" [_property _c2 p2 m2 [v2]]
  (fn [c1 p1 m1]
    [c1
     (when (and
            (json-array? m1)
            (< (count m1) v2))
       [(make-error "minItems: document contains too few items" p2 m2 p1 m1)])]))

(defmethod check-property "maxItems" [_property _c2 p2 m2 [v2]]
  (fn [c1 p1 m1]
    [c1
     (when (and
            (json-array? m1)
            (> (count m1) v2))
       [(make-error "maxItems: document contains too many items" p2 m2 p1 m1)])]))

;; TODO: should be unique according to json equality
;; TODO: could be more efficient - only needs to find one duplicate before it bails..
;; TODO: use sorted-set-by and put items in 1 at a time until one is rejected then bail - reduced
(defmethod check-property "uniqueItems" [_property _c2 p2 m2 [v2]]
  (if v2
    (fn [c1 p1 m1]
      [c1
       (when (json-array? m1)
         (when (not (= (count m1) (count (distinct m1))))
           [(make-error "uniqueItems: document contains duplicate items" p2 m2 p1 m1)]))])
    (fn [c1 _p1 _m1]
      [c1 nil])))

;; TODO: merge code with check-items...
(defn check-of [c2 p2 m2 v2]
  (let [i-and-css (vec (map-indexed (fn [i sub-schema] [i (check-schema c2 (conj p2 i) sub-schema)]) v2))]
    (fn [c1 p1 m1 message failed?]
      (let [old-local-c1 (update c1 :evaluated dissoc p1)
            [c1 es]
            (reduce
             (fn [[old-c old-es] [i cs]]
               (let [[new-local-c1 new-es] (cs old-local-c1 p1 m1)
                     new-c (if (empty? new-es) (update old-c :evaluated update p1 into-set (get (get new-local-c1 :evaluated) p1)) old-c)
                     es (concatv old-es new-es)]
                 [new-c es]))
             [c1 []]
             i-and-css)]
        [c1
         (make-error-on message p2 m2 p1 m1 failed? es)]))))

(defmethod check-property "oneOf" [_property c2 p2 m2 [v2]]
  (let [co (check-of c2 p2 m2 v2)
        m2-count (count v2)]
    (fn [c1 p1 m1]
      (co
       c1 p1 m1
       "oneOf: document failed to conform to one and only one sub-schema"
       (fn [es] (not= 1 (- m2-count (count es))))))))

(defmethod check-property "anyOf" [_property c2 p2 m2 [v2]]
  (let [co (check-of c2 p2 m2 v2)
        m2-count (count v2)]
    (fn [c1 p1 m1]
      (co
       c1 p1 m1
       "anyOf: document failed to conform to at least one sub-schema"
       (fn [es] (not (< (count es) m2-count)))))))
         
(defmethod check-property "allOf" [_property c2 p2 m2 [v2]]
  (let [co (check-of c2 p2 m2 v2)]
    (fn [c1 p1 m1]
      (co
       c1 p1 m1
       "allOf: document failed to conform to all sub-schemas"
       seq))))

;; TODO: share check-of
(defmethod check-property "not" [_property c2 p2 m2 [v2]]
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

;; catch-all

(defmethod check-property :default [property {{checker property} :validators :as c2} p2 m2 v2s]
  (if checker
    (let [cp (checker property c2 p2 m2 v2s)]
      (fn [c1 p1 m1]
        [c1 (cp p1 m1)]))
    (let [m (str "property: unexpected property encountered: " (pr-str property))]
      (fn [c1 _p1 _m1]
        [c1 (log/warn m)]))))

;;------------------------------------------------------------------------------

(let [property-groups
      [;; TODO: push quick wins towards lower ranks - type, format etc...

       ["type"]
       ["const"]
       ["minLength"]
       ["maxLength"]
       ["multipleOf"]
       ["format"]
       ["enum"]
       ["pattern"]

       ["$ref"]
       ["$schema"]
       ["$id"]
       ["id"]
       ["$anchor"]
       ["$recursiveRef"]
       ["$recursiveAnchor"]
       ["$dynamicRef"]
       ["$dynamicAnchor"]
       ["$vocabulary"]
       ["title"]
       ["description"]
       ["default"]
       ["examples"]
       ["deprecated"]
       ["readOnly"]
       ["writeOnly"]
       ["$comment"]
       ["definitions"]
       ["$defs"]

       ["propertyNames"]
       ["propertyDependencies"]

       ["minProperties"]
       ["maxProperties"]

       ["not"]
       ["anyOf"]
       ["oneOf"]
       ["allOf"]

       ["required"]
       ["dependentRequired"]

       ["dependencies"]
       ["dependentSchemas"]
       ["dependentRequired"]

       ["contains"]
       ["minContains"]
       ["maxContains"]
       
       ["minimum"]
       ["exclusiveMinimum"]
       
       ["maximum"]
       ["exclusiveMaximum"]

       ["contentEncoding"]
       ["contentMediaType"]
       ["contentSchema"] ;; TODO: unpack

       ["if"]
       ["then"]
       ["else"]

       ["minItems"]
       ["maxItems"]
       ["uniqueItems"]
       ["prefixItems"]
       ["items"]
       ["additionalItems"]
       ["unevaluatedItems"]

       ["properties"]
       ["patternProperties"]
       ["additionalProperties"]
       ["unevaluatedProperties"]
       ]
      property->ranks-and-group
      (into {} (mapcat (fn [i ps] (map-indexed (fn [j p] [p [[i j] ps]]) ps)) (range) property-groups))]

  (defn compile-m2 [m2]
    (mapv
     second
     (sort-by
      first
      (reduce
       (fn [acc [k v]]
         (let [[[r1 r2] g] (property->ranks-and-group k [[0 0] [k]])]
           (update
            acc
            r1
            (fnil (fn [[k v] & args] [k (apply assoc v args)]) [g (vec (repeat (count g) :absent))])
            r2
            v)))
       {}
       m2)))))

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
    ("draft3" "draft4") (get m "id")
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
    (let [p2-and-cps
          (mapv
           (fn [[ks vs]]
             (let [ks (if (= 1 (count ks)) (first ks) ks) ;; hack - lose later
                   new-p2 (conj p2 ks)]
               [new-p2 (check-property ks c2 new-p2 m2 vs)]))
           (compile-m2 m2))]
      (fn [c1 p1 m1]
        (if (present? m1)
          (let [[new-c1 es]
                (reduce
                 (fn [[old-c1 acc] [new-p2 cp]]
                   (let [[new-c1 [{m :message} :as es]] (cp old-c1 p1 m1)]
                     (when t? (println (pr-str new-p2) (pr-str p1) (if (seq es) ["❌" m] "✅")))
                     [new-c1 (concatv acc es)]))
                 [c1 []]
                 p2-and-cps)]
            [new-c1 ;; (first (stash new-c1 {} m1 p1))
             (make-error-on-failure "schema: document did not conform" p2 m2 p1 m1 es)])
          [c1 []])))))

;; quicker than actual 'apply' [?]
(defn apply3 [f [c p m]]
  (f c p m))

;; At the moment a validation is only a reduction of c1, not c2.
;; Since a draft switch is something that happens at c2-level, I think we need an interceptor...
;; if m2-fn returned [new-c2 m2] perhaps we would not need interceptors !
;; but we would have to thread m2 as well - can we do it - consider...

(defn make-draft-interceptor []
  (fn [delegate]
    (fn [c2 p2 {s "$schema" :as m2}]
      (delegate 
       (if-let [d (and s ($schema->draft s))]
         (update
          c2
          :draft
          (fn [old-d new-d]
            (when (not= old-d new-d)
              (log/info (str "switching draft: " old-d " -> "  new-d)))
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
        (if-let [a (get old-m2 k)]      ;; TODO - how can m2 be nil ?
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
(defn uri->schema [uri-base->dir c p {origin :origin path :path :as url}]
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

(defn make-context [{draft :draft u->s :uri->schema :as c2} {s "$schema" :as m2}]
  (let [draft (or draft
                  (when s ($schema-uri->draft (uri-base (parse-uri s))))
                  "latest")
        id-key (if (#{"draft3" "draft4"} draft) "id" "$id")
        sid (get m2 id-key)
        c2 (if-not u->s (assoc c2 :uri->schema (uri->continuation uri-base->dir)) c2) ;; TODO
        c2 (assoc c2 :draft draft)
        c2 (assoc c2 :id-key id-key)
        c2 (assoc c2 :uri->path (if sid {(parse-uri sid) []} {}))
        c2 (json-walk stash c2 {} [] m2)]
    (assoc
     c2
     :id-uri (or (:id-uri c2) (when sid (parse-uri sid))) ;; should be receiver uri - but seems to default to id/$id - yeugh
     :original-root m2
     :recursive-anchor []
     :root m2
     :strict-format? (let [f? (get c2 :strict-format?)] (if (nil? f?) true f?)) ;; pull this out into some default fn
     :strict-integer? (let [f? (get c2 :strict-integer?)] (if (nil? f?) false f?)) ;; pull this out into some default fn
     )))  

;; TODO: rename :root to ?:expanded?
(defn validate-2 [c2 schema]
  (let [{draft :draft id-key :id-key :as c2} (make-context c2 schema)
        sid (get schema id-key)
        cs (check-schema c2 [] schema)]
    (fn [c1 {did id-key _dsid "$schema" :as document}]
      ;;(log/info "validate:" sid "/" did)
      ;;(when (and dsid (not (= sid dsid))) (log/warn (format "document schema id not consistent with schema id: %s != %s" dsid sid)))
      (let [c1 (assoc c1 :id-key id-key  :uri->path {}) ;; docs must be of same draft as their schemas... ?
            c1 (json-walk stash c1 {} [] schema)
            c1 (assoc
                c1
                :id-key id-key
                :id-uri (when did (parse-uri did))
                :original-root document
                :recursive-anchor []
                :root document
                :draft draft
                :melder (:melder c2))
            [c1 es] (cs c1 [] document)]
        ;;(prn "C:" (:evaluated c1))
        {:valid? (empty? es) :errors es}))))
  
(defn $schema->m2 [s]
  (uri->schema-2 {} [] (parse-uri s)))

(declare validate-m2)

(defn validate-m2-2 [{draft :draft :as c2} m1]
  (let [s (or (and (json-object? m1) (m1 "$schema")) (draft->$schema draft))]
    (if-let [m2 ($schema->m2 s)]
      (if (= m2 m1)
        ;; we are at the top
        (let [v (validate-2 c2 m2)
              ;; self-validate - we need lower level api that returns c1 with marker stash...
              {es :errors :as r} (v {} m1)]
          (if (empty? es)
            v
            (constantly r)))
        ;; keep going
        (do
          ((validate-m2 c2 m2) {} m1)
          (validate-2 c2 m1)))
      (do
        (log/warn "unrecognised schema encountered:" s)
        (constantly [{} {:errors (str "could not resolve $schema: " s)}])))))

(def validate-m2 (memoize validate-m2-2))

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
   ;;(prn "HERE:" m2 m1)
   ((validate-m2 c2 m2) c1 m1)))
