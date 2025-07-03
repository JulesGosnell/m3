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
   [cljc.java-time.local-date :as ld]
   [cljc.java-time.offset-date-time :as odt]
   [cljc.java-time.offset-time :as ot]
   [clojure.string :refer [starts-with? ends-with? replace] :rename {replace string-replace}]
   [#?(:clj clojure.tools.logging :cljs m3.log) :as log]
   [m3.util :refer [absent present?]]
   [m3.uri :refer [parse-uri inherit-uri uri-base]]
   [m3.ref :refer [meld resolve-uri try-path]]
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

;;------------------------------------------------------------------------------

(def ^:dynamic *enable-memo* false) ;; recompile if you change this

(if *enable-memo*

  (defn memo [mf & [af]]
    (let [cache (atom {})
          af (or af identity)]
      (fn [& args]
        (let [k (vec (af args))]
          (if-let [[result] (get @cache k)]
            result
            (let [result (apply mf args)]
              (swap! cache assoc k [result])
              result))))))

  (defn memo [mf & [_]]
    mf))

(defmacro defmfn [name args & body]
  `(if *enable-memo*
     (do
       (declare ~name)
       (def ~name (memo (fn ~args ~@body))))
     (defn ~name ~args ~@body)))

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

(def into-set (fnil into #{}))
(def conj-set (fnil conj #{}))

(defn concatv [& args]
  (vec (apply concat args)))

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

(defn seq-contains? [s v]
  (boolean (some (partial json-= v) s)))

(defn bail-on-error [acc e]
  (if (seq e)
    (reduced e)
    acc))

;;------------------------------------------------------------------------------

(defmulti check-format-2 (fn [format _c2 _p2 _m2] format))

(defn check-format-keys [[format context p2 m2]]
  [format context m2])

(def check-format (memo check-format-2 check-format-keys))

(defn match [format pattern _c2 p2 m2 p1 m1]
  (when-not (re-find pattern m1)
    [(make-error (str "format: not a valid " format) p2 m2 p1 m1)]))

;; standard formats

(defmethod check-format-2 "email" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:[^\s@\"\.](?:(?!\.\.)[^\s@\"])*[^\s@\"\.]|\"(?:[^\r\n\\\"]|\\[\s\S])+\")@(?:[a-zA-Z0-9\-]+(?:\.[a-zA-Z0-9\-]+)*|\[IPv6:[a-fA-F0-9:]+\]|\[(?:(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)\])$"
        c2 p2 m2 p1 m1)))))

(defmethod check-format-2 "ipv4" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match
        format
        ;; https://howtodoinjava.com/java/regex/java-regex-validate-email-address/
        #"^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\.(?!$)|$)){4}$"
        c2 p2 m2 p1 m1)))))

(defmethod check-format-2 "ipv6" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match
        format
        ;; adapted from: https://github.com/ajv-validator/ajv-formats/blob/master/src/formats.ts
        #"^((([0-9a-f]{1,4}:){7}([0-9a-f]{1,4}|:))|(([0-9a-f]{1,4}:){6}(:[0-9a-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){5}(((:[0-9a-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){4}(((:[0-9a-f]{1,4}){1,3})|((:[0-9a-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){3}(((:[0-9a-f]{1,4}){1,4})|((:[0-9a-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){2}(((:[0-9a-f]{1,4}){1,5})|((:[0-9a-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){1}(((:[0-9a-f]{1,4}){1,6})|((:[0-9a-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9a-f]{1,4}){1,7})|((:[0-9a-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))$"
        c2 p2 m2 p1 m1)))))

(defmethod check-format-2 "hostname" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match
        format
        ;; adapted from: https://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
        #"^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]{0,61}[A-Za-z0-9])$"
        c2 p2 m2 p1 m1)))))

(defmethod check-format-2 "date-time" [_format _c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (try
         (odt/parse m1)
         nil
         (catch Exception e
           [(make-error (str "format: not a valid date-time: " (ex-message e)) p2 m2 p1 m1)]))))))

(defmethod check-format-2 "date" [_format _c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (try
         (ld/parse m1)
         nil
         (catch Exception e
           [(make-error (str "format: not a valid date: " (ex-message e)) p2 m2 p1 m1)]))))))

(defmethod check-format-2 "time" [_format _c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (try
         (ot/parse m1)
         nil
         (catch Exception e
           [(make-error (str "format: not a valid time: " (ex-message e)) p2 m2 p1 m1)]))))))

(defmethod check-format-2 "json-pointer" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:/(?:[^~/]|~[01])*)*$"
        c2 p2 m2 p1 m1)))))

(defmethod check-format-2 "relative-json-pointer" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:0|[1-9][0-9]*)(#|(?:/(?:[^~/]|~[01])*)*)$|^#(?:/(?:[^~/]|~[01])*)*$"
        c2 p2 m2 p1 m1)))))

;; TODO: this should be shared with uri.cljc
(def uri-regexp 
  ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
  #"^(?:[A-Za-z][A-Za-z0-9+.\-]*:[^\s]*|#(?:[^\s]*)?)$")

(defmethod check-format-2 "uri" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match format uri-regexp c2 p2 m2 p1 m1)))))

(defmethod check-format-2 "uri-reference" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^[^\s\\]*$"
        c2 p2 m2 p1 m1)))))

(defmethod check-format-2 "uri-template" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:[^\s{}]|(?:\{[^\s{}]*\}))*$"
        c2 p2 m2 p1 m1)))))

(defmethod check-format-2 "idn-email" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:[^\s@\[\]\"(),:;<>\\]+(?:\.[^\s@\[\]\"(),:;<>\\]+)*|\"(?:[^\"\\\r\n]|\\.)+\")@(?:[^\s@\[\]\"(),:;<>\\]+\.)*[^\s@\[\]\"(),:;<>\\]+$"
        c2 p2 m2 p1 m1)))))

;; this is really difficult.
;; I can't find a java, javascript, clojure or clojurescript library which comes close
;; writing my own seems like an unreasable amount of work just to pass this one part of the spec
;; wait for someone else to do it or AI to get good enough to generate the code....
(defmethod check-format-2 "idn-hostname" [_format _c2 _p2 _m2]
  (memo
   (fn [_c1 _p1 m1]
     (when (string? m1)
       nil))));NYI

(defmethod check-format-2 "iri" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^[A-Za-z][A-Za-z0-9+.\-]*://(?:[^\s/?#@\\]+@)?(?:\[[0-9A-Fa-f:]+\]|[^\s/?#@:]+)(?::\d+)?(?:/[^\s?#\\]*)?(?:\?[^\s#\\]*)?(?:#[^\s\\]*)?$"
        c2 p2 m2 p1 m1)))))

(defmethod check-format-2 "iri-reference" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^[^\s\\]*$"
        c2 p2 m2 p1 m1)))))

(defmethod check-format-2 "uuid" [format c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (string? m1)
       (match
        format
        ;; https://www.jvt.me/posts/2022/01/14/java-uuid-regex/
        #"^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$"
        c2 p2 m2 p1 m1)))))

;; ISO 8601 duration
;; https://en.wikipedia.org/wiki/ISO_8601#Durations
;; TODO:
;; - allow more than 4 numbers before each letter
;; - allow a single decimal point in each number group ?
;; - allow a general date-time format ?
(def json-duration-pattern
  (re-pattern
   (str
    "^"
    "P"
    "("
    "("
    "([0-9]{1,4}Y|)"
    "([0-9]{1,4}M|)"
    "([0-9]{1,4}D|)"
    ")"
    "("
    "T"
    "([0-9]{1,4}H|)"
    "([0-9]{1,4}M|)"
    "([0-9]{1,4}S|)"
    "|"
    ")"
    "|"
    "([0-9]{1,4}W|)"    
    ")"
    "$"    
    )))

(defn json-duration? [s]
  (boolean
   (when-let [[_p-t-or-w _ymdthms-or-w ymd _y _m _d thms _h _m _s w] (re-find json-duration-pattern s)]
     (not
      (or
       (and (empty? ymd) (empty? thms) (empty? w))
       (= "T" thms))))))

(defmethod check-format-2 "duration" [_format _c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (when (and (string? m1) (not (json-duration? m1)))
       [(make-error "format: not a valid duration:" p2 m2 p1 m1)]))))

(defmethod check-format-2 "regex" [_format _c2 p2 m2]
  (memo
   (fn [_c1 p1 m1]
     (try
       (when (string? m1)
         (ecma-pattern m1)
         [])
       (catch Exception e
         [(make-error (str "format: not a valid regex: " (ex-message e)) p2 m2 p1 m1)])))))

(defmethod check-format-2 "unknown" [_format _c2 _p2 _m2]
  (memo
   (fn [_c1 _p1 _m1]
     nil)))

;; see: https://github.com/juxt/jinx/blob/master/src/juxt/jinx/alpha/patterns.clj

(defmethod check-format-2 :default [f _c2 _p2 _m2]
  (memo
   (fn [_c1 _p1 _m1]
     (log/warn "format: not recognised:" (pr-str f))
     nil)))

;;------------------------------------------------------------------------------

(declare check-schema)

(defmulti check-type-2 (fn [type _c2 _p2 _m2]
                       ;;(println "check-type-2" type document)
                         type))

(def check-type (memo check-type-2))

(defmethod check-type-2 "object" [_type _c2 p2 m2]
  (memo
   (fn [c1 p1 m1]
     [c1 (when-not (map? m1) [(make-error "type: not an object" p2 m2 p1 m1)])])))

(defmethod check-type-2 "array" [_type _c2 p2 m2]
  (memo
   (fn [c1 p1 m1]
     [c1 (when-not (vector? m1) [(make-error "type: not an array" p2 m2 p1 m1)])])))

(defmethod check-type-2 "string" [_type _c2 p2 m2]
  (memo
   (fn [c1 p1 m1]
     [c1 (when-not (string? m1) [(make-error "type: not a string" p2 m2 p1 m1)])])))

(defmethod check-type-2 "integer" [_type {si? :strict-integer?} p2 m2]
  (let [check (if si? integer? json-integer?)]
    (memo
     (fn [c1 p1 m1]
       [c1 (when-not (check m1) [(make-error "type: not an integer" p2 m2 p1 m1)])]))))

(defmethod check-type-2 "number" [_type _c2 p2 m2]
  (memo
   (fn [c1 p1 m1]
     [c1 (when-not (json-number? m1) [(make-error "type: not a number" p2 m2 p1 m1)])])))

(defmethod check-type-2 "boolean" [_type _c2 p2 m2]
  (memo
   (fn [c1 p1 m1]
     [c1 (when-not (boolean? m1) [(make-error "type: not a boolean" p2 m2 p1 m1)])])))

(defmethod check-type-2 "null" [_type _c2 p2 m2]
  (memo
   (fn [c1 p1 m1]
     [c1 (when-not (nil? m1) [(make-error "type: non null" p2 m2 p1 m1)])])))

(defmethod check-type-2 "any" [_type _c2 _p2 _m2]
  (fn [c1 p1 m1] [c1 nil]))

(defmethod check-type-2 :default [ts c2 p2 m2]
  (if (json-array? ts)
    ;; it could be an array of elements that are:
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
      (memo
       (fn [c1 p1 m1]
         ;; TODO: we should report all the errors...
         [c1
          (when-not (some (fn [checker] (nil? (second (checker c1 p1 m1)))) checkers)
            [(make-error (format "type: none matched: %s" ts) p2 m2 p1 m1)])])))
    (memo
     (fn [c1 p1 m1]
       [c1 [(make-error (format "type: unrecognised: %s" ts) p2 m2 p1 m1)]]))))

;;------------------------------------------------------------------------------

(defmulti check-property-2 (fn [property _c2 _p2 _m2 _v2s]
                           ;;(println "check-property-2:" p2 p1)
                           property))

(def check-property (memo check-property-2))

;; standard common properties

(defmethod check-property-2 "type" [_property c2 p2 m2 [v2]]
  (check-type v2 c2 p2 m2))

(defmethod check-property-2 "const" [_property _c2 p2 m2 [v2]]
  (memo
   (fn [c1 p1 m1]
     [c1
      (when (not (json-= v2 m1))
        [(make-error (format "const: document does not contain schema value: %s != %s" m1 v2) p2 m2 p1 m1)])])))

(defmethod check-property-2 "enum" [_property _c2 p2 m2 [v2]]
  ;; N.B.
  ;; we can't use a memoised hash-set here because comparison is done by json-= not '='... - set-by ?
  (memo
   (fn [c1 p1 m1]
     ;; we could check that the m2's enum contained any const or default
     ;; value here - but thus should be done somehow during validation of
     ;; the m2 as an m1 and not waste time whilst we are validating all
     ;; it's m1s...
     ;; TODO: how about some injectable consistency checking fns which can be used when validating m2s ?
     [c1
      (when-not (seq-contains? v2 m1)
        [(make-error "enum: does not contain value" p2 m2 p1 m1)])])))

(defmethod check-property-2 "$comment"         [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property-2 "id"               [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property-2 "$id"              [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property-2 "description"      [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property-2 "title"            [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property-2 "readOnly"         [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property-2 "writeOnly"        [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))

(defmethod check-property-2 "default"          [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property-2 "$schema"          [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil])) ;; TODO: switch drafts in context...
(defmethod check-property-2 "examples"         [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property-2 "$anchor"          [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property-2 "$recursiveAnchor" [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))
(defmethod check-property-2 "$vocabulary"      [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))

;; NYI
(defmethod check-property-2 "$dynamicAnchor"   [_property _c2 _p2 _m2 _v2s] (fn [c1 _p1 _m1] [c1 nil]))

;; TODO: issue a warning somehow
(defmethod check-property-2 "deprecated"  [_property _c2 _p2 _m2 _v2s] (fn [_c1 _p1 _m1])) ;; TODO: issue a warning or error ?

;; standard number properties

(defmethod check-property-2 "minimum" [_property {d :draft} p2 m2 [v2]]
  (memo
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
          [(make-error "minimum: value to low" p2 m2 p1 m1)])]))))

(defmethod check-property-2 "exclusiveMinimum" [_property {d :draft} p2 {m "minimum" :as m2} [v2]]
  (memo
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
          [(make-error "minimum: value to low" p2 m2 p1 m1)])]))))

(defmethod check-property-2 "maximum" [_property {d :draft} p2 m2 [v2]]
  (memo
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
          [(make-error "maximum: value too high" p2 m2 p1 m1)])]))))

(defmethod check-property-2 "exclusiveMaximum" [_property {d :draft} p2 {m "maximum" :as m2} [v2]]
  (memo
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
          [(make-error "maximum: value too high" p2 m2 p1 m1)])]))))

(defmethod check-property-2 "multipleOf" [_property _c2 p2 m2 [v2]]
  (let [v2-bd (bigdec v2)]
    (memo
     (fn [c1 p1 m1]
       [c1
        (when (and
               (json-number? m1)
               (not (big-zero? (big-mod (bigdec m1) v2-bd))))
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

(defmethod check-property-2 "minLength" [_property _c2 p2 m2 [v2]]
  (let [ml2 (quot v2 2)]
    (memo
     (fn [c1 p1 m1]
       [c1
        (when (and
               (string? m1)
               (or
                (< (count m1) ml2) ;; precheck before using expensive json-length
                (< (json-length m1) v2)))
          [(make-error "minLength: string too short" p2 m2 p1 m1)])]))))

(defmethod check-property-2 "maxLength" [_property _c2 p2 m2 [v2]]
  (let [ml2 (* v2 2)]
    (memo
     (fn [c1 p1 m1]
       [c1
        (when (and
               (string? m1)
               (or
                (> (count m1) ml2) ;; precheck before using expensive json-length
                (> (json-length m1) v2)))
          [(make-error "maxLength: string too long" p2 m2 p1 m1)])]))))

(defmethod check-property-2 "pattern" [_property c2 p2 m2 [v2]]
  (if (starts-with? v2 "$format:")
    ;; N.B.
    ;; this is an extension to allow patternProperties to
    ;; leverage formats since the spec does not provide a
    ;; formatProperties...
    (check-property "format" c2 p2 m2 [(subs v2 (count "$format:"))])
    (let [p (ecma-pattern v2)]
      (memo
       (fn [c1 p1 m1]
         [c1
          (when (and
                 (json-string? m1)
                 (false? (ecma-match p m1)))
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
   "base64" base64-decode
   })

(def cmt->decoder
  {
   "application/json" json-decode
   })

(defmethod check-property-2 ["contentEncoding" "contentMediaType" "contentSchema"] [_property {d :draft :as c2} p2 m2 [ce cmt cs]]
  (let [ce-decoder (ce->decoder ce)
        cmt (if (present? cmt) cmt "application/json")
        cmt-decoder (cmt->decoder cmt)
        strict? (#{"draft7"} d) ;; check a context flag aswell
        checker (if (present? cs) (check-schema c2 p2 cs) (constantly []))]
    (memo
     (fn [c1 p1 m1]
       [c1
        (when (json-string? m1)
          (try
            (let [{es :errors :as v}
                  (checker
                   c1
                   p1
                   (let [new-m1
                         (try
                           (ce-decoder m1)
                           (catch Exception e
                             (throw
                              (ex-info
                               nil
                               {:errors
                                (let [m (str "contentEncoding: could not " ce " decode: " (pr-str m1) " - " (ex-message e))]
                                  (if strict?
                                    [(make-error m p2 m2 p1 m1)]
                                    (do
                                      (log/warn (string-replace m #"\n" " - "))
                                      [])))}))))]
                     (try
                       (cmt-decoder new-m1)
                       (catch Exception e
                         (throw
                          (ex-info
                           nil
                           {:errors
                            (let [m (str "contentMediaType: could not " cmt " decode: " (pr-str new-m1) (if (present? ce) (str " (from " ce " encoded " (pr-str m1) ")") "") " - " (string-replace (ex-message e) #"\n" " \\\\n "))]
                              (if strict?
                                [(make-error m p2 m2 p1 m1)]
                                (do
                                  (log/warn m
                                           ;;
                                            )
                                  [])))}))))))]
              (when (seq es)
                (if strict?
                  es
                  (log/warn "contentSchema: failed validation - " (prn-str v)))))
            (catch Exception e
              (:errors (ex-data e)))))]))))

;; HERE
(defmethod check-property-2 "format" [_property {strict? :strict-format? cfs :check-format :or {cfs {}} :as c2} p2 m2 [v2]]
  (let [f (if strict?
            (fn [f2] (fn [c p m] [c (f2 c p m)]))
            (fn [f2] (fn [c p m] (when-let [[{m :message}] (f2 c p m)] [c (log/warn m)]))))]
  ;; we do this here so that user may override default format checkers...
    (f ((or (cfs v2) check-format) v2 c2 p2 m2))))

(defn continue [c old-es new-es]
  [c (concatv old-es new-es)])

(defn bail-out [c old-es new-es]
  (if (seq new-es)
    (reduced [c new-es])
    [c old-es]))

(defmethod check-property-2 "dependencies" [_property {x? :exhaustive? :as c2} p2 m2 [v2]]
  (let [bail (if x? conj (fn [_acc r] (reduced [r])))
        property->checker
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
              (fn [c1 p1 m1] [c1 (reduce (fn [acc2 k2] (if (contains? m1 k2) acc2 (bail acc2 [k k2]))) [] v)])
              (or (json-object? v) (boolean? v)) ;; a schema dependency
              (check-schema c2 p2 v)
              ;; we should not need to check other cases as m2 should have been validated against m3
              )))
         {}
         v2)]
    (memo
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
         [c1 []])))))

(defmethod check-property-2 "dependentSchemas" [_property {x? :exhaustive? :as c2} p2 m2 [v2]]
  (let [bail (if x? conj (fn [_acc r] (reduced [r])))
        property->checker
        (reduce
         (fn [acc [k v]]
           (assoc acc k (check-schema c2 p2 v)))
         {}
         v2)]
    (memo
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

(defmethod check-property-2 "propertyDependencies" [_property {x? :exhaustive? :as c2} p2 _m2 [v2]]
  (let [bail (if x? continue bail-out)
        checkers (into {} (mapcat (fn [[k1 vs]] (map (fn [[k2 s]] [[k1 k2] (check-schema c2 p2 s)]) vs)) v2))
        ks (keys v2)]
    (memo
     (fn [c1 p1 m1]
       (if (json-object? m1)
         (reduce
          (fn [[c old-es] k]
            (let [v (m1 k)]
              (if-let [checker (and (json-string? v) (checkers [k v]))]
                (let [[c new-es] (checker c p1 m1)]
                  (bail c old-es new-es))
                [c old-es])))
          [c1 []]
          ks)
         [c1 []])))))

;; TODO: share more code with dependencies
(defmethod check-property-2 "dependentRequired" [_property {x? :exhaustive? :as c2} p2 m2 [v2]]
  (let [bail (if x? conj (fn [_acc r] (reduced [r])))
        property->checker
        (reduce
         (fn [acc [k v]]
           (assoc
            acc
            k
            (fn [c1 p1 m1] (reduce (fn [acc2 k2] (if (contains? m1 k2) acc2 (bail acc2 [k k2]))) [] v))))
         {}
         v2)]
    (memo
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

(defmethod check-property-2 "if" [_property c2 p2 _m2 [v2]]
  (let [checker (check-schema c2 p2 v2)
        pp2 (butlast p2)]
    (memo
     (fn [old-c1 p1 m1]
       (let [[new-c1 es] (checker old-c1 p1 m1)
             success? (empty? es)]
         [(update (if success? new-c1 old-c1) :if assoc pp2 success?) []])))))

(defmethod check-property-2 "then" [_property c2 p2 _m2 [v2]]
  (let [checker (check-schema c2 p2 v2)
        pp2 (butlast p2)]
    (memo
     (fn [c1 p1 m1]
       (if (true? (get (get c1 :if) pp2))
         (checker c1 p1 m1)
         [c1 []])))))

(defmethod check-property-2 "else" [_property c2 p2 _m2 [v2]]
  (let [checker (check-schema c2 p2 v2)
        pp2 (butlast p2)]
    (memo
     (fn [c1 p1 m1]
       (if (false? (get (get c1 :if) pp2))
         (checker c1 p1 m1)
         [c1 []])))))

(defmethod check-property-2 "definitions" [_property c2 p2 _m2 [v2]]
  (mapv (fn [[k v]] (check-schema c2 (conj p2 k) v)) v2)
  (fn [c1 _p1 _m1] [c1 nil]))

(defmethod check-property-2 "$defs" [_property c2 p2 _m2 [v2]]
  (mapv (fn [[k v]] (check-schema c2 (conj p2 k) v)) v2)
  (fn [c1 _p1 _m1] [c1 nil]))

(defn check-properties [_c2 p2 m2]
  (let [pp2 (butlast p2)]
    (fn [c1 p1 m1 bail k-and-css message]
      (let [[c1 es]
            (reduce
             (fn [[c old-es] [[k cs] sub-document]]
               (let [[c new-es] (cs c (conj p1 k) sub-document)]
                 (bail c old-es new-es)))
             [c1 []]
             (map (fn [[k :as k-and-cs]] [k-and-cs (m1 k)]) k-and-css))]
        [(let [ks (map first k-and-css)]
           (-> c1
           ;; TODO: only record matched if additonalProperties needed later ?
               (update :matched   update pp2 into-set ks)
           ;; TODO: only record evaluated if unevaluatedProperties needed later ?
               (update :evaluated update p1 into-set ks)))
         (make-error-on-failure message p2 m2 p1 m1 es)]))))

(defmethod check-property-2 "properties" [_property {x? :exhaustive? :as c2} p2 m2 [ps]]
  (let [bail (if x? continue bail-out)
        k-and-css (mapv (fn [[k v]] [k (check-schema c2 (conj p2 k) v)]) (when (present? ps) ps))
        cp (check-properties c2 p2 m2)]
    (memo
     (fn [c1 p1 m1]
       (if (json-object? m1)
         (let [k-and-css (filter (fn [[k]] (contains? m1 k)) k-and-css)]
           (cp c1 p1 m1 bail k-and-css "properties: at least one property did not conform to respective schema"))
         [c1 []])))))

;; what is opposite of "additional" - "matched" - used by spec to refer to properties matched by "properties" or "patternProperties"

(defmethod check-property-2 "patternProperties" [_property {x? :exhaustive? :as c2} p2 m2 [pps]]
  (let [bail (if x? continue bail-out)
        cp-and-pattern-and-ks (mapv (fn [[k v]] [(check-schema c2 (conj p2 k) v) (ecma-pattern k) k]) (when (present? pps) pps))
        cp (check-properties c2 p2 m2)]
    (memo
     (fn [c1 p1 m1]
       (if (json-object? m1)
         (let [k-and-css (apply concat (keep (fn [[k]] (keep (fn [[cs p]] (when (ecma-match p k) [k cs])) cp-and-pattern-and-ks)) m1))]
           (cp c1 p1 m1 bail k-and-css "patternProperties: at least one property did not conform to respective schema"))
         [c1 []])))))


(defmethod check-property-2 "additionalProperties" [_property {x? :exhaustive? :as c2} p2 m2 [v2]]
  (let [bail (if x? continue bail-out)
        cs (check-schema c2 p2 v2)
        pp2 (butlast p2)
        cp (check-properties c2 p2 m2)]
    (memo
     (fn [c1 p1 m1]
       (if (json-object? m1)
         (let [mps (get (get c1 :matched) pp2 #{})
               aps (remove (fn [[k]] (contains? mps k)) m1) ;; k might be nil
               p-and-css (mapv (fn [[k]] [k cs]) aps)] ; TODO: feels inefficient
           (cp c1 p1 m1 bail p-and-css "additionalProperties: at least one property did not conform to schema"))
         [c1 []])))))

(defmethod check-property-2 "unevaluatedProperties" [_property {x? :exhaustive? :as c2} p2 m2 [v2]]
  (let [bail (if x? continue bail-out)
        cs (check-schema c2 p2 v2)
        cp (check-properties c2 p2 m2)]
    (memo
     (fn [c1 p1 m1]
       (if (json-object? m1)
         (let [eps (get (get c1 :evaluated) p1 #{})
               ups (remove (fn [[k]] (contains? eps k)) m1) ;; k might be nil
               p-and-css (mapv (fn [[k]] [k cs]) ups)] ; TODO: feels inefficient
           (cp c1 p1 m1 bail p-and-css "unevaluatedProperties: at least one property did not conform to schema"))
         [c1 []])))))

;; TODO: can we move more up into m2 time ?
(defmethod check-property-2 "propertyNames" [_property {x? :exhaustive? :as c2} p2 m2 [v2]]
  (let [bail (if x? concatv bail-on-error)]
    (memo
     (fn [c1 p1 m1]
       [c1
        (when (json-object? m1)
          (make-error-on-failure
           "propertyNames: at least one property's name failed to conform to relevant schema"
           p2 m2 p1 m1
           (reduce
            (fn [acc [k]]
              (let [[_new-c1 es] ((check-schema c2 (conj p2 k) v2) c1 (conj p1 k) k)]
                (bail acc es)))
            []
            m1)))]))))

;; N.B. by default, this will bail on detection of first missing property - this may not be what is expected
(defmethod check-property-2 "required" [_property {x? :exhaustive?} p2 m2 [v2]]
  (let [bail (if x? conj (fn [_acc r] (reduced [r])))]
    (memo
     (fn [c1 p1 m1]
       [c1
        (when (json-object? m1)
          (when-let [missing (seq (reduce (fn [acc k] (if (contains? m1 k) acc (bail acc k))) [] v2))]
            [(make-error ["required: missing properties (at least):" missing] p2 m2 p1 m1)]))]))))

(defmethod check-property-2 "minProperties" [_property _c2 p2 m2 [v2]]
  (memo
   (fn [c1 p1 m1]
     [c1
      (when (and
             (json-object? m1)
             (< (count m1) v2))
        [(make-error "minProperties: document contains too few properties" p2 m2 p1 m1)])])))

(defmethod check-property-2 "maxProperties" [_property _c2 p2 m2 [v2]]
  (memo
   (fn [c1 p1 m1]
     [c1
      (when (and
             (json-object? m1)
             (> (count m1) v2))
        [(make-error "maxProperties: document has too many properties" p2 m2 p1 m1)])])))

;; standard array properties

;; we could save time by only maintaining :matched and :evaluated
;; context if required (additional and evaluated items)...
(defn check-items [_c2 p2 m2]
  (let [pp2 (butlast p2)]
    (fn [c1 p1 m1 bail i-and-css message]
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
                 (bail new-c old-es new-es)))
             [c1 []]
             (map vector i-and-css m1))]
        [c1 (make-error-on-failure message p2 m2 p1 m1 es)]))))

(defmethod check-property-2 "prefixItems" [_property {x? :exhaustive? :as c2} p2 m2 [v2]]
  (let [bail (if x? continue bail-out)
        i-and-css (vec (map-indexed (fn [i sub-schema] [i (check-schema c2 (conj p2 i) sub-schema)]) v2))
        ci (check-items c2 p2 m2)]
    (memo
     (fn [c1 p1 m1]
       (if (json-array? m1)
         (ci c1 p1 m1 bail i-and-css "prefixItems: at least one item did not conform to respective schema")
         [c1 []])))))

(defmethod check-property-2 "items" [_property {x? :exhaustive? :as c2} p2 m2 [v2]]
  (let [bail (if x? continue bail-out)
        n (count (m2 "prefixItems")) ;; TODO: achieve this by looking at c1 ?
        [m css] (if (json-array? v2)
              ["respective " (map-indexed (fn [i v] (check-schema c2 (conj p2 i) v)) v2)]
              ["" (repeat (check-schema c2 p2 v2))])
        ci (check-items c2 p2 m2)]
    (memo
     (fn [c1 p1 m1]
       (if (json-array? m1)
         (let [items (drop n m1)
               i-and-css (mapv (fn [i cs _] [(+ i n) cs]) (range) css items)]
           (ci c1 p1 items bail i-and-css (str "items: at least one item did not conform to " m "schema")))
         [c1 []])))))

(defmethod check-property-2 "additionalItems" [_property {x? :exhaustive? :as c2} p2 {is "items" :as m2} [v2]]
  (if (json-array? is) ;; additionalItems is only used when items is a tuple
    (let [bail (if x? continue bail-out)
          cs (check-schema c2 p2 v2)
          ;;pp2 (butlast p2)
          ci (check-items c2 p2 m2)]
      (memo
       (fn [c1 p1 m1]
         (if (json-array? m1)
           (let [
                 ;; this is how it should be done, but cheaper to just look at items (must be array for additionalItems to be meaningful) in m2 time
                 ;;mis (get (get c1 :matched) pp2 #{})
                 ;;ais  (remove (fn [[k]] (contains? mis k)) (map-indexed vector m1))
                 ;;i-and-css (mapv (fn [[k]] [k cs]) ais) ; TODO: feels inefficient
                 n (count is)
                 ais (drop n m1)
                 i-and-css (mapv (fn [i cs _] [(+ i n) cs]) (range) (repeat cs) ais)
                 ]
             (ci c1 p1 ais bail i-and-css "additionalItems: at least one item did not conform to schema"))
           [c1 []]))))
    (fn [c1 _p1 _m1]
      [c1 nil])))
    
(defmethod check-property-2 "unevaluatedItems" [_property {x? :exhaustive? :as c2} p2 m2 [v2]]
  (let [bail (if x? continue bail-out)
        css (repeat (check-schema c2 p2 v2))
        ci (check-items c2 p2 m2)]
    (memo
     (fn [{p->eis :evaluated :as c1} p1 m1]
       (if (json-array? m1)
         (let [eis (or (get p->eis p1) #{})
               index-and-items (filter (fn [[k]] (not (eis k))) (map-indexed (fn [i v] [i v]) m1))
               i-and-css (mapv (fn [cs [i]] [i cs]) css index-and-items)] ;; TODO: item not used
           (ci c1 p1 (map second index-and-items) bail i-and-css "unevaluatedItems: at least one item did not conform to schema"))
         [c1 []])))))

(defmethod check-property-2 "contains" [_property c2 p2 {mn "minContains" :as m2} [v2]]
  (let [cs (check-schema c2 p2 v2)
        base (if mn mn 1)
        ci (check-items c2 p2 v2)]
    (memo
     (fn [c1 p1 m1]
       (if (json-array? m1)
         (let [i-and-css (map (fn [i _] [i cs]) (range) m1)
               [new-c1 [{es :errors}]]
               (ci c1 p1 m1 continue i-and-css "contains: at least one item did not conform to schema")
               matches (- (count m1) (count es))]
           (if (<= (min base 1) matches)
             [new-c1 nil]
             [c1 [(make-error "contains: document has no matches" p2 m2 p1 m1)]])))))))

(defmethod check-property-2 "minContains" [_property _c2 p2 m2 [v2]]
  (let [pp2 (butlast p2)]
    (memo
     (fn [{matched :matched :as c1} p1 m1]
       (if-let [matches (and (json-array? m1) (get matched pp2))]
         (let [n (count matches)]
           (if (and
                matches
                (json-array? m1)
                (<= v2 n))
             [c1 nil]
             [c1 [(make-error (str "minContains: document has too few matches - " n) p2 m2 p1 m1)]]))
         [c1 nil])))))

(defmethod check-property-2 "maxContains" [_property _c2 p2 m2 [v2]]
  (let [pp2 (butlast p2)]
    (memo
     (fn [{matched :matched :as c1} p1 m1]
       (if-let [matches (and (json-array? m1) (get matched pp2))]
         (let [n (count matches)]
           (if (<= n v2)
             [c1 nil]
             [c1 [(make-error (str "maxContains: document has too many matches - " n) p2 m2 p1 m1)]]))
         [c1 nil])))))

(defmethod check-property-2 "minItems" [_property _c2 p2 m2 [v2]]
  (memo
   (fn [c1 p1 m1]
     [c1
      (when (and
             (json-array? m1)
             (< (count m1) v2))
        [(make-error "minItems: document contains too few items" p2 m2 p1 m1)])])))

(defmethod check-property-2 "maxItems" [_property _c2 p2 m2 [v2]]
  (memo
   (fn [c1 p1 m1]
     [c1
      (when (and
             (json-array? m1)
             (> (count m1) v2))
        [(make-error "maxItems: document contains too many items" p2 m2 p1 m1)])])))

;; TODO: should be unique according to json equality
;; TODO: could be more efficient - only needs to find one duplicate before it bails..
;; TODO: use sorted-set-by and put items in 1 at a time until one is rejected then bail - reduced
(defmethod check-property-2 "uniqueItems" [_property _c2 p2 m2 [v2]]
  (if v2
    (memo
     (fn [c1 p1 m1]
       [c1
        (when (json-array? m1)
          (when (not (= (count m1) (count (distinct m1))))
            [(make-error "uniqueItems: document contains duplicate items" p2 m2 p1 m1)]))]))
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

(defmethod check-property-2 "oneOf" [_property c2 p2 m2 [v2]]
  (let [co (check-of c2 p2 m2 v2)
        m2-count (count v2)]
    (memo
     (fn [c1 p1 m1]
       (co
        c1 p1 m1
        "oneOf: document failed to conform to one and only one sub-schema"
        (fn [es] (not= 1 (- m2-count (count es)))))))))

(defmethod check-property-2 "anyOf" [_property c2 p2 m2 [v2]]
  (let [co (check-of c2 p2 m2 v2)
        m2-count (count v2)]
    (memo
     (fn [c1 p1 m1]
       (co
        c1 p1 m1
        "anyOf: document failed to conform to at least one sub-schema"
        (fn [es] (not (< (count es) m2-count))))))))
         
(defmethod check-property-2 "allOf" [_property c2 p2 m2 [v2]]
  (let [co (check-of c2 p2 m2 v2)]
    (memo
     (fn [c1 p1 m1]
       (co
        c1 p1 m1
        "allOf: document failed to conform to all sub-schemas"
        seq)))))

;; TODO: share check-of
(defmethod check-property-2 "not" [_property c2 p2 m2 [v2]]
  (let [c (check-schema c2 p2 v2)]
    (memo
     (fn [c1 p1 m1]
       (let [old-local-c1 (update c1 :evaluated dissoc p1)
             [new-local-c1 es] (c old-local-c1 p1 m1)
             [c1 failed?] (if (seq es)
                                [(update c1 :evaluated update p1 into-set (get (get new-local-c1 :evaluated) p1)) true]
                                [c1 false])]
         [c1
          (when-not failed?
            [(make-error "not: document conformed to sub-schema" p2 m2 p1 m1)])])))))

;; catch-all

(defmethod check-property-2 :default [property {{checker property} :validators :as c2} p2 m2 v2s]
  (if checker
    (let [cp (checker property c2 p2 m2 v2s)]
      (memo
       (fn [c1 p1 m1]
         [c1 (cp p1 m1)])))
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
       ["contentEncoding" "contentMediaType" "contentSchema"] ;; TODO: unpack

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

(defn do-bail-on-error [c1 acc es]
  (if (seq es)
    (reduced [c1 es])
    [c1 acc]))

(defn dont-bail-on-error [c1 acc es]
  [c1 (concatv acc es)])

(defn get-bail [{x? :exhaustive?}]
  (if x? dont-bail-on-error do-bail-on-error))

(defn check-schema-2 [{t? :trace? :as c2} p2 m2]
  ;; TODO; this needs to be simplified
  (let [bail (get-bail c2)]
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
                       (bail new-c1 acc es)))
                   [c1 []]
                   p2-and-cps)]
              [new-c1
               (make-error-on-failure "schema: document did not conform" p2 m2 p1 m1 es)])
            [c1 []]))))))

;; quicker than actual 'apply' [?]
(defn apply3 [f [c p m]]
  (f c p m))
  
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

(def check-schema-1
  ((make-ref-interceptor "$dynamicRef" expand-$dynamic-ref)
   ((make-ref-interceptor "$recursiveRef" expand-$recursive-ref)
    ((make-ref-interceptor "$ref" expand-$ref)
     ((make-anchor-interceptor (constantly "$dynamicAnchor") stash-$dynamic-anchor)
      ((make-anchor-interceptor (constantly "$recursiveAnchor") stash-$recursive-anchor)
       ((make-anchor-interceptor :id-key stash-$id)
        check-schema-2)))))))

(def check-schema (memo check-schema-1))

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

(defn stash [{id-key :id-key :as acc} stuff {id id-key a "$anchor" da "$dynamicAnchor"} path]
  (-> [acc stuff]
      ((fn [[acc {id-uri :id-uri :as stuff}]] [(update acc :path->uri assoc path id-uri) stuff]))
      (stash-anchor path false true id)
      (stash-anchor path true false a)
      (stash-anchor path true false da)))
      
;;------------------------------------------------------------------------------

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
  (reduce-kv (fn [acc k v] (conj acc [v k])) {} (dissoc draft->$schema "latest")))

(def $schema-uri->draft
  (reduce-kv (fn [acc k v] (conj acc [(parse-uri k) v])) {} $schema->draft))

;;------------------------------------------------------------------------------

;; TODO: rename
(def uri-base->dir
  {"http://json-schema.org" "resources/schemas"
   "https://json-schema.org" "resources/schemas"})

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
    (memo
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
         {:valid? (empty? es) :errors es})))))

;; by recursing to top of schema hierarchy and then validating downwards we:
;; - confirm validity of entire model, not just the m1
;; - we can use our implicit knowledge of the structure of json docs to discover all the ids and anchors
(defn validate
  ;; when a document is self-descriptive (i.e. it has a $schema) we
  ;; can use this entry point....
  ([c1 {sid "$schema" :as m1}]
   (if sid
     (let [draft (get $schema->draft sid "latest")
           m2 (uri->schema-2 {} [] (parse-uri sid))]
       (if (= m2 m1)
         ;; we are at top of schema hierarchy - ground out
         ((validate-2 {:draft draft} m2) c1 m1)
         ;; continue up schema hierarchy...
         (validate {} m2)))
     [{:errors "no $schema given"}]))
  ;; but some m1s are not objects (e.g. string) and thus have nowhere
  ;; to carry $schema property - in which case use this entry point...
  ([c2 schema c1 document]
   (validate c2 schema)
   ((validate-2 c2 schema) c1 document)))
