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

#?(:cljs (def pmap map))

#?(:cljs (def fs (js/require "fs")))
#?(:cljs (def node-path (js/require "path")))

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

(defmulti check-format-2 (fn [format _m2-ctx _m2-path _m2-doc] format))

(defn check-format-keys [[format context m2-path m2-doc]]
  [format context m2-doc])

(def check-format (memo check-format-2 check-format-keys))

(defn match [format pattern _m2-ctx m2-path m2-doc m1-path m1-doc]
  (when-not (re-find pattern m1-doc)
    [(make-error (str "format: not a valid " format) m2-path m2-doc m1-path m1-doc)]))

;; standard formats

(defmethod check-format-2 "email" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:[^\s@\"\.](?:(?!\.\.)[^\s@\"])*[^\s@\"\.]|\"(?:[^\r\n\\\"]|\\[\s\S])+\")@(?:[a-zA-Z0-9\-]+(?:\.[a-zA-Z0-9\-]+)*|\[IPv6:[a-fA-F0-9:]+\]|\[(?:(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)\])$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "ipv4" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        format
        ;; https://howtodoinjava.com/java/regex/java-regex-validate-email-address/
        #"^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\.(?!$)|$)){4}$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "ipv6" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        format
        ;; adapted from: https://github.com/ajv-validator/ajv-formats/blob/master/src/formats.ts
        #"^((([0-9a-f]{1,4}:){7}([0-9a-f]{1,4}|:))|(([0-9a-f]{1,4}:){6}(:[0-9a-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){5}(((:[0-9a-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){4}(((:[0-9a-f]{1,4}){1,3})|((:[0-9a-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){3}(((:[0-9a-f]{1,4}){1,4})|((:[0-9a-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){2}(((:[0-9a-f]{1,4}){1,5})|((:[0-9a-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){1}(((:[0-9a-f]{1,4}){1,6})|((:[0-9a-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9a-f]{1,4}){1,7})|((:[0-9a-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "hostname" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        format
        ;; adapted from: https://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
        #"^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]{0,61}[A-Za-z0-9])$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "date-time" [_format _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (try
         (odt/parse m1-doc)
         nil
         (catch Exception e
           [(make-error (str "format: not a valid date-time: " (ex-message e)) m2-path m2-doc m1-path m1-doc)]))))))

(defmethod check-format-2 "date" [_format _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (try
         (ld/parse m1-doc)
         nil
         (catch Exception e
           [(make-error (str "format: not a valid date: " (ex-message e)) m2-path m2-doc m1-path m1-doc)]))))))

(defmethod check-format-2 "time" [_format _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (try
         (ot/parse m1-doc)
         nil
         (catch Exception e
           [(make-error (str "format: not a valid time: " (ex-message e)) m2-path m2-doc m1-path m1-doc)]))))))

(defmethod check-format-2 "json-pointer" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:/(?:[^~/]|~[01])*)*$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "relative-json-pointer" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:0|[1-9][0-9]*)(#|(?:/(?:[^~/]|~[01])*)*)$|^#(?:/(?:[^~/]|~[01])*)*$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

;; TODO: this should be shared with uri.cljc
(def uri-regexp 
  ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
  #"^(?:[A-Za-z][A-Za-z0-9+.\-]*:[^\s]*|#(?:[^\s]*)?)$")

(defmethod check-format-2 "uri" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match format uri-regexp m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "uri-reference" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^[^\s\\]*$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "uri-template" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:[^\s{}]|(?:\{[^\s{}]*\}))*$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "idn-email" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:[^\s@\[\]\"(),:;<>\\]+(?:\.[^\s@\[\]\"(),:;<>\\]+)*|\"(?:[^\"\\\r\n]|\\.)+\")@(?:[^\s@\[\]\"(),:;<>\\]+\.)*[^\s@\[\]\"(),:;<>\\]+$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

;; this is really difficult.
;; I can't find a java, javascript, clojure or clojurescript library which comes close
;; writing my own seems like an unreasable amount of work just to pass this one part of the spec
;; wait for someone else to do it or AI to get good enough to generate the code....
(defmethod check-format-2 "idn-hostname" [_format _m2-ctx _m2-path _m2-doc]
  (memo
   (fn [_m1-ctx _m1-path m1-doc]
     (when (string? m1-doc)
       nil))));NYI

(defmethod check-format-2 "iri" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^[A-Za-z][A-Za-z0-9+.\-]*://(?:[^\s/?#@\\]+@)?(?:\[[0-9A-Fa-f:]+\]|[^\s/?#@:]+)(?::\d+)?(?:/[^\s?#\\]*)?(?:\?[^\s#\\]*)?(?:#[^\s\\]*)?$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "iri-reference" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        format
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^[^\s\\]*$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "uuid" [format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        format
        ;; https://www.jvt.me/posts/2022/01/14/java-uuid-regex/
        #"^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

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
   (when-let [[p-t-or-w ymdthms-or-w ymd _y _m _d thms _h _m _s w] (re-find json-duration-pattern s)]
     (not
      (or
       (and (empty? ymd) (empty? thms) (empty? w))
       (= "T" thms))))))

(defmethod check-format-2 "duration" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (and (string? m1-doc) (not (json-duration? m1-doc)))
       [(make-error "format: not a valid duration:" m2-path m2-doc m1-path m1-doc)]))))

(defmethod check-format-2 "regex" [_format _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (try
       (when (string? m1-doc)
         (ecma-pattern m1-doc)
         [])
       (catch Exception e
         [(make-error (str "format: not a valid regex: " (ex-message e)) m2-path m2-doc m1-path m1-doc)])))))

(defmethod check-format-2 "unknown" [_format _m2-ctx _m2-path _m2-doc]
  (memo
   (fn [_m1-ctx _m1-path _m1-doc]
     nil)))

;; see: https://github.com/juxt/jinx/blob/master/src/juxt/jinx/alpha/patterns.clj

(defmethod check-format-2 :default [f _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (log/warn "format: not recognised:" (pr-str f))
     nil)))

;;------------------------------------------------------------------------------

(declare check-schema)

(defmulti check-type-2 (fn [type _m2-ctx _m2-path _m2-doc]
                       ;;(println "check-type-2" type document)
                         type))

(def check-type (memo check-type-2))

(defmethod check-type-2 "object" [_type _m2-ctx m2-path m2-doc]
  (memo
   (fn [m1-ctx m1-path m1-doc]
     [m1-ctx (when-not (map? m1-doc) [(make-error "type: not an object" m2-path m2-doc m1-path m1-doc)])])))

(defmethod check-type-2 "array" [_type _m2-ctx m2-path m2-doc]
  (memo
   (fn [m1-ctx m1-path m1-doc]
     [m1-ctx (when-not (vector? m1-doc) [(make-error "type: not an array" m2-path m2-doc m1-path m1-doc)])])))

(defmethod check-type-2 "string" [_type _m2-ctx m2-path m2-doc]
  (memo
   (fn [m1-ctx m1-path m1-doc]
     [m1-ctx (when-not (string? m1-doc) [(make-error "type: not a string" m2-path m2-doc m1-path m1-doc)])])))

(defmethod check-type-2 "integer" [_type {si? :strict-integer? :as c2} m2-path m2-doc]
  (let [check (if si? integer? json-integer?)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx (when-not (check m1-doc) [(make-error "type: not an integer" m2-path m2-doc m1-path m1-doc)])]))))

(defmethod check-type-2 "number" [_type _m2-ctx m2-path m2-doc]
  (memo
   (fn [m1-ctx m1-path m1-doc]
     [m1-ctx (when-not (json-number? m1-doc) [(make-error "type: not a number" m2-path m2-doc m1-path m1-doc)])])))

(defmethod check-type-2 "boolean" [_type _m2-ctx m2-path m2-doc]
  (memo
   (fn [m1-ctx m1-path m1-doc]
     [m1-ctx (when-not (boolean? m1-doc) [(make-error "type: not a boolean" m2-path m2-doc m1-path m1-doc)])])))

(defmethod check-type-2 "null" [_type _m2-ctx m2-path m2-doc]
  (memo
   (fn [m1-ctx m1-path m1-doc]
     [m1-ctx (when-not (nil? m1-doc) [(make-error "type: non null" m2-path m2-doc m1-path m1-doc)])])))

(defmethod check-type-2 "any" [_type _m2-ctx m2-path m2-doc]
  (fn [m1-ctx m1-path m1-doc] [m1-ctx nil]))

(defmethod check-type-2 :default [ts m2-ctx m2-path m2-doc]
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
                (check-type t m2-ctx (conj m2-path i) t)
                (json-object? t)
                (check-schema m2-ctx (conj m2-path i) t)
                :else
                (throw (ex-info "hmmm" {:types ts :type t}))))
            ts))]
      (memo
       (fn [m1-ctx m1-path m1-doc]
         ;; TODO: we should report all the errors...
         [m1-ctx
          (when-not (some (fn [checker] (nil? (second (checker m1-ctx m1-path m1-doc)))) checkers)
            [(make-error (format "type: none matched: %s" ts) m2-path m2-doc m1-path m1-doc)])])))
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx [(make-error (format "type: unrecognised: %s" ts) m2-path m2-doc m1-path m1-doc)]]))))

;;------------------------------------------------------------------------------

(defmulti check-property-2 (fn [property _m2-ctx _m2-path _m2-doc _m2-vals]
                           ;;(println "check-property-2:" m2-path m1-path)
                           property))

(def check-property (memo check-property-2))

;; standard common properties

(defmethod check-property-2 "type" [_property m2-ctx m2-path m2-doc [m2-val]]
  (check-type m2-val m2-ctx m2-path m2-doc))

(defmethod check-property-2 "const" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (memo
   (fn [m1-ctx m1-path m1-doc]
     [m1-ctx
      (when (not (json-= m2-val m1-doc))
        [(make-error (format "const: document does not contain schema value: %s != %s" m1-doc m2-val) m2-path m2-doc m1-path m1-doc)])])))

(defmethod check-property-2 "enum" [_property _m2-ctx m2-path m2-doc [m2-val]]
  ;; N.B.
  ;; we can't use a memoised hash-set here because comparison is done by json-= not '='... - set-by ?
  (memo
   (fn [m1-ctx m1-path m1-doc]
     ;; we could check that the m2's enum contained any const or default
     ;; value here - but thus should be done somehow during validation of
     ;; the m2 as an m1 and not waste time whilst we are validating all
     ;; it's m1s...
     ;; TODO: how about some injectable consistency checking fns which can be used when validating m2s ?
     [m1-ctx
      (when-not (seq-contains? m2-val m1-doc)
        [(make-error "enum: does not contain value" m2-path m2-doc m1-path m1-doc)])])))

(defmethod check-property-2 "$comment"         [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))
(defmethod check-property-2 "id"               [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))
(defmethod check-property-2 "$id"              [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))
(defmethod check-property-2 "description"      [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))
(defmethod check-property-2 "title"            [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))
(defmethod check-property-2 "readOnly"         [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))
(defmethod check-property-2 "writeOnly"        [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))

(defmethod check-property-2 "default"          [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))
(defmethod check-property-2 "$schema"          [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil])) ;; TODO: switch drafts in context...
(defmethod check-property-2 "examples"         [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))
(defmethod check-property-2 "$anchor"          [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))
(defmethod check-property-2 "$recursiveAnchor" [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))
(defmethod check-property-2 "$vocabulary"      [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))

;; NYI
(defmethod check-property-2 "$dynamicAnchor"   [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))

;; TODO: issue a warning somehow
(defmethod check-property-2 "deprecated"  [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc])) ;; TODO: issue a warning or error ?

;; standard number properties

(defn check-m??imum [p p= e1 e2 e3]
  (fn [{d :draft} m2-path m2-doc [m??imum exclusive?]]
    (if (= "draft4" d)
      ;; exclusiveM??imum is a boolean that modifies m??imum
      (if (present? m??imum)
        (let [[p e]
              (if (and (present? exclusive?) exclusive?)
                [p e1]
                [p= e2])]
          (memoize
           (fn [m1-ctx m1-path m1-doc]
             [m1-ctx
              (when (json-number? m1-doc)
                (when-not (p m1-doc m??imum)
                  [(make-error e m2-path m2-doc m1-path m1-doc)]))])))
        (constantly []))
      ;; m??imum and exclusiveM??imum are both numbers and treated separately
      (let [check-m??imum
            (if (present? m??imum)
              (fn [m1-path m1-doc]
                (fn [acc]
                  (if (p= m1-doc m??imum)
                    acc
                    (conj acc [(make-error e2 m2-path m2-doc m1-path m1-doc)]))))
              (fn [m1-path m1-doc] identity))
            check-exclusive
            (if (present? exclusive?)
              (fn [m1-path m1-doc]
                (fn [acc]
                  (if (p m1-doc exclusive?)
                    acc
                    (conj acc [(make-error e3 m2-path m2-doc m1-path m1-doc)]))))
              (fn [m1-path m1-doc] identity))]
        (memoize
         (fn [m1-ctx m1-path m1-doc]
           [m1-ctx
            (when (json-number? m1-doc)
              (let [cm (check-m??imum m1-path m1-doc)
                    ce (check-exclusive m1-path m1-doc)]
                (-> [] (cm) (ce))))]))))))

(defmethod check-property-2 ["minimum" "exclusiveMinimum"] [_property m2-ctx m2-path m2-doc m2-vals]
  ((check-m??imum
    >
    >=
    "minimum & exclusiveMinimum: value is not above"
    "minimum: value is not equal to or above"
    "exclusiveMinimum: value is not above")
   m2-ctx m2-path m2-doc m2-vals))

;; TODO: optimise
(defmethod check-property-2 ["maximum" "exclusiveMaximum"] [_property m2-ctx m2-path m2-doc m2-vals]
  ((check-m??imum
    <
    <=
    "maximum & exclusiveMaximum: value is not below"
    "maximum: value is not equal to or below"
    "exclusiveMaximum: value is not below")
   m2-ctx m2-path m2-doc m2-vals))

(defmethod check-property-2 "multipleOf" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (let [m2-val-bd (bigdec m2-val)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (and
               (json-number? m1-doc)
               (not (big-zero? (big-mod (bigdec m1-doc) m2-val-bd))))
          [(make-error (format "%s is not a multiple of %s" m1-doc m2-val) m2-path m2-doc m1-path m1-doc)])]))))

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

(defmethod check-property-2 "minLength" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (let [ml2 (quot m2-val 2)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (and
               (string? m1-doc)
               (or
                (< (count m1-doc) ml2) ;; precheck before using expensive json-length
                (< (json-length m1-doc) m2-val)))
          [(make-error "minLength: string too short" m2-path m2-doc m1-path m1-doc)])]))))

(defmethod check-property-2 "maxLength" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (let [ml2 (* m2-val 2)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (and
               (string? m1-doc)
               (or
                (> (count m1-doc) ml2) ;; precheck before using expensive json-length
                (> (json-length m1-doc) m2-val)))
          [(make-error "maxLength: string too long" m2-path m2-doc m1-path m1-doc)])]))))

(defmethod check-property-2 "pattern" [_property m2-ctx m2-path m2-doc [m2-val]]
  (if (starts-with? m2-val "$format:")
    ;; N.B.
    ;; this is an extension to allow patternProperties to
    ;; leverage formats since the spec does not provide a
    ;; formatProperties...
    (check-property "format" m2-ctx m2-path m2-doc [(subs m2-val (count "$format:"))])
    (let [p (ecma-pattern m2-val)]
      (memo
       (fn [m1-ctx m1-path m1-doc]
         [m1-ctx
          (when (and
                 (json-string? m1-doc)
                 (false? (ecma-match p m1-doc)))
            [(make-error "pattern: doesn't match" m2-path m2-doc m1-path m1-doc)])])))))

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

(defmethod check-property-2 ["contentEncoding" "contentMediaType" "contentSchema"] [_property {d :draft :as m2-ctx} m2-path m2-doc [ce cmt cs]]
  (let [ce-decoder (ce->decoder ce)
        cmt (if (present? cmt) cmt "application/json")
        cmt-decoder (cmt->decoder cmt)
        strict? (#{"draft7"} d) ;; check a context flag aswell
        checker (if (present? cs) (check-schema m2-ctx m2-path cs) (constantly []))]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (json-string? m1-doc)
          (try
            (let [{es :errors :as v}
                  (checker
                   m1-ctx
                   m1-path
                   (let [new-m1-doc
                         (try
                           (ce-decoder m1-doc)
                           (catch Exception e
                             (throw
                              (ex-info
                               nil
                               {:errors
                                (let [m (str "contentEncoding: could not " ce " decode: " (pr-str m1-doc) " - " (ex-message e))]
                                  (if strict?
                                    [(make-error m m2-path m2-doc m1-path m1-doc)]
                                    (do
                                      (log/warn (string-replace m #"\n" " - "))
                                      [])))}))))]
                     (try
                       (cmt-decoder new-m1-doc)
                       (catch Exception e
                         (throw
                          (ex-info
                           nil
                           {:errors
                            (let [m (str "contentMediaType: could not " cmt " decode: " (pr-str new-m1-doc) (if (present? ce) (str " (from " ce " encoded " (pr-str m1-doc) ")") "") " - " (string-replace (ex-message e) #"\n" " \\\\n "))]
                              (if strict?
                                [(make-error m m2-path m2-doc m1-path m1-doc)]
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
(defmethod check-property-2 "format" [_property {strict? :strict-format? cfs :check-format :or {cfs {}} :as m2-ctx} m2-path m2-doc [m2-val]]
  (let [f (if strict?
            (fn [f2] (fn [c p m] [c (f2 c p m)]))
            (fn [f2] (fn [c p m] (when-let [[{m :message}] (f2 c p m)] [c (log/warn m)]))))]
  ;; we do this here so that user may override default format checkers...
    (f ((or (cfs m2-val) check-format) m2-val m2-ctx m2-path m2-doc))))

(defmethod check-property-2 "dependencies" [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [m2-val]]
  (let [bail (if x? conj (fn [_acc r] (reduced [r])))
        property->checker
        (reduce
         (fn [acc [k v]]
           (assoc
            acc
            k
            (cond
              (json-string? v) ;; a single property dependency
              (fn [m1-ctx m1-path m1-doc] [m1-ctx (when (not (contains? m1-doc v)) [v v])])
              (json-array? v) ;; a multiple property dependency
              ;; TODO: this looks very suspect
              (fn [m1-ctx m1-path m1-doc] [m1-ctx (reduce (fn [acc2 k2] (if (contains? m1-doc k2) acc2 (bail acc2 [k k2]))) [] v)])
              (or (json-object? v) (boolean? v)) ;; a schema dependency
              (check-schema m2-ctx m2-path v)
              ;; we should not need to check other cases as m2 should have been validated against m3
              )))
         {}
         m2-val)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (json-object? m1-doc)
          (when-let [missing
                     (seq
                      (reduce
                       (fn [acc [k v]]
                         (if (contains? m1-doc k)
                           (concatv acc (second ((property->checker k) m1-ctx m1-path m1-doc)))
                           acc))
                       []
                       m2-val))]
            [(make-error ["dependencies: missing properties (at least):" missing] m2-path m2-doc m1-path m1-doc)]))]))))

(defmethod check-property-2 "dependentSchemas" [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [m2-val]]
  (let [bail (if x? conj (fn [_acc r] (reduced [r])))
        property->checker
        (reduce
         (fn [acc [k v]]
           (assoc acc k (check-schema m2-ctx m2-path v)))
         {}
         m2-val)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (json-object? m1-doc)
          (when-let [missing
                     (seq
                      (reduce
                       (fn [acc [k v]]
                         (if (contains? m1-doc k)
                           (concatv acc (second ((property->checker k) m1-ctx m1-path m1-doc))) ;; TODO: thread c1 through
                           acc))
                       []
                       m2-val))]
            [(make-error ["dependentSchemas: missing properties (at least):" missing] m2-path m2-doc m1-path m1-doc)]))]))))

(defmethod check-property-2 "propertyDependencies" [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [m2-val]]
  (let [bail (if x? concatv bail-on-error)
        checkers (into {} (mapcat (fn [[k1 vs]] (map (fn [[k2 s]] [[k1 k2] (check-schema m2-ctx m2-path s)]) vs)) m2-val))
        ks (keys m2-val)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (json-object? m1-doc)
          (reduce
           (fn [acc k]
             (let [v (m1-doc k)]
               (if-let [checker (and (json-string? v) (checkers [k v]))]
                 (bail acc (second (checker m1-ctx m1-path m1-doc))) ;; TODO: thread c1 through...
                 acc)))
           []
           ks))]))))

;; TODO: share more code with dependencies
(defmethod check-property-2 "dependentRequired" [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [m2-val]]
  (let [bail (if x? conj (fn [_acc r] (reduced [r])))
        property->checker
        (reduce
         (fn [acc [k v]]
           (assoc
            acc
            k
            (fn [m1-ctx m1-path m1-doc] (reduce (fn [acc2 k2] (if (contains? m1-doc k2) acc2 (bail acc2 [k k2]))) [] v))))
         {}
         m2-val)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (json-object? m1-doc)
          (when-let [missing
                     (seq
                      (reduce
                       (fn [acc [k v]]
                         (if (contains? m1-doc k)
                           (concatv acc ((property->checker k) m1-ctx m1-path m1-doc))
                           acc))
                       []
                       m2-val))]
            [(make-error ["dependentRequired: missing properties (at least):" missing] m2-path m2-doc m1-path m1-doc)]))]))))

;;------------------------------------------------------------------------------

;; TODO - return passed m1-ctx
(defn make-checker [m2-ctx m2-path m2-doc]
  (let [checker (check-schema m2-ctx m2-path m2-doc)]
    (fn [m1-ctx m1-path m1-doc]
      (empty? (second (checker m1-ctx m1-path m1-doc))))))

(defmethod check-property-2 ["if" "then" "else"] [_property m2-ctx m2-path _m2-doc [if? then else]]
  (if (present? if?)
    (let [if-checker (check-schema m2-ctx m2-path if?)
          then-checker (if (present? then) (check-schema m2-ctx m2-path then) (fn [c2 _p2 _m2] [c2 []]))
          else-checker (if (present? else) (check-schema m2-ctx m2-path else) (fn [c2 _p2 _m2] [c2 []]))]
      (memo
       (fn [m1-ctx m1-path m1-doc]
         ((if (empty? (second (if-checker m1-ctx m1-path m1-doc))) then-checker else-checker) m1-ctx m1-path m1-doc))))
    (fn [m1-ctx _m1-path _m1-doc]
      [m1-ctx nil])))

;; TODO
;; "unevaluatedItems"
;; - linked to schema composition
;; - see http://json-schema.org/understanding-json-schema/reference/object.html#unevaluated-properties

;; http://json-schema.org/understanding-json-schema/reference/object.html

(defmethod check-property-2 "definitions" [_property m2-ctx m2-path _m2-doc [m2-val]]
  (mapv (fn [[k v]] (check-schema m2-ctx (conj m2-path k) v)) m2-val)
  (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))

(defmethod check-property-2 "$defs" [_property m2-ctx m2-path _m2-doc [m2-val]]
  (mapv (fn [[k v]] (check-schema m2-ctx (conj m2-path k) v)) m2-val)
  (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil]))

;; TODO: handle :default-additional-properties here
(defmethod check-property-2 ["properties" "patternProperties" "additionalProperties" "unevaluatedProperties"] [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [ps pps aps ups]]
  (let [bail (if x? concatv bail-on-error)
        cp-and-ks (mapv (fn [[k v]] [(check-schema m2-ctx (conj m2-path k) v) k]) (when (present? ps) ps))
        named? (if (present? ps) (partial contains? ps) (constantly false))

        cp-and-pattern-and-ks (mapv (fn [[k v]] [(check-schema m2-ctx (conj m2-path k) v) (ecma-pattern k) k]) (when (present? pps) pps))
        patterns (mapv second cp-and-pattern-and-ks)
        pattern? (if (seq patterns) (fn [k] (some (fn [pattern] (ecma-match pattern k)) patterns)) (constantly false))

        [em cs] (cond
                  (present? aps)
                  ["additionalProperties: at least one additional property failed to conform to schema" (check-schema m2-ctx m2-path aps)]
                  (present? ups)
                  ["unevaluatedProperties: at least one unevaluated property failed to conform to schema" (check-schema m2-ctx m2-path ups)]
                  :else
                  [nil (fn [c p m] [c []])])]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (json-object? m1-doc)
          (or
           (make-error-on-failure
            "properties: at least one property failed to conform to the relevant schema"
            m2-path m2-doc m1-path m1-doc
            (reduce
             (fn [acc [cp k]]
               (let [[new-m1-ctx es] (cp m1-ctx (conj m1-path k) (get m1-doc k absent))]
                 (bail acc es)))
             []
             cp-and-ks))
          ;; any property (including "named" properties) present in m1 whose key
          ;; matches a patternProperty key must conform to the corresponding
          ;; schema
           (make-error-on-failure
            "patternProperties: at least one property failed to conform to relevant schema"
            m2-path m2-doc m1-path m1-doc
            (reduce
             (fn [acc1 [cp pattern ps]]
               (reduce
                (fn [acc2 [k v]]
                  (if (ecma-match pattern k)
                    (let [[new-c1-ctx es] (cp m1-ctx (conj m1-path k) v)]
                      (bail acc2 es))
                    acc2))
                acc1
                m1-doc))
             []
             cp-and-pattern-and-ks))
          ;; any property present in the m1 that is not a named or a
          ;; pattern property must conform to the additional
          ;; properties schema
           (when-let [additional (seq (remove (fn [[k _v]] (or (named? k) (pattern? k))) m1-doc))]
             (make-error-on-failure
              em
              m2-path m2-doc m1-path m1-doc
              (reduce
               (fn [acc [k v]]
                 (let [[new-m1-ctx es] (cs m1-ctx (conj m1-path k) v)]
                   (bail acc es)))
               []
               additional)))))]))))

;; TODO: can we move more up into m2 time ?
(defmethod check-property-2 "propertyNames" [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [m2-val]]
  (let [bail (if x? concatv bail-on-error)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (json-object? m1-doc)
          (make-error-on-failure
           "propertyNames: at least one property's name failed to conform to relevant schema"
           m2-path m2-doc m1-path m1-doc
           (reduce
            (fn [acc [k]]
              (let [[new-m1-ctx es] ((check-schema m2-ctx (conj m2-path k) m2-val) m1-ctx (conj m1-path k) k)]
                (bail acc es)))
            []
            m1-doc)))]))))

;; N.B. by default, this will bail on detection of first missing property - this may not be what is expected
(defmethod check-property-2 "required" [_property {x? :exhaustive?} m2-path m2-doc [m2-val]]
  (let [bail (if x? conj (fn [_acc r] (reduced [r])))]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (json-object? m1-doc)
          (when-let [missing (seq (reduce (fn [acc k] (if (contains? m1-doc k) acc (bail acc k))) [] m2-val))]
            [(make-error ["required: missing properties (at least):" missing] m2-path m2-doc m1-path m1-doc)]))]))))

(defmethod check-property-2 "minProperties" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (memo
   (fn [m1-ctx m1-path m1-doc]
     [m1-ctx
      (when (and
             (json-object? m1-doc)
             (< (count m1-doc) m2-val))
        [(make-error "minProperties: document contains too few properties" m2-path m2-doc m1-path m1-doc)])])))

(defmethod check-property-2 "maxProperties" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (memo
   (fn [m1-ctx m1-path m1-doc]
     [m1-ctx
      (when (and
             (json-object? m1-doc)
             (> (count m1-doc) m2-val))
        [(make-error "maxProperties: document has too many properties" m2-path m2-doc m1-path m1-doc)])])))

;; standard array properties

(defn continue [c old-es new-es]
  [c (concatv old-es new-es)])

(defn bail-out [c old-es new-es]
  (if (seq new-es)
    (reduced [c new-es])
    [c old-es]))

(defn check-items [m2-path m2-doc m1-ctx m1-path m1-doc bail i-and-css message]
  (let [[m1-ctx es]
        (reduce
         (fn [[c old-es] [[i cs] sub-document]]
           (let [[c new-es] (cs c (conj m1-path i) sub-document)]
             (bail c old-es new-es)))
         [m1-ctx []]
         (map vector i-and-css m1-doc))]
    (if (seq es)
      [m1-ctx (make-error-on-failure message m2-path m2-doc m1-path m1-doc es)]
      [m1-ctx
       ;; TODO:
       ;; (update m1-ctx :evaluated-items update m1-path (fnil conj #{}) (mapv first i-and-css))
       []])))

(defmethod check-property-2 "prefixItems" [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [m2-val]]
  (let [bail (if x? continue bail-out)
        i-and-css (vec (map-indexed (fn [i sub-schema] [i (check-schema m2-ctx (conj m2-path i) sub-schema)]) m2-val))]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       (if (json-array? m1-doc)
         (check-items
          m2-path m2-doc m1-ctx m1-path m1-doc bail i-and-css
          "prefixItems: at least one item did not conform to respective schema")
         [m1-ctx []])))))

(defmethod check-property-2 "items" [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [m2-val :as m2-vals]]
  (let [bail (if x? continue bail-out)]
    (if (true? m2-val) ;; cheeky look-ahead - potentially save lots of unnecessary work...
      (fn [m1-ctx _m1-path _m1-doc]
        [m1-ctx nil])
      ;; TODO: we should do this branch on validator version ?
      (if (json-array? m2-val)
        (let [cp (check-property "prefixItems" m2-ctx m2-path {"prefixItems" m2-val} m2-vals)]
          (memo
           (fn [m1-ctx m1-path m1-doc]
             (if (json-array? m1-doc)
               (cp m1-ctx m1-path m1-doc)
               [m1-ctx nil]))))
        ;; TODO: achieve this by looking at m1-ctx ?
        (let [n (count (m2-doc "prefixItems"))
              cs (check-schema m2-ctx m2-path m2-val)]
          (memo
           (fn [m1-ctx m1-path m1-doc]
             (if (json-array? m1-doc)
               (let [items (drop n m1-doc)
                     i-and-css (map-indexed (fn [i _] [i cs]) items)]
                 (check-items
                  m2-path m2-doc m1-ctx m1-path items bail i-and-css
                  "items: at least one item did not conform to schema"))
               [m1-ctx []]))))))))

(defmethod check-property-2 "additionalItems" [_property m2-ctx m2-path {is "items" :as m2-doc} [m2-val]]
  (if (json-array? is)
    (let [n (count is)]
      (let [checker (make-checker m2-ctx m2-path m2-val)]
        (memo
         (fn [m1-ctx m1-path m1-doc]
           [m1-ctx
            (when-not (every? (partial checker m1-ctx m1-path) (drop n m1-doc))
              [(make-error "additionalItems: present and non-conformant" m2-path m2-doc m1-path m1-doc)])]))))
    (fn [m1-ctx _m1-path _m1-doc]
      [m1-ctx nil])))
    
(defmethod check-property-2 "unevaluatedItems" [_property {d :draft :as m2-ctx} m2-path {pis "prefixItems" is "items" ais "additionalItems" :as m2-doc} [m2-val]]
  (let [uis m2-val
        [pis is uis] (if (vector? is) [is nil (or ais uis)] [pis is uis]) ;; hack to get through testsuite which mixes drafts
        n (count pis)
        is-checker (if (nil? is) (constantly false) (make-checker m2-ctx m2-path is))
        uis-checker (make-checker m2-ctx m2-path uis)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (json-array? m1-doc)
          (let [bad-items
                (drop-while
                 (partial uis-checker m1-ctx m1-path) ;; skip conformant unevaluatedItems
                 (drop-while
                  (partial is-checker m1-ctx m1-path) ;; skip conformant items - TODO: m1-path needs index appended
                  (drop n m1-doc)))] ;; skip prefixItems - if non-conformant they will raise error elsewhere
            (when (seq bad-items)
              [(make-error (str "unevaluatedItems: at least 1 non-conformant item present:" (prn-str bad-items)) m2-path m2-doc m1-path m1-doc)])))]))))

(defn contains-within-bounds [checks? es lower-bound upper-bound too-low-error too-high-error success]
  (let [count-checked (count (filter checks? es))]
    (cond
      (< count-checked lower-bound) too-low-error
      (> count-checked upper-bound) too-high-error
      :else success)))

;; TODO: optimise
(defmethod check-property-2 ["contains" "minContains" "maxContains"] [_property m2-ctx m2-path m2-doc [m2-val mn? mx?]]
  (if-let [checker (and (present? m2-val) (make-checker m2-ctx m2-path m2-val))]
    (let [lower-bound (if (present? mn?) mn? 1)
          upper-bound (if (present? mx?) mx? long-max-value)]
      (memo
       (fn [m1-ctx m1-path m1-doc]
         [m1-ctx
          (when (json-array? m1-doc)
            (contains-within-bounds
             (partial checker m1-ctx m1-path)
             m1-doc
             lower-bound
             upper-bound
             [(make-error "contains: document does not contain enough conformant elements" m2-path m2-doc m1-path m1-doc)]
             [(make-error "contains: document contains too many conformant elements" m2-path m2-doc m1-path m1-doc)]
             []))])))
           (fn [m1-ctx _m1-path _m1-doc] [m1-ctx nil])))

(defmethod check-property-2 "minItems" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (memo
   (fn [m1-ctx m1-path m1-doc]
     [m1-ctx
      (when (and
             (json-array? m1-doc)
             (< (count m1-doc) m2-val))
        [(make-error "minItems: document contains too few items" m2-path m2-doc m1-path m1-doc)])])))

(defmethod check-property-2 "maxItems" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (memo
   (fn [m1-ctx m1-path m1-doc]
     [m1-ctx
      (when (and
             (json-array? m1-doc)
             (> (count m1-doc) m2-val))
        [(make-error "maxItems: document contains too many items" m2-path m2-doc m1-path m1-doc)])])))

;; TODO: should be unique according to json equality
;; TODO: could be more efficient - only needs to find one duplicate before it bails..
;; TODO: use sorted-set-by and put items in 1 at a time until one is rejected then bail - reduced
(defmethod check-property-2 "uniqueItems" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (if m2-val
    (memo
     (fn [m1-ctx m1-path m1-doc]
       [m1-ctx
        (when (json-array? m1-doc)
          (when (not (= (count m1-doc) (count (distinct m1-doc))))
            [(make-error "uniqueItems: document contains duplicate items" m2-path m2-doc m1-path m1-doc)]))]))
    (fn [m1-ctx _m1-path _m1-doc]
      [m1-ctx nil])))

;; TODO: look for parallel? flag in context and use pmap instead of map
(defn check-of [{x? :exhaustive? p? :parallel :as m2-ctx} m2-path m2-doc m2-val]
  (let [i-and-css (vec (map-indexed (fn [i sub-schema] [i (check-schema m2-ctx (conj m2-path i) sub-schema)]) m2-val))]
    (fn [m1-ctx m1-path m1-doc message failed? failing?]
      ;;(println "check-of:" m2-path m1-path i-and-css)
      (let [bail (if x? (constantly false) failing?)]
        ;; TODO: old-copy of m1-ctx being returned
        [m1-ctx
         (make-error-on
          message
          m2-path m2-doc m1-path m1-doc
          failed?
          (if p?
            (pmap)
            (reduce
             (fn [acc [i cs]]
            ;;(println "THERE:" m2-path i)
               (let [[new-m1-ctx e] (cs m1-ctx m1-path m1-doc)
                     es (concatv acc e)]
                 (if (bail i acc e) (reduced es) es)))
             []
             i-and-css)))]))))

(defmethod check-property-2 "oneOf" [_property m2-ctx m2-path m2-doc [m2-val]]
  (let [co (check-of m2-ctx m2-path m2-doc m2-val)
        m2-count (count m2-val)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       (co
        m1-ctx m1-path m1-doc
        "oneOf: document failed to conform to one and only one sub-schema"
        (fn [es] (not= 1 (- m2-count (count es))))
        (fn [i acc e] (and (nil? e) (< (count acc) i))))))))

(defmethod check-property-2 "anyOf" [_property m2-ctx m2-path m2-doc [m2-val]]
  (let [co (check-of m2-ctx m2-path m2-doc m2-val)
        m2-count (count m2-val)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       (co
        m1-ctx m1-path m1-doc
        "anyOf: document failed to conform to at least one sub-schema"
        (fn [es] (= (count es) m2-count))
        (fn [_i _acc e] (nil? e)))))))
         
(defmethod check-property-2 "allOf" [_property m2-ctx m2-path m2-doc [m2-val]]
  (let [co (check-of m2-ctx m2-path m2-doc m2-val)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       (co
        m1-ctx m1-path m1-doc
        "allOf: document failed to conform to all sub-schemas"
        seq
        (fn [_i _acc e] (some? e)))))))

;; TODO: check this working as expected
(defmethod check-property-2 "not" [_property m2-ctx m2-path m2-doc [m2-val]]
  (let [c (check-schema m2-ctx m2-path m2-val)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       (let [[m1-ctx r] (c m1-ctx m1-path m1-doc)]
         [m1-ctx
          (when-not (seq r)
            [(make-error "not: document conformed to sub-schema" m2-path m2-doc m1-path m1-doc)])])))))

;; catch-all

(defmethod check-property-2 :default [property {{checker property} :validators :as m2-ctx} m2-path m2-doc [m2-val :as m2-vals]]
  (if checker
    (let [cp (checker property m2-ctx m2-path m2-doc m2-vals)]
      (memo
       (fn [m1-ctx m1-path m1-doc]
         [m1-ctx (cp m1-path m1-doc)])))
    (let [m (str "property: unexpected property encountered: " (pr-str property))]
      (fn [m1-ctx _m1-path _m1-doc]
        [m1-ctx (log/warn m)]))))

;;------------------------------------------------------------------------------

(defn path-extends?
  "does the second path extend the first - equality is treated as extension."
  [l r]
  (every? (partial apply =) (map vector l r)))



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

;; ?
       ["propertyNames"]
       ["propertyDependencies"]

       ["minProperties"]
       ["maxProperties"]

       ["not"]
       ["anyOf"]
       ["oneOf"]
       ["allOf"]

   ;; ["required" "dependentRequired"]                                                  :required
       ["required"]
       ["dependentRequired"]

   ;; ["dependencies" "dependentSchemas" "propertyDependencies"]                        :dependencies
       ["dependencies"]
       ["dependentSchemas"]
       ["dependentRequired"]

       ["contains" "minContains" "maxContains"]  ;; TODO: I think this involves evaluation ?
       ["minimum" "exclusiveMinimum"]
       ["maximum" "exclusiveMaximum"]
       ["contentEncoding" "contentMediaType" "contentSchema"]
       ["if" "then" "else"]

   ;; these should be evaluated last since they need to know about unevaluated properties/items

       ["minItems"]
       ["maxItems"]
       ["uniqueItems"]
   ;; ["prefixItems" "items" "additionalItems" "unevaluateItems"]                       :items ;; TODO: handle :default-additional-items here
       ["prefixItems"]
       ["items"]
       ["additionalItems"]
       ["unevaluatedItems"]


       ;; TODO:
       ;; - ensure ordering of interdependent properties - evaluated MUST come after all relevant siblings
       ;; - push expensive checks to end as we may never need them...
       
       ["properties" "patternProperties" "additionalProperties" "unevaluatedProperties"] ;; TODO: what about propertyNames ?
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

(defn do-bail-on-error [m1-ctx acc es]
  (if (seq es)
    (reduced [m1-ctx es])
    [m1-ctx acc]))

(defn dont-bail-on-error [m1-ctx acc es]
  [m1-ctx (concatv acc es)])

(defn get-bail [{x? :exhaustive?}]
  (if x? dont-bail-on-error do-bail-on-error))

(defn check-schema-2 [{t? :trace? :as m2-ctx} m2-path m2-doc]
  ;; TODO; this needs to be simplified
  (let [bail (get-bail m2-ctx)]
    (cond
      (true? m2-doc)
      (fn [m1-ctx _m1-path _m1-doc]
        [m1-ctx nil])

      (false? m2-doc)
      (fn [m1-ctx m1-path m1-doc]
        [m1-ctx
         (when (present? m1-doc)
           [(make-error "schema is false: nothing will match" m2-path m2-doc m1-path m1-doc)])])

      :else
      (let [m2-path-and-cps
            (mapv
             (fn [[ks vs]]
               (let [ks (if (= 1 (count ks)) (first ks) ks) ;; hack - lose later
                     new-m2-path (conj m2-path ks)]
                 [new-m2-path (check-property ks m2-ctx new-m2-path m2-doc vs)]))
             (compile-m2 m2-doc))]
        (fn [m1-ctx m1-path m1-doc]
          (if (present? m1-doc)
            (let [[new-m1-ctx es]
                  (reduce
                   (fn [[old-m1-ctx acc] [new-m2-path cp]]
                     (let [[new-m1-ctx [{m :message} :as es]] (cp old-m1-ctx m1-path m1-doc)]
                       (when t? (println (pr-str new-m2-path) (pr-str m1-path) (if (seq es) ["❌" m] "✅")))
                       (bail new-m1-ctx acc es)))
                   [m1-ctx []]
                   m2-path-and-cps)]
              [new-m1-ctx
               (make-error-on-failure "schema: document did not conform" m2-path m2-doc m1-path m1-doc es)])
            [m1-ctx []]))))))

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

(defn stash-reference [[acc {id-uri :id-uri :as stuff} :as x] path $ref]
  (if (string? $ref)
    [(update acc :path->uri assoc path (inherit-uri id-uri (parse-uri $ref))) stuff]
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

;; TODO: merge with above somehow... - grow into jump table for entire codebase
(def $schema->draft-info
  {
   "http://json-schema.org/draft-03/schema"       ["draft3"       "id" ]
   "http://json-schema.org/draft-04/schema"       ["draft4"       "id" ]
   "http://json-schema.org/draft-06/schema"       ["draft6"       "$id"]
   "http://json-schema.org/draft-07/schema"       ["draft7"       "$id"]
   "https://json-schema.org/draft/2019-09/schema" ["draft2019-09" "$id"]
   "https://json-schema.org/draft/2020-12/schema" ["draft2020-12" "$id"]
   "https://json-schema.org/draft/next/schema"    ["draft-next"   "$id"]
   })

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
(defn validate-2 [m2-ctx schema]
  (let [{draft :draft id-key :id-key :as m2-ctx} (make-context m2-ctx schema)
        sid (get schema id-key)
        cs (check-schema m2-ctx [] schema)]
    (memo
     (fn [m1-ctx {did id-key dsid "$schema" :as document}]
        ;;(log/info "validate:" sid "/" did)
        ;;(when (and dsid (not (= sid dsid))) (log/warn (format "document schema id not consistent with schema id: %s != %s" dsid sid)))
       (let [m1-ctx (assoc m1-ctx :id-key id-key  :uri->path {}) ;; docs must be of same draft as their schemas... ?
             m1-ctx (json-walk stash m1-ctx {} [] schema)
             m1-ctx (assoc
                     m1-ctx
                     :id-key id-key
                     :id-uri (when did (parse-uri did))
                     :original-root document
                     :recursive-anchor []
                     :root document
                     :draft draft
                     :melder (:melder m2-ctx))
             [_ es] (cs m1-ctx [] document)]
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
  ([m2-ctx schema m1-ctx document]
   (validate m2-ctx schema)
   ((validate-2 m2-ctx schema) m1-ctx document)))
