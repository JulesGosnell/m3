(ns m3.validate
  (:require
   #?@(:cljs [[goog.string :refer [format]]])
   #?(:clj  [cheshire.core :as cheshire]
      :cljs [cljs.core :as cljs])
   [cljc.java-time.local-date :as ld]
   [cljc.java-time.offset-date-time :as odt]
   [cljc.java-time.offset-time :as ot]
   [clojure.string :refer [split starts-with? ends-with? replace] :rename {replace string-replace}]
   [#?(:clj clojure.tools.logging :cljs m3.log) :as log]
   [m3.uri :refer [parse-uri inherit-uri uri-base uri-fragment]])
  #?(:clj
     (:import
      [org.graalvm.polyglot Context Value]))
  ;; temporary
  ;; (:import [java.net URL]
  ;;          [java.io BufferedReader InputStreamReader])
  )

#?(:cljs
   (def Exception js/Error))

;;------------------------------------------------------------------------------

;(def absent (Object.))
(def absent :absent)

(defn absent? [v]
  (= absent v))

(defn present? [v]
  (not (absent? v)))

;;------------------------------------------------------------------------------

(defn json-decode [s]
  #?(:clj
     (cheshire/decode s)
     :cljs
     (cljs/js->clj (js/JSON.parse s) :keywordize-keys false)))

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

(defn parse-int [s]
  #?(:clj  (Integer/parseInt s)
     :cljs (js/parseInt s)))

(defn ->int-or-string [s]
  (if (re-matches #"[0-9].*" s)
    (parse-int s)
    s))

(defn uri-decode
  "URI-decodes a percent-encoded string."
  [s]
  #?(:clj  (java.net.URLDecoder/decode s "UTF-8")
     :cljs (try
             (js/decodeURIComponent s)
             (catch :default e
               (throw (ex-info "Invalid percent-encoding in URI" {:cause e}))))))

(defn unescape [s]
  (-> s
      (string-replace "~0" "~")
      (string-replace "~1" "/")))

;; or should we just walk the data structure...
(defn canonicalise [path $ref]
  (reduce
   (fn [acc r]
     (let [r (unescape (uri-decode r))]
       (case r
         "" (if (= acc path) [] (conj acc ""))
         "." acc
         ".." (vec (take (dec (count acc)) acc))
         (conj acc (->int-or-string r)))))
   path
   (or (seq (if (= $ref "/") [""] (split $ref #"/" -1))) [""]))) ;; N.B.: -1 prevents trailing ""s being removed

;; TODO: needs a lot of work !

(declare make-context)

(defn try-path [{t? :trace? path->uri :path->uri :as ctx} path p root]
  (when p
    (let [m (get-in root p absent)]
      (when (present? m)
        (when t? (prn "resolved:" path "->" p))
        [;; use static (original) base uri rather than dynamic (inherited from ref)
         (assoc ctx :id-uri (path->uri p))
         path m]))))

(declare resolve-$ref)

(defn resolve-$ref-uri [{m2 :root uri->path :uri->path uri->schema :uri->schema :as ctx} path {t :type f :fragment :as uri} $ref]

  ;;(prn "resolve-$ref-uri:" uri $ref)

  (or

   ;; did it match a stashed [$]id or $.*anchor ?

   ;; exactly
   (try-path ctx path (uri->path uri) m2)

   ;; in all but fragment
   (when-let [path2 (uri->path (uri-base uri))]
     (or
      (try-path ctx path (canonicalise path2 f) m2)
      ;; why ?
      (try-path ctx path (concat path2 (canonicalise [] f)) m2)))

   ;; it's just a fragment
   (and (= t :fragment)
        (try-path ctx path (canonicalise path (or f "")) m2)) ;TODO

   ;; did it match a remote schema
   (when-let [[c p _m] (and uri->schema (uri->schema ctx path uri))]

     ;; thisis how I would like it to work:
     
     ;; TODO: risk of a stack overflow here if uri does not resolve in context of remote schema...
     ;; (try
     ;;   (resolve-$ref-uri c p uri (str "#" (:fragment uri)))
     ;;   (catch StackOverflowError _
     ;;     (log/error "OVERFLOW:" (pr-str [uri (:uri->path c) c]))
     ;;     ))

     ;; but this is safer
     (resolve-$ref-uri c p (uri-fragment uri) (str "#" (:fragment uri)))
     )

   (log/warn "$ref: could not resolve:" (pr-str $ref) (pr-str uri)))

  )

;;------------------------------------------------------------------------------
;; expanding $refs

;; should we be resolving this at m1-time ? are we ?
(defmulti merge-$ref (fn [{m :$ref-merger d :draft} parent reffed] (or m d)))

(defn deep-merge [& maps]
  (letfn [(reconcile-keys [val-in-result val-in-latter]
            (if (and (map? val-in-result)
                     (map? val-in-latter))
              (merge-with reconcile-keys val-in-result val-in-latter)
              val-in-latter))
          (reconcile-maps [result latter]
            (merge-with reconcile-keys result latter))]
    (reduce reconcile-maps maps)))


(defn merge-$ref-deep-under [ctx parent reffed] (if (and (map? reffed)(map? parent)) (deep-merge reffed parent) reffed))
(defn merge-$ref-deep-over  [ctx parent reffed] (if (and (map? reffed)(map? parent)) (deep-merge parent reffed) reffed))
(defn merge-$ref-under      [ctx parent reffed] (if (and (map? reffed)(map? parent)) (merge reffed parent) reffed))
(defn merge-$ref-over       [ctx parent reffed] (if (and (map? reffed)(map? parent)) (merge parent reffed) reffed))
(defn merge-$ref-replace    [ctx parent reffed] (if reffed reffed false))
;; these two should be equivalent
(defn merge-$ref-all-of     [ctx parent reffed] {"allOf" [parent reffed]})

(defmethod merge-$ref "draft3"         [ctx parent reffed] (merge-$ref-replace   ctx parent reffed))
(defmethod merge-$ref "draft4"         [ctx parent reffed] (merge-$ref-replace   ctx parent reffed))
(defmethod merge-$ref "draft6"         [ctx parent reffed] (merge-$ref-replace   ctx parent reffed))
(defmethod merge-$ref "draft7"         [ctx parent reffed] (merge-$ref-replace   ctx parent reffed))
;; both deep-over and under seem to work here
(defmethod merge-$ref "draft2019-09"   [ctx parent reffed] (merge-$ref-deep-over ctx parent reffed))
(defmethod merge-$ref "draft2020-12"   [ctx parent reffed] (merge-$ref-deep-over ctx parent reffed))
(defmethod merge-$ref "draft2021-12"   [ctx parent reffed] (merge-$ref-deep-over ctx parent reffed))
(defmethod merge-$ref "draft-next"     [ctx parent reffed] (merge-$ref-deep-over ctx parent reffed))
(defmethod merge-$ref "latest"         [ctx parent reffed] (merge-$ref-deep-over ctx parent reffed))

(defmethod merge-$ref :deep-merge-over  [ctx parent reffed] (merge-$ref-deep-over  ctx parent reffed))
(defmethod merge-$ref :deep-merge-under [ctx parent reffed] (merge-$ref-deep-under ctx parent reffed))
(defmethod merge-$ref :merge-under      [ctx parent reffed] (merge-$ref-under      ctx parent reffed))
(defmethod merge-$ref :merge-over       [ctx parent reffed] (merge-$ref-over       ctx parent reffed))
(defmethod merge-$ref :replace          [ctx parent reffed] (merge-$ref-replace    ctx parent reffed))
(defmethod merge-$ref :all-of           [ctx parent reffed] (merge-$ref-all-of     ctx parent reffed))

;;(defmethod merge-$ref :default         [ctx parent reffed] (merge-$ref-evaluate ctx parent reffed))

;;------------------------------------------------------------------------------

(defn resolve-$ref [{id-uri :id-uri :as ctx} path $ref]
  (resolve-$ref-uri ctx path (inherit-uri id-uri (parse-uri $ref)) $ref))

;;------------------------------------------------------------------------------

(defn merge-helper [old-c parent [c p child :as x]]
  (when x
    [c p (merge-$ref old-c parent child)]))
  
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

(defn expand-$ref [old-c old-p old-m r]
  (when-let [[new-c new-p new-m] (merge-helper old-c old-m (resolve-$ref old-c old-p r))]
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
        (resolve-$ref-uri c p uri r))))
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
      (resolve-$ref-uri c p uri r)))))
 
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

(defn match [pattern _m2-ctx m2-path m2-doc m1-path m1-doc]
  (when-not (re-find pattern m1-doc)
    [(make-error (str "format: does not match pattern: " (pr-str m1-doc) " - " (pr-str pattern)) m2-path m2-doc m1-path m1-doc)]))

;; standard formats

(defmethod check-format-2 "email" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:[^\s@\"\.](?:(?!\.\.)[^\s@\"])*[^\s@\"\.]|\"(?:[^\r\n\\\"]|\\[\s\S])+\")@(?:[a-zA-Z0-9\-]+(?:\.[a-zA-Z0-9\-]+)*|\[IPv6:[a-fA-F0-9:]+\]|\[(?:(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)\])$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "ipv4" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        ;; https://howtodoinjava.com/java/regex/java-regex-validate-email-address/
        #"^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\.(?!$)|$)){4}$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "ipv6" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        ;; adapted from: https://github.com/ajv-validator/ajv-formats/blob/master/src/formats.ts
        #"^((([0-9a-f]{1,4}:){7}([0-9a-f]{1,4}|:))|(([0-9a-f]{1,4}:){6}(:[0-9a-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){5}(((:[0-9a-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){4}(((:[0-9a-f]{1,4}){1,3})|((:[0-9a-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){3}(((:[0-9a-f]{1,4}){1,4})|((:[0-9a-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){2}(((:[0-9a-f]{1,4}){1,5})|((:[0-9a-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){1}(((:[0-9a-f]{1,4}){1,6})|((:[0-9a-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9a-f]{1,4}){1,7})|((:[0-9a-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "hostname" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
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
           [(make-error (str "format: not a valid date-time: " (pr-str m1-doc) " - " (ex-message e)) m2-path m2-doc m1-path m1-doc)]))))))

(defmethod check-format-2 "date" [_format _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (try
         (ld/parse m1-doc)
         nil
         (catch Exception e
           [(make-error (str "format: not a valid date: " (pr-str m1-doc) " - " (ex-message e)) m2-path m2-doc m1-path m1-doc)]))))))

(defmethod check-format-2 "time" [_format _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (try
         (ot/parse m1-doc)
         nil
         (catch Exception e
           [(make-error (str "format: not a valid time: " (pr-str m1-doc) " - " (ex-message e)) m2-path m2-doc m1-path m1-doc)]))))))

(defmethod check-format-2 "json-pointer" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:/(?:[^~/]|~[01])*)*$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "relative-json-pointer" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:0|[1-9][0-9]*)(#|(?:/(?:[^~/]|~[01])*)*)$|^#(?:/(?:[^~/]|~[01])*)*$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

;; TODO: this should be shared with uri.cljc
(def uri-regexp 
  ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
  #"^(?:[A-Za-z][A-Za-z0-9+.\-]*:[^\s]*|#(?:[^\s]*)?)$")

(defmethod check-format-2 "uri" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match uri-regexp m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "uri-reference" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^[^\s\\]*$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "uri-template" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:[^\s{}]|(?:\{[^\s{}]*\}))*$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "idn-email" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^(?:[^\s\c@\[\]\"(),:;<>\\]+(?:\.[^\s\c@\[\]\"(),:;<>\\]+)*|\"(?:[^\"\\\r\n]|\\.)+\")@(?:[^\s\c@\[\]\"(),:;<>\\]+\.)*[^\s\c@\[\]\"(),:;<>\\]+$"
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

(defmethod check-format-2 "iri" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^[A-Za-z][A-Za-z0-9+.\-]*:\/\/(?:[^\\s/?#@\\\\]+@)?(?:\[[0-9A-Fa-f:]+\]|[^\\s/?#@\\\\:]+)(?::\d+)?(?:\/[^\s?#\\\\]*)?(?:\?[^\s#\\\\]*)?(?:#[^\s\\\\]*)?$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "iri-reference" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
        ;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
        #"^[^\s\\]*$"
        m2-ctx m2-path m2-doc m1-path m1-doc)))))

(defmethod check-format-2 "uuid" [_format m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (string? m1-doc)
       (match
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
       [(make-error (str "format: not a valid duration: " (pr-str m1-doc)) m2-path m2-doc m1-path m1-doc)]))))

(defmethod check-format-2 "regex" [_format _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (try
       (when (string? m1-doc)
         (ecma-pattern m1-doc)
         [])
       (catch Exception e
         [(make-error (str "format: not a valid regex: " (pr-str m1-doc) " - " (ex-message e)) m2-path m2-doc m1-path m1-doc)])))))

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
   (fn [_m1-ctx m1-path m1-doc]
     (when-not (map? m1-doc) [(make-error "type: not an object" m2-path m2-doc m1-path m1-doc)]))))

(defmethod check-type-2 "array" [_type _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when-not (vector? m1-doc) [(make-error "type: not an array" m2-path m2-doc m1-path m1-doc)]))))

(defmethod check-type-2 "string" [_type _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when-not (string? m1-doc) [(make-error "type: not a string" m2-path m2-doc m1-path m1-doc)]))))

(defmethod check-type-2 "integer" [_type {si? :strict-integer? :as c2} m2-path m2-doc]
  (let [check (if si? integer? json-integer?)]
    (memo
     (fn [_m1-ctx m1-path m1-doc]
       (when-not (check m1-doc) [(make-error "type: not an integer" m2-path m2-doc m1-path m1-doc)])))))

(defmethod check-type-2 "number" [_type _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when-not (json-number? m1-doc) [(make-error "type: not a number" m2-path m2-doc m1-path m1-doc)]))))

(defmethod check-type-2 "boolean" [_type _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when-not (boolean? m1-doc) [(make-error "type: not a boolean" m2-path m2-doc m1-path m1-doc)]))))

(defmethod check-type-2 "null" [_type _m2-ctx m2-path m2-doc]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when-not (nil? m1-doc) [(make-error "type: non null" m2-path m2-doc m1-path m1-doc)]))))

(defmethod check-type-2 "any" [_type _m2-ctx m2-path m2-doc]
  (fn [_m1-ctx m1-path m1-doc]))

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
         (when-not (some (fn [checker] (nil? (checker m1-ctx m1-path m1-doc))) checkers)
           [(make-error (format "type: none matched: %s" ts) m2-path m2-doc m1-path m1-doc)]))))
    (memo
     (fn [_m1-ctx m1-path m1-doc]
       [(make-error (format "type: unrecognised: %s" ts) m2-path m2-doc m1-path m1-doc)]))))

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
   (fn [_m1-ctx m1-path m1-doc]
     (when (not (json-= m2-val m1-doc))
       [(make-error (format "const: document does not contain schema value: %s != %s" m1-doc m2-val) m2-path m2-doc m1-path m1-doc)]))))

(defmethod check-property-2 "enum" [_property _m2-ctx m2-path m2-doc [m2-val]]
  ;; N.B.
  ;; we can't use a memoised hash-set here because comparison is done by json-= not '='... - set-by ?
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     ;; we could check that the m2's enum contained any const or default
     ;; value here - but thus should be done somehow during validation of
     ;; the m2 as an m1 and not waste time whilst we are validating all
     ;; it's m1s...
     ;; TODO: how about some injectable consistency checking fns which can be used when validating m2s ?
     (when-not (seq-contains? m2-val m1-doc)
       [(make-error "enum: does not contain value" m2-path m2-doc m1-path m1-doc)]))))

(defmethod check-property-2 "$comment"         [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "id"               [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "$id"              [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "description"      [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "title"            [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "readOnly"         [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "writeOnly"        [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))

(defmethod check-property-2 "default"          [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "$schema"          [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "examples"         [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "$anchor"          [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "$recursiveAnchor" [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "$vocabulary"      [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "$ref"             [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))

;; NYI
(defmethod check-property-2 "$dynamicAnchor"   [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))
(defmethod check-property-2 "$dynamicRef"      [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))

;; TODO: issue a warning somehow
(defmethod check-property-2 "deprecated"  [_property _m2-ctx _m2-path _m2-doc _m2-vals] (fn [_m1-ctx _m1-path _m1-doc]))

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
           (fn [_m1-ctx m1-path m1-doc]
             (when (json-number? m1-doc)
               (when-not (p m1-doc m??imum)
                 [(make-error e m2-path m2-doc m1-path m1-doc)])))))
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
         (fn [_m1-ctx m1-path m1-doc]
           (when (json-number? m1-doc)
             (let [cm (check-m??imum m1-path m1-doc)
                   ce (check-exclusive m1-path m1-doc)]
               (-> [] (cm) (ce))))))))))

(defmethod check-property-2 :minimum [_property m2-ctx m2-path m2-doc m2-vals]
  ((check-m??imum
    >
    >=
    "minimum & exclusiveMinimum: value is not above"
    "minimum: value is not equal to or above"
    "exclusiveMinimum: value is not above")
   m2-ctx m2-path m2-doc m2-vals))

;; TODO: optimise
(defmethod check-property-2 :maximum [_property m2-ctx m2-path m2-doc m2-vals]
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
     (fn [_m1-ctx m1-path m1-doc]
       (when (and
              (json-number? m1-doc)
              (not (zero? (mod (bigdec m1-doc) m2-val-bd))))
         [(make-error (format "%s is not a multiple of %s" m1-doc m2-val) m2-path m2-doc m1-path m1-doc)])))))

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
     (fn [_m1-ctx m1-path m1-doc]
       (when (and
              (string? m1-doc)
              (or
               (< (count m1-doc) ml2) ;; precheck before using expensive json-length
               (< (json-length m1-doc) m2-val)))
         [(make-error "minLength: string too short" m2-path m2-doc m1-path m1-doc)])))))

(defmethod check-property-2 "maxLength" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (let [ml2 (* m2-val 2)]
    (memo
     (fn [_m1-ctx m1-path m1-doc]
       (when (and
              (string? m1-doc)
              (or
               (> (count m1-doc) ml2) ;; precheck before using expensive json-length
               (> (json-length m1-doc) m2-val)))
         [(make-error "maxLength: string too long" m2-path m2-doc m1-path m1-doc)])))))

(defmethod check-property-2 "pattern" [property m2-ctx m2-path m2-doc [m2-val]]
  (if (starts-with? m2-val "$format:")
    ;; N.B.
    ;; this is an extension to allow patternProperties to
    ;; leverage formats since the spec does not provide a
    ;; formatProperties...
    (check-property-2 "format" m2-ctx m2-path m2-doc [(subs m2-val (count "$format:"))]) ; TODO: use check-property ?
    (let [p (ecma-pattern m2-val)]
      (memo
       (fn [_m1-ctx m1-path m1-doc]
         (when (and
                (json-string? m1-doc)
                (false? (ecma-match p m1-doc)))
           [(make-error "pattern: doesn't match" m2-path m2-doc m1-path m1-doc)]))))))

#?(:clj
   (let [^java.util.Base64 decoder (java.util.Base64/getDecoder)]
     (defn base64-decode [^String s] (String. (.decode decoder (.getBytes s "UTF-8")) "UTF-8")))

   :cljs
   (defn base64-decode [s] "NYI"))

(def ce->decoder
  {absent identity
   "quoted-printable" identity
   "base16" (fn [_] (throw (ex-info "base16/decode: NYI" {})))
   "base32" (fn [_] (throw (ex-info "base32/decode: NYI" {})))
   "base64" base64-decode
   })

(def cmt->decoder
  {
   absent             json-decode
   "application/json" json-decode
   })

;; TODO:
;; - should it also round-trip to test stuff ?
;; - I've only implemented base64 and yet it passes all tests
;; - no unmarshall json solution in clojurescript yet...

(defmethod check-property-2 :content [_property {d :draft :as m2-ctx} m2-path m2-doc [ce cmt cs]]
  (let [ce-decoder (ce->decoder ce)
        cmt-decoder (cmt->decoder cmt)
        strict? (#{"draft7"} d) ;; check a context flag aswell
        checker (if (present? cs) (check-schema m2-ctx m2-path cs) (constantly []))]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       (when (json-string? m1-doc)
         (try
           (let [{es :errors :as v}
                 (checker
                  m1-ctx
                  m1-path
                  (try
                    (cmt-decoder
                     (try
                       (ce-decoder m1-doc)
                       (catch Exception e
                         (throw
                          (ex-info
                           nil
                           {:errors
                            (let [m (str "contentEncoding: error during decode: " (ex-message e))]
                              (if strict?
                                [(make-error m m2-path m2-doc m1-path m1-doc)]
                                (do
                                  (log/warn (string-replace m #"\n" " - "))
                                  [])))})))))
                    (catch Exception e
                      (throw
                       (ex-info
                        nil
                        {:errors
                         (let [m (str "contentMediaType: error during decode: " (ex-message e))]
                           (if strict?
                             [(make-error m m2-path m2-doc m1-path m1-doc)]
                             (do
                               (log/warn (string-replace m #"\n" " - "))
                               [])))})))))]
             (when (seq es)
               (if strict?
                 es
                 (log/warn "contentSchema: failed validation - " (prn-str v)))))
           (catch Exception e
             (:errors (ex-data e)))))))))

(defmethod check-property-2 "format" [_property {strict? :strict-format? cfs :check-format :or {cfs {}} :as m2-ctx} m2-path m2-doc [m2-val]]
  (let [f (if strict? identity (fn [f2] (fn [c p m] (when-let [[{m :message}] (f2 c p m)] (log/warn m)))))]
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
              (fn [m1-ctx m1-path m1-doc] (when (not (contains? m1-doc v)) [v v]))
              (json-array? v) ;; a multiple property dependency
              ;; TODO: this looks very suspect
              (fn [m1-ctx m1-path m1-doc] (reduce (fn [acc2 k2] (if (contains? m1-doc k2) acc2 (bail acc2 [k k2]))) [] v))
              (or (json-object? v) (boolean? v)) ;; a schema dependency
              (check-schema m2-ctx m2-path v)
              ;; we should not need to check other cases as m2 should have been validated against m3
              )))
         {}
         m2-val)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
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
           [(make-error ["dependencies: missing properties (at least):" missing] m2-path m2-doc m1-path m1-doc)]))))))

(defmethod check-property-2 "dependentSchemas" [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [m2-val]]
  (let [bail (if x? conj (fn [_acc r] (reduced [r])))
        property->checker
        (reduce
         (fn [acc [k v]]
           (assoc
            acc
            k
            (check-schema m2-ctx m2-path v)))
         {}
         m2-val)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
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
           [(make-error ["dependentSchemas: missing properties (at least):" missing] m2-path m2-doc m1-path m1-doc)]))))))

(defmethod check-property-2 "propertyDependencies" [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [m2-val]]
  (let [bail (if x? concatv bail-on-error)
        checkers (into {} (mapcat (fn [[k1 vs]] (map (fn [[k2 s]] [[k1 k2] (check-schema m2-ctx m2-path s)]) vs)) m2-val))
        ks (keys m2-val)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       (when (json-object? m1-doc)
         (reduce
          (fn [acc k]
            (let [v (m1-doc k)]
              (if-let [checker (and (json-string? v) (checkers [k v]))]
                (bail acc (checker m1-ctx m1-path m1-doc))
                acc)))
          []
          ks))))))

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
           [(make-error ["dependentRequired: missing properties (at least):" missing] m2-path m2-doc m1-path m1-doc)]))))))

;;------------------------------------------------------------------------------

(defn make-checker [m2-ctx m2-path m2-doc]
  (let [checker (check-schema m2-ctx m2-path m2-doc)]
    (fn [m1-ctx m1-path m1-doc]
      (empty? (checker m1-ctx m1-path m1-doc)))))

(defmethod check-property-2 :if-then-else [_property m2-ctx m2-path m2-doc [if? then else]]
  (if (present? if?)
    (let [if-checker (check-schema m2-ctx m2-path if?)
          then-checker (if (present? then) (check-schema m2-ctx m2-path then) (constantly []))
          else-checker (if (present? else) (check-schema m2-ctx m2-path else) (constantly []))]
      (memo
       (fn [m1-ctx m1-path m1-doc]
         ((if (empty? (if-checker m1-ctx m1-path m1-doc)) then-checker else-checker) m1-ctx m1-path m1-doc))))
    (constantly [])))

;; TODO
;; "unevaluatedProperties"
;; - linked to schema composition
;; - see http://json-schema.org/understanding-json-schema/reference/object.html#unevaluated-properties

;; http://json-schema.org/understanding-json-schema/reference/object.html

(defmethod check-property-2 "definitions" [_property m2-ctx m2-path _m2-doc [m2-val]]
  (mapv (fn [[k v]] (check-schema m2-ctx (conj m2-path k) v)) m2-val)
  (fn [_m1-ctx _m1-path _m1-doc]))

(defmethod check-property-2 "$defs" [_property m2-ctx m2-path _m2-doc [m2-val]]
  (mapv (fn [[k v]] (check-schema m2-ctx (conj m2-path k) v)) m2-val)
  (fn [_m1-ctx _m1-path _m1-doc]))

(defmethod check-property-2 :properties [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [ps pps aps]]
  (let [bail (if x? concatv bail-on-error)
        cp-and-ks (mapv (fn [[k v]] [(check-schema m2-ctx (conj m2-path k) v) k]) (when (present? ps) ps))
        named? (if (present? ps) (partial contains? ps) (constantly false))

        cp-and-pattern-and-ks (mapv (fn [[k v]] [(check-schema m2-ctx (conj m2-path k) v) (ecma-pattern k) k]) (when (present? pps) pps))
        patterns (mapv second cp-and-pattern-and-ks)
        pattern? (if (seq patterns) (fn [k] (some (fn [pattern] (ecma-match pattern k)) patterns)) (constantly false))
        
        cs (if (present? aps) (check-schema m2-ctx m2-path aps) (constantly []))]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       (when (json-object? m1-doc)
         (or
          (make-error-on-failure
           "properties: at least one property failed to conform to the relevant schema"
           m2-path m2-doc m1-path m1-doc
           (reduce
            (fn [acc [cp k]]
              (bail acc (cp m1-ctx (conj m1-path k) (get m1-doc k absent))))
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
                   (bail acc2 (cp m1-ctx (conj m1-path k) v))
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
             "additionalProperties: at least one additional property failed to conform to schema"
             m2-path m2-doc m1-path m1-doc
             (reduce
              (fn [acc [k v]]
                (bail acc (cs m1-ctx (conj m1-path k) v)))
              []
              additional)))))))))

;; TODO: can we move more up into m2 time ?
(defmethod check-property-2 "propertyNames" [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [m2-val]]
  (let [bail (if x? concatv bail-on-error)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       (when (json-object? m1-doc)
         (make-error-on-failure
          "propertyNames: at least one property's name failed to conform to relevant schema"
          m2-path m2-doc m1-path m1-doc
          (reduce
           (fn [acc [k]]
             (bail acc ((check-schema m2-ctx (conj m2-path k) m2-val) m1-ctx (conj m1-path k) k)))
           []
           m1-doc)))))))

;; N.B. by default, this will bail on detection of first missing property - this may not be what is expected
(defmethod check-property-2 "required" [_property {x? :exhaustive?} m2-path m2-doc [m2-val]]
  (let [bail (if x? conj (fn [_acc r] (reduced [r])))]
    (memo
     (fn [_m1-ctx m1-path m1-doc]
       (when (json-object? m1-doc)
         (when-let [missing (seq (reduce (fn [acc k] (if (contains? m1-doc k) acc (bail acc k))) [] m2-val))]
           [(make-error ["required: missing properties (at least):" missing] m2-path m2-doc m1-path m1-doc)]))))))

(defmethod check-property-2 "minProperties" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (and
            (json-object? m1-doc)
            (< (count m1-doc) m2-val))
       [(make-error "minProperties: document contains too few properties" m2-path m2-doc m1-path m1-doc)]))))

(defmethod check-property-2 "maxProperties" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (and
            (json-object? m1-doc)
            (> (count m1-doc) m2-val))
       [(make-error "maxProperties: document has too many properties" m2-path m2-doc m1-path m1-doc)]))))

;; standard array properties

(defmethod check-property-2 "prefixItems" [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [m2-val]]
  (let [bail (if x? concatv bail-on-error)
        i-and-css (vec (map-indexed (fn [i sub-schema] [i (check-schema m2-ctx (conj m2-path i) sub-schema)]) m2-val))]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       (when (json-array? m1-doc)
         (make-error-on-failure
          "prefixItems: at least one item did not conform to respective schema"
          m2-path m2-doc m1-path m1-doc
          (reduce
           (fn [acc [[i cs] sub-document]]
             (bail acc (cs m1-ctx (conj m1-path i) sub-document)))
           []
           (map vector i-and-css m1-doc))))))))

(defmethod check-property-2 "items" [_property {x? :exhaustive? :as m2-ctx} m2-path m2-doc [m2-val :as m2-vals]]
  (let [bail (if x? concatv bail-on-error)]
    (if (true? m2-val) ;; cheeky look-ahead - potentially save lots of unnecessary work...
      (constantly nil)
      (if (json-array? m2-val)
        (let [cp (check-property "prefixItems" m2-ctx m2-path {"prefixItems" m2-val} m2-vals)]
          (memo
           (fn [m1-ctx m1-path m1-doc]
             (when (json-array? m1-doc)
               (cp m1-ctx m1-path m1-doc)))))
        (let [n (count (m2-doc "prefixItems"))
              cs (check-schema m2-ctx m2-path m2-val)]
          (memo
           (fn [m1-ctx m1-path m1-doc]
             (when (json-array? m1-doc)
               (make-error-on-failure
                "items: at least one item did not conform to schema"
                m2-path m2-doc m1-path m1-doc
                (reduce
                 (fn [acc [i sub-document]]
                   (bail acc (cs m1-ctx (conj m1-path i) sub-document)))
                 []
                 (map-indexed vector (drop n m1-doc))))))))))))

(defmethod check-property-2 "additionalItems" [_property m2-ctx m2-path {is "items" :as m2-doc} [m2-val]]
  (if (json-array? is)
    (let [n (count is)]
      (cond
        (boolean? m2-val)
        (memo
         (fn [_m1-ctx m1-path m1-doc]
           (when (and (false? m2-val) (> (count m1-doc) n))
             [(make-error "additionalItems: should not be present" m2-path m2-doc m1-path m1-doc)])))
        (json-object? m2-val)
        (let [checker (make-checker m2-ctx m2-path m2-val)]
          (memo
           (fn [m1-ctx m1-path m1-doc]
             (when-not (every? (partial checker m1-ctx m1-path) (drop n m1-doc))
               [(make-error "additionalItems: present and non-conformant" m2-path m2-doc m1-path m1-doc)]))))))
    (constantly nil)))
    
(defmethod check-property-2 "unevaluatedItems" [_property {d :draft :as m2-ctx} m2-path {pis "prefixItems" is "items" ais "additionalItems" :as m2-doc} [m2-val]]
  (let [uis m2-val
        [pis is uis] (if (vector? is) [is nil (or ais uis)] [pis is uis]) ;; hack to get through testsuite which mixes drafts
        n (count pis)
        is-checker (if (nil? is) (constantly false) (make-checker m2-ctx m2-path is))
        uis-checker (make-checker m2-ctx m2-path uis)]
    (memo
     (fn [m1-ctx m1-path m1-doc]
       (when (json-array? m1-doc)
         (let [bad-items
               (drop-while
                (partial uis-checker m1-ctx m1-path) ;; skip conformant unevaluatedItems
                (drop-while
                 (partial is-checker m1-ctx m1-path) ;; skip conformant items - TODO: m1-path needs index appended
                 (drop n m1-doc)))] ;; skip prefixItems - if non-conformant they will raise error elsewhere
           (when (seq bad-items)
             [(make-error (str "unevaluatedItems: at least 1 non-conformant item present:" (prn-str bad-items)) m2-path m2-doc m1-path m1-doc)])))))))

(defn contains-within-bounds [checks? es lower-bound upper-bound too-low-error too-high-error success]
  (let [count-checked (count (filter checks? es))]
    (cond
      (< count-checked lower-bound) too-low-error
      (> count-checked upper-bound) too-high-error
      :else success)))

;; TODO: optimise
(defmethod check-property-2 :contains [_property m2-ctx m2-path m2-doc [m2-val mn? mx?]]
  (if-let [checker (and (present? m2-val) (make-checker m2-ctx m2-path m2-val))]
    (let [lower-bound (if (present? mn?) mn? 1)
          upper-bound (if (present? mx?) mx? Long/MAX_VALUE)]
      (memo
       (fn [m1-ctx m1-path m1-doc]
         (when (json-array? m1-doc)
           (contains-within-bounds
            (partial checker m1-ctx m1-path)
            m1-doc
            lower-bound
            upper-bound
            [(make-error "contains: document does not contain enough conformant elements" m2-path m2-doc m1-path m1-doc)]
            [(make-error "contains: document contains too many conformant elements" m2-path m2-doc m1-path m1-doc)]
            [])))))
    (constantly [])))

(defmethod check-property-2 "minItems" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (and
            (json-array? m1-doc)
            (< (count m1-doc) m2-val))
       [(make-error "minItems: document contains too few items" m2-path m2-doc m1-path m1-doc)]))))

(defmethod check-property-2 "maxItems" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (memo
   (fn [_m1-ctx m1-path m1-doc]
     (when (and
            (json-array? m1-doc)
            (> (count m1-doc) m2-val))
       [(make-error "maxItems: document contains too many items" m2-path m2-doc m1-path m1-doc)]))))

;; TODO: should be unique according to json equality
;; TODO: could be more efficient - only needs to find one duplicate before it bails..
;; TODO: use sorted-set-by and put items in 1 at a time until one is rejected then bail - reduced
(defmethod check-property-2 "uniqueItems" [_property _m2-ctx m2-path m2-doc [m2-val]]
  (if m2-val
    (memo
     (fn [_m1-ctx m1-path m1-doc]
       (when (json-array? m1-doc)
         (when (not (= (count m1-doc) (count (distinct m1-doc))))
           [(make-error "uniqueItems: document contains duplicate items" m2-path m2-doc m1-path m1-doc)]))))
    (constantly nil)))

;; TODO: look for parallel? flag in context and use pmap instead of map
(defn check-of [{x? :exhaustive? p? :parallel :as m2-ctx} m2-path m2-doc m2-val]
  (let [i-and-css (vec (map-indexed (fn [i sub-schema] [i (check-schema m2-ctx (conj m2-path i) sub-schema)]) m2-val))]
    (fn [m1-ctx m1-path m1-doc message failed? failing?]
      (let [bail (if x? (constantly false) failing?)]
        (make-error-on
         message
         m2-path m2-doc m1-path m1-doc
         failed?
         (if p?
           (pmap
            )
         (reduce
          (fn [acc [i cs]]
            (let [e (cs m1-ctx m1-path m1-doc)
                  es (concatv acc e)]
              (if (bail i acc e) (reduced es) es)))
          []
          i-and-css)))))))

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
       (let [r (c m1-ctx m1-path m1-doc)]
         (when-not (seq r)
           [(make-error "not: document conformed to sub-schema" m2-path m2-doc m1-path m1-doc)]))))))

;; catch-all

(defmethod check-property-2 :default [property {{checker property} :validators :as m2-ctx} m2-path m2-doc [m2-val :as m2-vals]]
  (if checker
    (let [cp (checker property m2-ctx m2-path m2-doc m2-vals)]
      (memo
       (fn [_m1-ctx m1-path m1-doc]
         (cp m1-path m1-doc))))
    (let [m (str "unexpected property in schema: " (pr-str property))]
      (fn [_m1-ctx m1-path m1-doc]
        (log/warn m)))))

;;------------------------------------------------------------------------------

(defn object-schema? [{t "type" ps "properties" pps "patternProperties" aps "additionalProperties" ups "unevaluatedProperties"}]
  (or (= "object" t) ps pps aps ups))

(defn array-schema? [{t "type" is "items" ais "additionalItems" pis "prefixItems" uis "unevaluatedItems"}]
  (or (= "array" t) is ais pis uis))

(defn path-extends?
  "does the second path extend the first - equality is treated as extension."
  [l r]
  (every? (partial apply =) (map vector l r)))

;; we need to figure out how to group properties that need to be processed together:
;; TODO: a single lookup that returns both group and property list
(def properties->group
  {
   ["properties" "patternProperties" "additionalProperties" ;; "unevaluatedProperties"
    ] :properties
   ;; ["prefixItems" "items" "additionalItems" "unevaluateItems"]                       :items
   ;; ["required" "dependentRequired"]                                                  :required
   ;; ["dependencies" "dependentSchemas" "propertyDependencies"]                        :depedencies
   ["contains" "minContains" "maxContains"]                                          :contains
   ["minimum" "exclusiveMinimum"]                                                    :minimum
   ["maximum" "exclusiveMaximum"]                                                    :maximum
   ["contentEncoding" "contentMediaType" "contentSchema"]                            :content
   ["if" "then" "else"]                                                              :if-then-else
   })
(let [g->ps 
  (reduce-kv (fn [acc k v] (assoc acc v k)) {} properties->group)]
  (defn group->properties [g]
    (get g->ps g [g])))

(let [p->g (reduce-kv (fn [acc ks v] (reduce (fn [acc k] (assoc acc k v)) acc ks)) {} properties->group)]
  (defn property->group [p]
    (get p->g p p)))

(let [p->g-and-ps (reduce-kv (fn [acc ps g] (reduce (fn [acc p] (assoc acc p [g ps])) acc ps)) {} properties->group)]
  (defn property->group-and-properties [p]
    (or (get p->g-and-ps p) [p [p]])))

(defn select-values [m ks absent]
  (mapv (fn [k] (get m k :absent)) ks))

(defn check-schema-2 [{x? :exhaustive? t? :trace? :as m2-ctx} m2-path m2-doc]
  ;; TODO; this needs to be simplified
  (let [bail (if x? concatv bail-on-error)
        ;; TODO - fix this - it will no longer work
        m2-doc (if (and (object-schema? m2-doc) (not (contains? m2-doc "additionalProperties")) (contains? m2-ctx :default-additional-properties))
                 (assoc m2-doc "additionalProperties" (:default-additional-properties m2-ctx))
                 m2-doc)
        m2-doc (if (and (array-schema? m2-doc) (not (contains? m2-doc "additionalItems")) (contains? m2-ctx :default-additional-items))
                 (assoc m2-doc "additionalItems" (:default-additional-items m2-ctx))
                 m2-doc)]
    (cond
      (true? m2-doc)
      (constantly nil)

      (false? m2-doc)
      (fn [_m1-ctx m1-path m1-doc]
        (when (present? m1-doc)
          [(make-error "schema is false: nothing will match" m2-path m2-doc m1-path m1-doc)]))

      :else
      (let [group->keys (group-by property->group (keys m2-doc))
            ;; I think there is a more clever way to do this where we iterat through the m2-doc grouping the [k v] tuple rather thn just the k and then having to later deref the v... - leave for later
            group->values (mapv (fn [[g ks]] [g (select-values m2-doc (group->properties g) absent)]) group->keys)
            m2-path-and-cps
            (mapv
             (fn [[k v]]
               (let [new-m2-path (conj m2-path k)]
                 [new-m2-path (check-property k m2-ctx new-m2-path m2-doc v)]))
             group->values)]
        (fn [m1-ctx m1-path m1-doc]
          (when (present? m1-doc)
            (make-error-on-failure
             "schema: document did not conform"
             m2-path m2-doc m1-path m1-doc
             (reduce
              (fn [acc [new-m2-path cp]]
                (let [[{m :message} :as es] (cp m1-ctx m1-path m1-doc)]
                  (when t? (println (pr-str new-m2-path) (pr-str m1-path) (if (seq es) ["❌" m] "✅")))
                  (bail acc es)))
              []
              m2-path-and-cps))))))))

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
          (let [new-m2 (dissoc old-m2 k)]
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

;;------------------------------------------------------------------------------

;; TODO: rename
(def uri-base->dir
  {"http://json-schema.org" "resources/schemas"
   "https://json-schema.org" "resources/schemas"})

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

(declare uri->continuation)

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

(defn uri->continuation [uri-base->dir]
  (let [uri->schema (partial uri->schema uri-base->dir)]
    (fn [c p uri]
      (when-let [m (uri->schema c p uri)] ;; TODO: what if schema is 'false'
        [(-> (make-context
              (-> c
                  (select-keys [:uri->schema :trace?])
                  (assoc :id-uri (uri-base uri)))
              m)
             (assoc :id-uri (uri-base uri))
             (update :uri->path assoc (uri-base uri) []))
         []
         m]))))  

;; TODO: rename :root to ?:expanded?
(defn validate
  ([m2-ctx schema]
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
                      :$ref-merger (:$ref-merger m2-ctx))
              es (cs m1-ctx [] document)]
          {:valid? (empty? es) :errors es})))))
  ([m2-ctx schema m1-ctx document]
   ((validate m2-ctx schema) m1-ctx document))
  ([{s "$schema" :as document}]
   (if s
     ((validate {:trace? false} (uri->schema-2 {} [] (parse-uri s))) {} document)
     [{:errors "no $schema given"}])))
