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

(ns m3.property
  (:require
   #?(:cljs [goog.string.format]
      :cljs [cljs.core :as cljs])
   [clojure.string :refer [starts-with? replace] :rename {replace string-replace}]
   [#?(:clj clojure.tools.logging :cljs m3.log) :as log]
   [m3.platform :refer [pformat json-decode big-zero? big-mod pbigdec]]
   [m3.util :refer [absent present? concatv into-set conj-set seq-contains? make-error make-error-on make-error-on-failure get-check-schema third]]
   [m3.ecma :refer [ecma-pattern ecma-match]]
   [m3.uri :refer [parse-uri inherit-uri]]
   [m3.ref :refer [resolve-uri meld try-path]]
   [m3.type :refer [json-number? json-string? json-array? json-object? check-type make-type-checker make-new-type-checker json-=]]
   [m3.format :as format :refer [draft->format->checker]]))

;;------------------------------------------------------------------------------
;; standard common properties

;; check-property-extends is defined later, near check-property-allOf
(declare check-property-extends)

(defn check-property-disallow [_property c2 p2 m2 v2]
  (let [[c2 m2 ct] (check-type v2 c2 p2 m2)]
    [c2
     m2
     (fn [c1 p1 m1]
       (let [[c1 m1 es] (ct c1 p1 m1)]
         [c1
          m1
          (when (nil? es) [(make-error "disallow: type matched" p2 m2 p1 m1)])]))]))

(defn check-property-type [_property c2 p2 m2 v2]
  (check-type v2 c2 p2 m2))

(defn check-property-const [_property c2 p2 m2 v2]
  [c2
   m2
   (fn [c1 p1 m1]
     [c1
      m1
      (when (not (json-= v2 m1))
        [(make-error (pformat "const: document does not contain schema value: %s != %s" m1 v2) p2 m2 p1 m1)])])])

(defn check-property-enum [_property c2 p2 m2 v2]
  [c2
   m2
   (fn [c1 p1 m1]
     ;; we could check that the m2's enum contained any const or default
     ;; value here - but thus should be done somehow during validation of
     ;; the m2 as an m1 and not waste time whilst we are validating all
     ;; it's m1s...
     ;; TODO: how about some injectable consistency checking fns which can be used when validating m2s ?
     [c1
      m1
      (when-not (seq-contains? v2 json-= m1)
        [(make-error "enum: does not contain value" p2 m2 p1 m1)])])])

(defn check-property-id [_property c2 _p2 m2 v2]
  ;; Stash id into c2 for compile-time ref resolution (replaces interceptor)
  (let [new-id-uri (inherit-uri (:id-uri c2) (parse-uri v2))
        c2 (assoc c2 :id-uri new-id-uri)]
    [c2
     m2
     ;; Also stash into c1 at runtime for document validation
     (fn [{old-id-uri :id-uri :as c1} p1 {id "id" :as m1}]
       [(if-let [new-id-uri (and id (inherit-uri old-id-uri (parse-uri id)))]
          (-> c1
              (update :path->uri assoc p1 new-id-uri)
              (update :uri->path assoc new-id-uri p1)
              (assoc :id-uri new-id-uri))
          (-> c1
              (update :path->uri assoc p1 old-id-uri)))
        m1
        nil])]))

(defn check-property-$id [_property c2 _p2 m2 v2]
  ;; Stash $id into c2 for compile-time ref resolution (replaces interceptor)
  (let [new-id-uri (inherit-uri (:id-uri c2) (parse-uri v2))
        c2 (assoc c2 :id-uri new-id-uri)]
    [c2
     m2
     ;; Also stash into c1 at runtime for document validation
     (fn [{old-id-uri :id-uri :as c1} p1 {id "$id" :as m1}]
       [(if-let [new-id-uri (and id (inherit-uri old-id-uri (parse-uri id)))]
          (-> c1
              (update :path->uri assoc p1 new-id-uri)
              (update :uri->path assoc new-id-uri p1)
              (assoc :id-uri new-id-uri))
          (-> c1
              (update :path->uri assoc p1 old-id-uri)))
        m1
        nil])]))

;; Anchors only need :uri->path (no change, but included for completeness)
(defn check-property-$anchor [_property c2 _p2 m2 v2]
  [c2
   m2
   (fn [c1 p1 m1]
     (let [anchor-uri (inherit-uri (c1 :id-uri) (parse-uri (str "#" v2)))]
       [(update c1 :uri->path assoc anchor-uri p1) m1 nil]))])

(defn check-property-$recursiveAnchor [_property c2 p2 m2 v2]
  ;; Stash into c2 for compile-time ref resolution (replaces interceptor)
  (let [schema-p2 (vec (butlast p2))
        c2 (if (true? v2)
             (let [[uris top] (:$recursive-anchor c2 [#{} nil])]
               (assoc c2 :$recursive-anchor
                      (if top
                        [(conj uris (:id-uri c2)) top]
                        [#{(:id-uri c2)} schema-p2])))
             c2)]
    [c2
     m2
     ;; Also stash into c1 at runtime for document validation
     (fn [c1 p1 m1]
       (if (true? v2)
         (let [[uris top] (c1 :$recursive-anchor [#{} nil])]
           [(assoc c1 :$recursive-anchor [(conj uris (c1 :id-uri)) (or top p1)]) m1 nil])
         [c1 m1 nil]))]))

(defn check-property-$dynamicAnchor [_property c2 p2 m2 v2]
  ;; Stash into c2 for compile-time ref resolution (replaces interceptor)
  (let [schema-p2 (vec (butlast p2))
        anchor-uri (inherit-uri (:id-uri c2) (parse-uri (str "#" v2)))
        c2 (update-in c2 [:$dynamic-anchor anchor-uri]
                       (fn [old new] (if old old new)) schema-p2)]
    [c2
     m2
     ;; Also stash into c1 at runtime for document validation
     (fn [c1 p1 m1]
       (let [anchor-uri (inherit-uri (c1 :id-uri) (parse-uri (str "#" v2)))]
         [(update c1 :$dynamic-anchor assoc anchor-uri p1) m1 nil]))]))

(defn check-property-$comment [_property c2 _p2 m2 _v2]
  [c2
   m2
   (fn [c1 _p1 m1]
    ;;(log/info (str "$comment:" v2 " : " _p1))
     [c1 m1 nil])])

;; $ref resolution logic - called from the $ref interceptor.
;; Resolution is LAZY (inside f1) to handle recursive schemas.
(defn check-property-$ref [_property {id-uri :id-uri draft :draft path->uri :path->uri :as c2} p2 m2 v2]
  (let [schema-p2 (vec (butlast p2))
        ;; For draft <= draft7, $ref ignores sibling $id.
        ;; Use the parent's id-uri from the pre-scan.
        effective-id-uri (if (#{:draft3 :draft4 :draft6 :draft7} draft)
                           (or (get path->uri schema-p2) id-uri)
                           id-uri)
        ref-uri (inherit-uri effective-id-uri (parse-uri v2))
        m2-no-ref (dissoc m2 "$ref")
        effective-c2 (assoc c2 :id-uri effective-id-uri)]
    [(if (#{:draft3 :draft4 :draft6 :draft7} draft)
       ;; For old drafts, $ref overrides siblings — tell compile-m2 to skip them
       (assoc c2 :skip-keys (set (keys m2-no-ref)))
       c2)
     m2
     ;; f1: resolve and compile LAZILY at runtime
     (fn [c1 p1 m1]
       (if (present? m1)
         (let [check-schema-fn (get-check-schema)
               resolution (resolve-uri effective-c2 schema-p2 ref-uri v2)]
           (if resolution
             (let [[new-c _new-p resolved-m] resolution
                   melded (meld effective-c2 m2-no-ref resolved-m)
                   ;; Update :root so nested $refs (e.g. "#/$defs/...") see melded schema.
                   ;; Only safe when path exists in root (avoids creating maps for vector indices).
                   final-c2 (if (and (= effective-c2 new-c)
                                     (or (empty? schema-p2)
                                         (some? (get-in (:root effective-c2) schema-p2))))
                              (assoc-in effective-c2 (into [:root] schema-p2) melded)
                              new-c)
                   [_c2 _m2 compiled-f1] (check-schema-fn final-c2 schema-p2 melded)]
               (compiled-f1 c1 p1 m1))
             ;; Resolution failed - compile schema without $ref
             (let [[_c2 _m2 compiled-f1] (check-schema-fn effective-c2 schema-p2 m2-no-ref)]
               (compiled-f1 c1 p1 m1))))
         [c1 m1 nil]))]))

(defn check-property-$schema [_property c2 _p2 m2 v2]
  ;; This is where dialect switching happens!
  ;; When we encounter a $schema, we:
  ;; 1. Parse it to determine the draft
  ;; 2. Try to load the metaschema to extract its $vocabulary
  ;; 3. Build the appropriate dialect
  ;; 4. Return updated c2 with new dialect - compile-m2's loop/recur will handle the rest
  (let [uri (parse-uri v2)
        draft (or ((deref (resolve 'm3.draft/$schema-uri->draft)) uri)
                  (:draft c2))
        ;; Try to load the metaschema to get its $vocabulary
        uri->schema (:uri->schema c2)
        metaschema (when uri->schema
                     (try
                       ;; IMPORTANT: uri->schema returns [c2 path schema], not just schema!
                       (let [[_ _ ms] (uri->schema c2 [] uri)]
                         ms)
                       (catch #?(:clj Exception :cljs js/Error) e
                         (log/info (str "Could not load metaschema: " v2 " - " #?(:clj (.getMessage e) :cljs (.-message e))))
                         nil)))
        ;; Extract $vocabulary from metaschema (if present)
        vocab-map (get metaschema "$vocabulary")
;; Build dialect: use $vocabulary if present, otherwise use default
        new-dialect (if vocab-map
                      ((deref (resolve 'm3.vocabulary/make-dialect)) draft vocab-map)
                      ((deref (resolve 'm3.vocabulary/draft->default-dialect)) draft))
        new-c2 (assoc c2
                      :dialect new-dialect
                      :draft draft)]
    [new-c2
     m2
     (fn [c1 _p1 m1]
       ;; TODO: In the future, we could validate m2 against its meta-schema here
       ;; For now, just pass through
       [c1 m1 nil])]))

(defn check-property-$recursiveRef [_property {id-uri :id-uri :as c2} p2 m2 v2]
  (let [schema-p2 (vec (butlast p2))]
    [c2
     m2
     ;; f1: resolve LAZILY at runtime
     (fn [c1 p1 m1]
       (if (present? m1)
         (if (= "#" v2)
           (let [check-schema-fn (get-check-schema)
                 [uris top] (:$recursive-anchor c2)
                 m2-no-ref (dissoc m2 "$recursiveRef")
                 resolution (or (try-path c2 schema-p2 (and uris (uris id-uri) top) (:original-root c2))
                                (resolve-uri c2 schema-p2 id-uri v2))]
             (if resolution
               (let [[new-c _new-p resolved-m] resolution
                     melded (meld c2 m2-no-ref resolved-m)
                     final-c2 (if (and (= c2 new-c)
                                       (or (empty? schema-p2)
                                           (some? (get-in (:root c2) schema-p2))))
                                (assoc-in c2 (into [:root] schema-p2) melded)
                                new-c)
                     [_c2 _m2 compiled-f1] (check-schema-fn final-c2 schema-p2 melded)]
                 (compiled-f1 c1 p1 m1))
               (let [[_c2 _m2 compiled-f1] (check-schema-fn c2 schema-p2 m2-no-ref)]
                 (compiled-f1 c1 p1 m1))))
           (do (log/warn "$recursiveRef: unexpected value:" (pr-str v2))
               [c1 m1 nil]))
         [c1 m1 nil]))]))
(defn check-property-$dynamicRef [_property {id-uri :id-uri :as c2} p2 m2 v2]
  (let [schema-p2 (vec (butlast p2))
        uri (inherit-uri id-uri (parse-uri v2))]
    [c2
     m2
     ;; f1: resolve LAZILY at runtime
     (fn [c1 p1 m1]
       (if (present? m1)
         (let [check-schema-fn (get-check-schema)
               da (:$dynamic-anchor c2)
               m2-no-ref (dissoc m2 "$dynamicRef")
               resolution (or (try-path c2 schema-p2 (get da uri) (:original-root c2))
                              (resolve-uri c2 schema-p2 uri v2))]
           (if resolution
             (let [[new-c _new-p resolved-m] resolution
                   melded (meld c2 m2-no-ref resolved-m)
                   final-c2 (if (and (= c2 new-c)
                                     (or (empty? schema-p2)
                                         (some? (get-in (:root c2) schema-p2))))
                              (assoc-in c2 (into [:root] schema-p2) melded)
                              new-c)
                   [_c2 _m2 compiled-f1] (check-schema-fn final-c2 schema-p2 melded)]
               (compiled-f1 c1 p1 m1))
             (let [[_c2 _m2 compiled-f1] (check-schema-fn c2 schema-p2 m2-no-ref)]
               (compiled-f1 c1 p1 m1))))
         [c1 m1 nil]))]))
(defn check-property-description   [_property c2 _p2 m2 _v2] [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])])
(defn check-property-readOnly      [_property c2 _p2 m2 _v2] [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])])
(defn check-property-writeOnly     [_property c2 _p2 m2 _v2] [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])])
(defn check-property-title         [_property c2 _p2 m2 _v2] [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])])
(defn check-property-default       [_property c2 _p2 m2 _v2] [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])])
(defn check-property-examples      [_property c2 _p2 m2 _v2] [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])])

(defn check-property-$vocabulary [_property {d :draft :as c2} _p2 m2 v2]
  ;; $vocabulary specifies which vocabularies are active for this schema
  ;; v2 is a map like: {"vocab-uri-1" true, "vocab-uri-2" false, ...}
  ;; Build a new dialect using only the vocabularies that are true
  (let [new-dialect ((deref (resolve 'm3.vocabulary/make-dialect)) d v2)
        new-c2 (assoc c2 :dialect new-dialect)]
    [new-c2
     m2
     (fn [c1 _p1 m1]
       ;; No validation work needed - just pass through
       [c1 m1 nil])]))

;; TODO: issue a warning somehow
(defn check-property-deprecated [_property c2 _p2 m2 _v2] [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])]) ;; TODO: issue a warning or error ?

;; standard number properties

(defn check-property-minimum-old [_property c2 p2 m2 v2]
  (let [e? (m2 "exclusiveMinimum")
        p? (if e? < <=)]
    [c2
     m2
     (make-new-type-checker
      json-number?
      (fn [c1 p1 m1]
        [c1
         m1
         (when-not (p? v2 m1)
           [(make-error (str "minimum" (when e? "(with exclusiveMinimum)") ": value to low") p2 m2 p1 m1)])]))]))

(defn check-property-minimum-new [_property c2 p2 m2 v2]
  [c2
   m2
   (make-new-type-checker
    json-number?
    (fn [c1 p1 m1]
      [c1
       m1
       (when-not (<= v2 m1)
         [(make-error "minimum: value to low" p2 m2 p1 m1)])]))])

(defn check-property-exclusiveMinimum-old [_property c2 _p2 {m "minimum" :as m2} _v2]
  [c2
   m2
   (fn [c1 _p1 m1]
     (when-not m (log/warn "exclusiveMinimum: no minimum present to modify"))
     [c1 m1 []])])

(defn check-property-exclusiveMinimum-new [_property c2 p2 m2 v2]
  [c2
   m2
   (make-new-type-checker
    json-number?
    (fn [c1 p1 m1]
      [c1
       m1
       (when-not (< v2 m1)
         [(make-error "minimum: value to low" p2 m2 p1 m1)])]))])

(defn check-property-maximum-old [_property c2 p2 m2 v2]
  (let [e? (m2 "exclusiveMaximum")
        p? (if e? > >=)]
    [c2
     m2
     (make-new-type-checker
      json-number?
      (fn [c1 p1 m1]
        [c1
         m1
         (when-not (p? v2 m1)
           [(make-error (str "maximum" (when e? "(with exclusiveMaximum)") ": value too high") p2 m2 p1 m1)])]))]))

(defn check-property-maximum-new [_property c2 p2 m2 v2]
  [c2
   m2
   (make-type-checker
    json-number?
    (fn [c1 p1 m1]
      [c1
       m1
       (when-not (>= v2 m1)
         [(make-error "maximum: value too high" p2 m2 p1 m1)])]))])

(defn check-property-exclusiveMaximum-old [_property c2 _p2 {m "maximum" :as m2} _v2]
  [c2
   m2
   (make-new-type-checker
    json-number?
    (fn [c1 _p1 m1]
      (when-not m (log/warn "exclusiveMaximum: no maximum present to modify"))
      [c1 m1 []]))])

(defn check-property-exclusiveMaximum-new [_property c2 p2 m2 v2]
  [c2
   m2
   (make-new-type-checker
    json-number?
    (fn [c1 p1 m1]
      [c1
       m1
       (when-not (> v2 m1)
         [(make-error "maximum: value too high" p2 m2 p1 m1)])]))])

(defn check-property-divisibleBy [_property c2 p2 m2 v2]
  (let [v2-bd (pbigdec v2)]
    [c2
     m2
     (make-new-type-checker
      json-number?
      (fn [c1 p1 m1]
        [c1
         m1
         (when (not (big-zero? (big-mod (pbigdec m1) v2-bd)))
           [(make-error (pformat "%s is not divisible by of %s" m1 v2) p2 m2 p1 m1)])]))]))

(defn check-property-multipleOf [_property c2 p2 m2 v2]
  (let [v2-bd (pbigdec v2)]
    [c2
     m2
     (make-new-type-checker
      json-number?
      (fn [c1 p1 m1]
        [c1
         m1
         (when (not (big-zero? (big-mod (pbigdec m1) v2-bd)))
           [(make-error (pformat "%s is not a multiple of %s" m1 v2) p2 m2 p1 m1)])]))]))

;; standard string properties

(defn char-code-at [^String str pos]
  #?(:clj (.charAt str (int pos))
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

(defn check-property-minLength [_property c2 p2 m2 v2]
  (let [ml2 (quot v2 2)]
    [c2
     m2
     (make-new-type-checker
      json-string?
      (fn [c1 p1 m1]
        [c1
         m1
         (when (or
                (< (count m1) ml2) ;; precheck before using expensive json-length
                (< (json-length m1) v2))
           [(make-error "minLength: string too short" p2 m2 p1 m1)])]))]))

(defn check-property-maxLength [_property c2 p2 m2 v2]
  (let [ml2 (* v2 2)]
    [c2
     m2
     (make-new-type-checker
      json-string?
      (fn [c1 p1 m1]
        [c1
         m1
         (when (or
                (> (count m1) ml2) ;; precheck before using expensive json-length
                (> (json-length m1) v2))
           [(make-error "maxLength: string too long" p2 m2 p1 m1)])]))]))

;; TODO: entire draft->format->checker table should be picked up from c2
(defn make-check-property-format [strict?]
  (fn [_property {cfs :check-format :or {cfs {}} strict-format? :strict-format? draft :draft :as c2} p2 m2 v2]
    (let [f (if (or strict? strict-format?)
              (fn [f2] (make-type-checker json-string? (fn [c p m] [c m (f2 c p m)])))
              (fn [f2] (make-type-checker json-string? (fn [c p m] (when-let [[{m :message}] (f2 c p m)] [c m (log/warn m)])))))]
      ;; we do this here so that user may override default format checkers...
      ;; First check for custom override, then draft-specific checker, then fallback to multimethod
      [c2
       m2
       (if-let [checker (or (cfs v2)
                            (get-in draft->format->checker [draft v2])
                           ;; Fallback to multimethod for now
                            (fn [_ _ _] (fn [_ _ _] (log/warn "format: not recognised:" draft (pr-str v2)))))]
         (f (checker c2 p2 m2))
        ;; Unknown format - return identity function
         (f (constantly nil)))])))

(defn check-property-pattern [_property c2 p2 m2 v2]
  (if (starts-with? v2 "$format:")
    ;; N.B.
    ;; this is an extension to allow patternProperties to
    ;; leverage formats since the spec does not provide a
    ;; formatProperties...
    ((make-check-property-format false) "format" c2 p2 m2 (subs v2 (count "$format:"))) ;; TODO: decide strictness from context somehow
    (let [p (ecma-pattern v2)]
      [c2
       m2
       (make-new-type-checker
        json-string?
        (fn [c1 p1 m1]
          [c1
           m1
           (when (false? (ecma-match p m1))
             [(make-error "pattern: doesn't match" p2 m2 p1 m1)])]))])))

#?(:clj
   (let [^java.util.Base64$Decoder decoder (java.util.Base64/getDecoder)]
     (defn base64-decode [^String s] (String. (.decode decoder (.getBytes s "UTF-8")) "UTF-8")))

   :cljs
   (let [base64-re #"^[A-Za-z0-9+/]*={0,2}$"]
     (defn base64-decode [s]
       (when-not (re-find base64-re s)
         (throw (ex-info (str "Illegal base64 character") {})))
       (.toString (js/Buffer.from s "base64") "utf8"))))

(def ce->decoder
  {absent identity
   "quoted-printable" identity
   "base16" (fn [_] (throw (ex-info "base16/decode: NYI" {})))
   "base32" (fn [_] (throw (ex-info "base32/decode: NYI" {})))
   "base64" base64-decode})

(def cmt->decoder
  {"application/json" json-decode})

(defn make-check-property-contentEncoding [strict?]
  (fn [_property c2 p2 m2 v2]
    (let [ce-decoder (ce->decoder v2)
          pp2 (butlast p2)]
      [c2
       m2
       (make-new-type-checker
        json-string?
        (fn [c1 p1 old-m1]
          (let [[new-m1 es]
                (try
                  [(ce-decoder old-m1) nil]
                  (catch #?(:cljs js/Error :clj Exception) e
                    [nil
                     (let [m (str "contentEncoding: could not " v2 " decode: " (pr-str old-m1) " - " (ex-message e))]
                       (if strict?
                         [(make-error m p2 m2 p1 old-m1)]
                         (do
                           (log/warn (string-replace m #"\n" " - "))
                           [])))]))]
            (if new-m1
              [(update c1 :content assoc pp2 new-m1) new-m1 nil]
              [c1 old-m1 es]))))])))

(defn make-check-property-contentMediaType [strict?]
  (fn [_property c2 p2 m2 v2]
    (let [cmt v2
          cmt-decoder (cmt->decoder cmt)
          pp2 (butlast p2)]
      [c2
       m2
       (make-new-type-checker
        json-string?
        (fn [c1 p1 m1]
          (let [old-m1 (or (get (get c1 :content) pp2) m1)
                [new-m1 es]
                (try
                  [(cmt-decoder old-m1) nil]
                  (catch #?(:cljs js/Error :clj Exception) e
                    [nil
                     (let [m (str "contentMediaType: could not " v2 " decode: " (pr-str old-m1) " - " (string-replace (ex-message e) #"\n" " \\\\n "))]
                       (if strict?
                         [(make-error m p2 m2 p1 old-m1)]
                         (do
                           (log/warn (string-replace m #"\n" " - "))
                           [])))]))]

            (if new-m1
              [(update c1 :content assoc pp2 new-m1) new-m1 nil]
              [c1 old-m1 es]))))])))

(defn make-check-property-contentSchema [strict?]
  (fn [_property c2 p2 {cmt "contentMediaType" :as m2} v2]
    (let [[c2 m2 checker] ((get-check-schema) c2 p2 v2)
          pp2 (butlast p2)]
      [c2
       m2
       (fn [c1 p1 m1]
         (let [old-m1 (or (get (get c1 :content) pp2) m1)
               old-m1 (if cmt old-m1 (json-decode old-m1))] ;; TODO: error handling
           [c1
            m1
            (try
              (let [[c1 m1 {es :errors :as v}] (checker c1 p1 old-m1)]
                (when (seq es)
                  (if strict?
                    es
                    (log/warn "contentSchema: failed validation - " (prn-str v)))))
              (catch #?(:cljs js/Error :clj Exception) e
                (:errors (ex-data e))))]))])))

(defn check-property-dependencies [_property c2 p2 m2 v2]
  (let [property->checker
        (reduce
         (fn [acc [k v]]
           (assoc
            acc
            k
            (cond
              (json-string? v) ;; a single property dependency
              (fn [c1 _p1 m1] [c1 m1 (when (not (contains? m1 v)) [v v])])
              (json-array? v) ;; a multiple property dependency
              ;; TODO: this looks very suspect
              (fn [c1 _p1 m1] [c1 m1 (reduce (fn [acc2 k2] (if (contains? m1 k2) acc2 (conj acc2 [k k2]))) [] v)])
              (or (json-object? v) (boolean? v)) ;; a schema dependency
              (third ((get-check-schema) c2 p2 v)) ; TODO: we are throwing away c2/m2
              ;; we should not need to check other cases as m2 should have been validated against m3
              )))
         {}
         v2)]
    [c2
     m2
     (make-new-type-checker
      json-object?
      (fn [c1 p1 m1]
        (let [[c1 m1 es]
              (reduce
               (fn [[c m old-es] [k _v]]
                 (if (contains? m k)
                   (let [[c m new-es] ((property->checker k) c p1 m)]
                     [c m (concatv old-es new-es)])
                   [c m old-es]))
               [c1 m1 []]
               v2)]
          [c1
           m1
           (when-let [missing (seq es)]
             [(make-error ["dependencies: missing properties (at least):" missing] p2 m2 p1 m1)])])))]))

(defn check-property-dependentSchemas [_property c2 p2 m2 v2]
  (let [property->checker
        (reduce
         (fn [acc [k v]]
           (assoc acc k (third ((get-check-schema) c2 p2 v)))) ;TODO: we are throwing away c2/m2
         {}
         v2)]
    [c2
     m2
     (make-new-type-checker
      json-object?
      (fn [c1 p1 m1]
        (let [[c1 m1 es]
              (reduce
               (fn [[c m old-es] [k _v]]
                 (if (contains? m k)
                   (let [[c m new-es] ((property->checker k) c p1 m)]
                     [c m (concatv old-es new-es)])
                   [c m old-es]))
               [c1 m1 []]
               v2)]
          [c1
           m1
           (when-let [missing (seq es)]
             [(make-error ["dependentSchemas: missing properties (at least):" missing] p2 m2 p1 m1)])])))]))

(defn check-property-propertyDependencies [_property c2 p2 m2 v2]
  (let [f2 (get-check-schema)
        checkers (into {} (mapcat (fn [[k1 vs]] (map (fn [[k2 s]] [[k1 k2] (third (f2 c2 p2 s))]) vs)) v2))
        ks (keys v2)]
    [c2
     m2
     (make-new-type-checker
      json-object?
      (fn [c1 p1 m1]
        (reduce
         (fn [[c1 m1 old-es] k]
           (let [v (m1 k)]
             (if-let [checker (and (json-string? v) (checkers [k v]))]
               (let [[c1 m1 new-es] (checker c1 p1 m1)]
                 [c1 m1 (concatv old-es new-es)])
               [c1 m1 old-es])))
         [c1 m1 []]
         ks)))]))

;; TODO: share more code with dependencies
(defn check-property-dependentRequired [_property c2 p2 m2 v2]
  (let [property->checker
        (reduce
         (fn [acc [k v]]
           (assoc
            acc
            k
            (fn [_c1 _p1 m1] (reduce (fn [acc2 k2] (if (contains? m1 k2) acc2 (conj acc2 [k k2]))) [] v))))
         {}
         v2)]
    [c2
     m2
     (make-new-type-checker
      json-object?
      (fn [c1 p1 m1]
        [c1
         m1
         (when-let [missing
                    (seq
                     (reduce
                      (fn [acc [k _v]]
                        (if (contains? m1 k)
                          (concatv acc ((property->checker k) c1 p1 m1))
                          acc))
                      []
                      v2))]
           [(make-error ["dependentRequired: missing properties (at least):" missing] p2 m2 p1 m1)])]))]))

;;------------------------------------------------------------------------------

(defn check-property-if [_property c2 p2 m2 v2]
  (let [[c2 m2 f1] ((get-check-schema) c2 p2 v2)
        pp2 (butlast p2)]
    [c2
     m2
     (fn [old-c1 p1 m1]
       (let [[new-c1 m1 es] (f1 old-c1 p1 m1)
             success? (empty? es)]
         [(update (if success? new-c1 old-c1) :if assoc pp2 success?) m1 []]))]))

(defn check-property-then [_property c2 p2 _m2 v2]
  (let [[c2 m2 f1] ((get-check-schema) c2 p2 v2)
        pp2 (butlast p2)]
    [c2
     m2
     (fn [c1 p1 m1]
       (if (true? (get (get c1 :if) pp2))
         (f1 c1 p1 m1)
         [c1 m1 []]))]))

(defn check-property-else [_property c2 p2 _m2 v2]
  (let [[c2 m2 f1] ((get-check-schema) c2 p2 v2)
        pp2 (butlast p2)]
    [c2
     m2
     (fn [c1 p1 m1]
       (if (false? (get (get c1 :if) pp2))
         (f1 c1 p1 m1)
         [c1 m1 []]))]))

;; definitions/$defs
;; m3/m2 time - validate structure of content
;; m2/m1 time - hmmm...
(defn check-property-definitions [_property c2 _p2 m2 _v2]
  [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])])

(defn check-property-$defs [_property c2 _p2 m2 _v2]
  [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])])

;; bifurcate upwards to reduce amount of work done to just what it required...
(defn check-properties [c2 p2 m2]
  (let [pp2 (butlast p2)]
    [c2
     m2
     (fn [c1 p1 m1 k-and-css message]
       (let [[c1 es]
             (reduce
              (fn [[c old-es] [[k cs] sub-document]]
                (let [[c m new-es] (cs c (conj p1 k) sub-document)]
                  [c (concatv old-es new-es)]))
              [c1 []]
              (map (fn [[k :as k-and-cs]] [k-and-cs (m1 k)]) k-and-css))]
         [(let [ks (map first k-and-css)]
            (-> c1
           ;; TODO: only record matched if additonalProperties needed later ?
                (update :matched update pp2 into-set ks)
           ;; TODO: only record evaluated if unevaluatedProperties needed later ?
                (update :evaluated update p1 into-set ks)))
          m1
          (make-error-on-failure message p2 m2 p1 m1 es)]))]))

(defn check-property-properties [_property c2 p2 m2 ps]
  (let [f2 (get-check-schema)
        k-and-css (mapv (fn [[k v]] [k (third (f2 c2 (conj p2 k) v))]) ps)
        [c2 m2 f1] (check-properties c2 p2 m2)]
    [c2
     m2
     (make-new-type-checker
      json-object?
      (fn [c1 p1 m1]
        (let [k-and-css (filter (fn [[k]] (contains? m1 k)) k-and-css)]
          (f1 c1 p1 m1 k-and-css "properties: at least one property did not conform to respective schema"))))]))

;; draft3: "required" is a boolean inside each property sub-schema, not an array on the object.
;; At the sub-schema level this is a no-op, since the checker only fires for present properties.
;; The actual required check is done by check-property-properties-draft3 below.
(defn check-property-required-draft3 [_property c2 _p2 m2 _v2]
  [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])])

;; draft3: variant of check-property-properties that also enforces "required": true
;; in property sub-schemas. Extracts required keys at compile time.
;; TODO: review when context-stashing (json-walk removal) work solidifies - required keys
;; could be gathered during M3/M2 validation pass instead of scanning sub-schemas here.
(defn check-property-properties-draft3 [_property c2 p2 m2 ps]
  (let [f2 (get-check-schema)
        k-and-css (mapv (fn [[k v]] [k (third (f2 c2 (conj p2 k) v))]) ps)
        required-keys (vec (keep (fn [[k v]] (when (true? (get v "required")) k)) ps))
        [c2 m2 f1] (check-properties c2 p2 m2)]
    [c2
     m2
     (make-new-type-checker
      json-object?
      (fn [c1 p1 m1]
        (let [k-and-css (filter (fn [[k]] (contains? m1 k)) k-and-css)
              [c1 m1 prop-es] (f1 c1 p1 m1 k-and-css "properties: at least one property did not conform to respective schema")
              missing (seq (remove #(contains? m1 %) required-keys))
              req-es (when missing [(make-error ["required: missing properties (at least):" (vec missing)] p2 m2 p1 m1)])]
          [c1 m1 (concatv prop-es req-es)])))]))

;; what is opposite of "additional" - "matched" - used by spec to refer to properties matched by "properties" or "patternProperties"

(defn check-property-patternProperties [_property c2 p2 m2 pps]
  (let [f2 (get-check-schema)
        cp-and-pattern-and-ks (mapv (fn [[k v]] [(third (f2 c2 (conj p2 k) v)) (ecma-pattern k) k]) pps)
        [c2 m2 f1] (check-properties c2 p2 m2)]
    [c2
     m2
     (make-new-type-checker
      json-object?
      (fn [c1 p1 m1]
        (let [k-and-css (apply concat (keep (fn [[k]] (keep (fn [[cs p]] (when (ecma-match p k) [k cs])) cp-and-pattern-and-ks)) m1))]
          (f1 c1 p1 m1 k-and-css "patternProperties: at least one property did not conform to respective schema"))))]))

(defn check-property-additionalProperties [_property c2 p2 m2 v2]
  (let [f2 (get-check-schema)
        [c2 m2 cs] (f2 c2 p2 v2)
        pp2 (butlast p2)
        [c2 m2 f1] (check-properties c2 p2 m2)]
    [c2
     m2
     (make-new-type-checker
      json-object?
      (fn [c1 p1 m1]
        (let [mps (get (get c1 :matched) pp2 #{})
              aps (remove (fn [[k]] (contains? mps k)) m1) ;; k might be nil
              p-and-css (mapv (fn [[k]] [k cs]) aps)] ; TODO: feels inefficient
          (f1 c1 p1 m1 p-and-css "additionalProperties: at least one property did not conform to schema"))))]))

(defn check-property-unevaluatedProperties [_property c2 p2 m2 v2]
  (let [f2 (get-check-schema)
        [c2 m2 cs] (f2 c2 p2 v2)
        [c2 m2 f1] (check-properties c2 p2 m2)]
    [c2
     m2
     (make-new-type-checker
      json-object?
      (fn [c1 p1 m1]
        (let [eps (get (get c1 :evaluated) p1 #{})
              ups (remove (fn [[k]] (contains? eps k)) m1) ;; k might be nil
              p-and-css (mapv (fn [[k]] [k cs]) ups)] ; TODO: feels inefficient
          (f1 c1 p1 m1 p-and-css "unevaluatedProperties: at least one property did not conform to schema"))))]))

(defn check-property-propertyNames [_property c2 p2 m2 v2]
  [c2
   m2
   (fn [c1 p1 m1]
     [c1
      m1
      (when (json-object? m1)
        (make-error-on-failure
         "propertyNames: at least one property's name failed to conform to relevant schema"
         p2 m2 p1 m1
         (reduce-kv
          (fn [acc k1 _v1]
            (let [[c2 m2 f1] ((get-check-schema) c2 (conj p2 k1) v2)
                  [_c1 _m1 es] (f1 c1 (conj p1 k1) k1)]
              (concatv acc es)))
          []
          m1)))])])

(defn check-property-required [_property c2 p2 m2 v2]
  [c2
   m2
   (make-new-type-checker
    json-object?
    (fn [c1 p1 m1]
      [c1
       m1
       (when-let [missing (seq (reduce (fn [acc k] (if (contains? m1 k) acc (conj acc k))) [] v2))]
         [(make-error ["required: missing properties (at least):" missing] p2 m2 p1 m1)])]))])

(defn check-property-minProperties [_property c2 p2 m2 v2]
  [c2
   m2
   (make-new-type-checker
    json-object?
    (fn [c1 p1 m1]
      [c1
       m1
       (when (< (count m1) v2)
         [(make-error "minProperties: document contains too few properties" p2 m2 p1 m1)])]))])

(defn check-property-maxProperties [_property c2 p2 m2 v2]
  [c2
   m2
   (make-new-type-checker
    json-object?
    (fn [c1 p1 m1]
      [c1
       m1
       (when (> (count m1) v2)
         [(make-error "maxProperties: document has too many properties" p2 m2 p1 m1)])]))])

;; standard array properties

;; we could save time by only maintaining :matched and :evaluated
;; context if required (additional and evaluated items)...

(defn check-items [c2 p2 m2]
  (let [pp2 (butlast p2)]
    [c2
     m2
     (fn [c1 p1 m1 i-and-css message]
       (let [old-local-c1
             (-> c1
                 (update :matched assoc pp2 #{})
                 (update :evaluated assoc p1 #{}))
             [c1 es]
             (reduce
              (fn [[old-c old-es] [[i cs] sub-document]]
                (let [[_ _ new-es] (cs old-local-c1 (conj p1 i) sub-document)
                      new-c (if (empty? new-es)
                              (-> old-c
                                  (update :matched update pp2 conj-set i)
                                  (update :evaluated update p1 conj-set i))
                              old-c)]
                  [new-c (concatv old-es new-es)]))
              [c1 []]
              (map vector i-and-css m1))]
         [c1 m1 (make-error-on-failure message p2 m2 p1 m1 es)]))]))

(defn tweak [m1 [c1 es]]
  [c1 m1 es])

(defn check-property-prefixItems [_property c2 p2 m2 v2]
  (let [f2 (get-check-schema)
        i-and-css (vec (map-indexed (fn [i sub-schema] [i (third (f2 c2 (conj p2 i) sub-schema))]) v2))
        [c2 m2 f1] (check-items c2 p2 m2)]
    [c2
     m2
     (make-new-type-checker
      json-array?
      (fn [c1 p1 m1]
        (f1 c1 p1 m1 i-and-css "prefixItems: at least one item did not conform to respective schema")))]))

(defn check-property-items [_property {d :draft :as c2} p2 m2 v2]
  (let [f2 (get-check-schema)
        n (count (m2 "prefixItems")) ;; TODO: achieve this by looking at c1 ?
        [m css] (if (json-array? v2)
                  (do
                    (case d
                      (:draft3 :draft4 :draft6 :draft7 :draft2019-09)
                      nil
                      (:draft2020-12 :draft-next)
                      (log/info (str "prefixItems: was introduced in draft2020-12 to handle tuple version of items - you are using: " d)))
                    ["respective " (map-indexed (fn [i v] (third (f2 c2 (conj p2 i) v))) v2)])
                  ["" (repeat (third (f2 c2 p2 v2)))])
        [c2 m2 f1] (check-items c2 p2 m2)]
    [c2
     m2
     (make-new-type-checker
      json-array?
      (fn [c1 p1 m1]
        (let [items (drop n m1)
              i-and-css (mapv (fn [i cs _] [(+ i n) cs]) (range) css items)
              [c1 _m1 es] (f1 c1 p1 items i-and-css (str "items: at least one item did not conform to " m "schema"))] ;; TODO: why can we not pass m1 straight through ?
          [c1 m1 es])))]))

(defn check-property-additionalItems [_property c2 p2 {is "items" :as m2} v2]
  (if (json-array? is) ;; additionalItems is only used when items is a tuple
    (let [f2 (get-check-schema)
          [c2 m2 cs] (f2 c2 p2 v2)
          ;;pp2 (butlast p2)
          [c2 m2 f1] (check-items c2 p2 m2)]
      [c2
       m2
       (make-new-type-checker
        json-array?
        (fn [c1 p1 m1]
          (let [;; this is how it should be done, but cheaper to just look at items (must be array for additionalItems to be meaningful) in m2 time
                 ;;mis (get (get c1 :matched) pp2 #{})
                 ;;ais  (remove (fn [[k]] (contains? mis k)) (map-indexed vector m1))
                 ;;i-and-css (mapv (fn [[k]] [k cs]) ais) ; TODO: feels inefficient
                n (count is)
                ais (drop n m1)
                i-and-css (mapv (fn [i cs _] [(+ i n) cs]) (range) (repeat cs) ais)]
            (f1 c1 p1 ais i-and-css "additionalItems: at least one item did not conform to schema"))))])
    [c2
     m2
     (fn [c1 _p1 m1]
       [c1 m1 nil])]))

(defn check-property-unevaluatedItems [_property c2 p2 m2 v2]
  (let [f2 (get-check-schema)
        css (repeat (third (f2 c2 p2 v2)))
        [c2 m2 f1] (check-items c2 p2 m2)]
    [c2
     m2
     (make-new-type-checker
      json-array?
      (fn [{p->eis :evaluated :as c1} p1 m1]
        (let [eis (or (get p->eis p1) #{})
              index-and-items (filter (fn [[k]] (not (eis k))) (map-indexed (fn [i v] [i v]) m1))
              i-and-css (mapv (fn [cs [i]] [i cs]) css index-and-items)] ;; TODO: item not used
          (f1 c1 p1 (map second index-and-items) i-and-css "unevaluatedItems: at least one item did not conform to schema"))))]))

(defn check-property-contains [_property c2 p2 {mn "minContains" :as m2} v2]
  (let [f2 (get-check-schema)
        [c2 m2 cs] (f2 c2 p2 v2)
        base (if mn mn 1)
        [c2 _v2 f1] (check-items c2 p2 v2)]
    [c2
     m2
     (make-new-type-checker
      json-array?
      (fn [c1 p1 m1]
        (let [i-and-css (map (fn [i _] [i cs]) (range) m1)
              [new-c1 _m1 [{es :errors}]]
              (f1 c1 p1 m1 i-and-css "contains: at least one item did not conform to schema")
              matches (- (count m1) (count es))]
          (if (<= (min base 1) matches)
            [new-c1 m1 nil]
            [c1 m1 [(make-error "contains: document has no matches" p2 m2 p1 m1)]]))))]))

(defn check-property-minContains [_property c2 p2 m2 v2]
  (let [pp2 (butlast p2)]
    [c2
     m2
     (make-new-type-checker
      json-array?
      (fn [{matched :matched :as c1} p1 m1]
        (if-let [matches (get matched pp2)]
          (let [n (count matches)]
            (if (and
                 matches
                 (<= v2 n))
              [c1 m1 nil]
              [c1 m1 [(make-error (str "minContains: document has too few matches - " n) p2 m2 p1 m1)]]))
          [c1 m1 nil])))]))

(defn check-property-maxContains [_property c2 p2 m2 v2]
  (let [pp2 (butlast p2)]
    [c2
     m2
     (make-new-type-checker
      json-array?
      (fn [{matched :matched :as c1} p1 m1]
        (if-let [matches (get matched pp2)]
          (let [n (count matches)]
            (if (<= n v2)
              [c1 m1 nil]
              [c1 m1 [(make-error (str "maxContains: document has too many matches - " n) p2 m2 p1 m1)]]))
          [c1 m1 nil])))]))

(defn check-property-minItems [_property c2 p2 m2 v2]
  [c2
   m2
   (make-new-type-checker
    json-array?
    (fn [c1 p1 m1]
      [c1
       m1
       (when (< (count m1) v2)
         [(make-error "minItems: document contains too few items" p2 m2 p1 m1)])]))])

(defn check-property-maxItems [_property c2 p2 m2 v2]
  [c2
   m2
   (make-new-type-checker
    json-array?
    (fn [c1 p1 m1]
      [c1
       m1
       (when (> (count m1) v2)
         [(make-error "maxItems: document contains too many items" p2 m2 p1 m1)])]))])

;; TODO: should be unique according to json equality
;; TODO: could be more efficient - only needs to find one duplicate before it bails..
;; TODO: use sorted-set-by and put items in 1 at a time until one is rejected then bail - reduced
;; TODO: shouldn't this be using check-items ?
(defn check-property-uniqueItems [_property c2 p2 m2 v2]
  [c2
   m2
   (if v2
     (make-new-type-checker
      json-array?
      (fn [c1 p1 m1]
        [c1
         m1
         (when (not (= (count m1) (count (distinct m1))))
           [(make-error "uniqueItems: document contains duplicate items" p2 m2 p1 m1)])]))
     (fn [c1 _p1 m1]
       [c1 m1 nil]))])

;; TODO: merge code with check-items...
(defn check-of [c2 p2 m2 v2]
  (let [f2 (get-check-schema)
        i-and-css (vec (map-indexed (fn [i sub-schema] [i (third (f2 c2 (conj p2 i) sub-schema))]) v2))]
    (fn [c1 p1 m1 message failed?]
      (let [old-local-c1 (update c1 :evaluated dissoc p1)
            [c1 es]
            (reduce
             (fn [[old-c old-es] [_i cs]]
               (let [[new-local-c1 m1 new-es] (cs old-local-c1 p1 m1)
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
    [c2
     m2
     (fn [c1 p1 m1]
       (tweak
        m1
        (co
         c1 p1 m1
         "oneOf: document failed to conform to one and only one sub-schema"
         (fn [es] (not= 1 (- m2-count (count es)))))))]))

(defn check-property-anyOf [_property c2 p2 m2 v2]
  (let [co (check-of c2 p2 m2 v2)
        m2-count (count v2)]
    [c2
     m2
     (fn [c1 p1 m1]
       (tweak
        m1
        (co
         c1 p1 m1
         "anyOf: document failed to conform to at least one sub-schema"
         (fn [es] (not (< (count es) m2-count))))))]))

(defn check-property-allOf [_property c2 p2 m2 v2]
  (let [co (check-of c2 p2 m2 v2)]
    [c2
     m2
     (fn [c1 p1 m1]
       (tweak
        m1
        (co
         c1 p1 m1
         "allOf: document failed to conform to all sub-schemas"
         seq)))]))

(defn check-property-extends [_property c2 p2 m2 v2]
  (let [schemas (if (sequential? v2) v2 [v2])
        co (check-of c2 p2 m2 schemas)]
    [c2
     m2
     (fn [c1 p1 m1]
       (tweak
        m1
        (co
         c1 p1 m1
         "extends: document failed to conform to all extended schemas"
         seq)))]))

;; TODO: share check-of
(defn check-property-not [_property c2 p2 m2 v2]
  (let [[c2 m2 f1] ((get-check-schema) c2 p2 v2)]
    [c2
     m2
     (fn [c1 p1 m1]
       ;; TODO: this is confused - needs a unit test
       (let [old-local-c1 (update c1 :evaluated dissoc p1)
             [new-local-c1 m1 es] (f1 old-local-c1 p1 m1)
             [c1 failed?] (if (seq es)
                            [(update c1 :evaluated update p1 into-set (get (get new-local-c1 :evaluated) p1)) true]
                            [c1 false])]
         [c1
          m1
          (when-not failed?
            [(make-error "not: document conformed to sub-schema" p2 m2 p1 m1)])]))]))
