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
   #?(:cljs [goog.string.format])
   [clojure.string :refer [starts-with? replace] :rename {replace string-replace}]
   [#?(:clj clojure.tools.logging :cljs m3.log) :as log]
   [m3.platform :refer [pformat json-decode big-zero? big-mod pbigdec]]
   [m3.util :refer [absent present? concatv into-set conj-set seq-contains? make-error make-error-on make-error-on-failure get-check-schema third]]
   [m3.ecma :refer [ecma-pattern ecma-match]]
   [m3.uri :refer [parse-uri inherit-uri]]
   [m3.ref :refer [resolve-uri try-path]]
   [m3.type :refer [json-number? json-string? json-array? json-object? check-type make-type-checker json-=]]
   [m3.format :as format]))

;;------------------------------------------------------------------------------
;; shared compilation helper

(defn- compile-sub-schemas
  "Compile sub-schemas into [c2 [[key f1] ...]].
   entries is a seq of [key sub-schema] pairs.
   Returns [c2 k-and-checkers] with id-uri restored to parent scope."
  [c2 p2 entries]
  (let [f2 (get-check-schema)
        parent-id-uri (:id-uri c2)
        [c2 acc]
        (reduce
         (fn [[c2 acc] [k v]]
           (let [[c2 _m2 f1] (f2 (assoc c2 :id-uri parent-id-uri) (conj p2 k) v)]
             [c2 (conj acc [k f1])]))
         [c2 []]
         entries)]
    [(assoc c2 :id-uri parent-id-uri) acc]))

;;------------------------------------------------------------------------------
;; standard common properties

(defn- noop-checker [_property c2 _p2 m2 _v2] [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])])

;; check-property-extends is defined later, near check-property-allOf
(declare check-property-extends)

(defn make-check-property-disallow [type->checker]
  (fn [_property c2 p2 m2 v2]
    (let [[c2 m2 f1] (check-type type->checker v2 c2 p2 m2)]
      [c2
       m2
       (fn [c1 p1 m1]
         (let [[c1 m1 es] (f1 c1 p1 m1)]
           [c1
            m1
            (when (nil? es) [(make-error "disallow: type matched" p2 m2 p1 m1)])]))])))

(defn make-check-property-type [type->checker]
  (fn [_property c2 p2 m2 v2]
    (check-type type->checker v2 c2 p2 m2)))

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

(defn- make-check-property-id [id-key]
  (fn [_property c2 p2 m2 v2]
    (let [schema-p2 (vec (butlast p2))
          old-id-uri (:id-uri c2)
          new-id-uri (inherit-uri old-id-uri (parse-uri v2))
          c2 (-> c2
                 (assoc :id-uri new-id-uri)
                 (update :uri->path assoc new-id-uri schema-p2)
                 (update :path->uri assoc schema-p2 new-id-uri))]
      [c2
       m2
       (fn [{old-id-uri :id-uri :as c1} p1 m1]
         (let [id (get m1 id-key)]
           [(if-let [new-id-uri (and id (inherit-uri old-id-uri (parse-uri id)))]
              (-> c1
                  (update :path->uri assoc p1 new-id-uri)
                  (update :uri->path assoc new-id-uri p1)
                  (assoc :id-uri new-id-uri))
              (-> c1
                  (update :path->uri assoc p1 old-id-uri)))
            m1
            nil]))])))

(def check-property-id (make-check-property-id "id"))
(def check-property-$id (make-check-property-id "$id"))

(defn check-property-$anchor [_property c2 p2 m2 v2]
  (let [schema-p2 (vec (butlast p2))
        anchor-uri (inherit-uri (:id-uri c2) (parse-uri (str "#" v2)))
        c2 (update c2 :uri->path assoc anchor-uri schema-p2)]
    [c2
     m2
     (fn [c1 p1 m1]
       (let [anchor-uri (inherit-uri (c1 :id-uri) (parse-uri (str "#" v2)))]
         [(update c1 :uri->path assoc anchor-uri p1) m1 nil]))]))

(defn check-property-$recursiveAnchor [_property c2 p2 m2 v2]
  (let [schema-p2 (vec (butlast p2))
        ;; Capture compile-time context for runtime dynamic scope.
        ;; When $recursiveRef resolves at runtime, it needs the schema and
        ;; compilation context of the outermost $recursiveAnchor — which may
        ;; differ from what's visible at compile time (e.g. the $recursiveRef
        ;; is inside a separately-compiled remote schema).
        ;; We store the scope-id so that at runtime we can look up
        ;; the FINAL c2 with all stash entries from c1, not an early snapshot.
        anchor-scope-id (:scope-id c2)
        anchor-schema m2
        c2 (if (true? v2)
             (let [[uris top] (:$recursive-anchor c2 [#{} nil])]
               (assoc c2 :$recursive-anchor
                      [(conj uris (:id-uri c2))
                       ;; Keep the shallowest (outermost) path as top.
                       ;; $defs may compile nested anchors before the root's
                       ;; own anchor runs, so we compare path depth.
                       (if (or (nil? top) (< (count schema-p2) (count top)))
                         schema-p2
                         top)]))
             c2)]
    [c2
     m2
     ;; Also stash into c1 at runtime for document validation
     (fn [c1 p1 m1]
       (if (true? v2)
         (let [[uris top] (c1 :$recursive-anchor [#{} nil])]
           [(-> c1
                (assoc :$recursive-anchor [(conj uris (c1 :id-uri)) (or top p1)])
                ;; Store the outermost anchor's schema and scope-id for $recursiveRef.
                ;; Only set once — the first $recursiveAnchor f1 to run is the
                ;; outermost in the dynamic scope (validation descends outer→inner).
                (update :$recursive-anchor-schema
                        (fn [existing]
                          (or existing {:schema anchor-schema :scope-id anchor-scope-id}))))
            m1 nil])
         [c1 m1 nil]))]))

(defn check-property-$dynamicAnchor [_property c2 p2 m2 v2]
  ;; $dynamicAnchor also acts as a regular $anchor for $ref resolution
  ;; (dynamic behavior only applies to $dynamicRef)
  (let [schema-p2 (vec (butlast p2))
        anchor-uri (inherit-uri (:id-uri c2) (parse-uri (str "#" v2)))
        ;; Capture compile-time context for runtime dynamic scope.
        ;; Like $recursiveAnchor, we store schema+scope-id so that
        ;; $dynamicRef can look up the final c2 from c1 at runtime.
        anchor-scope-id (:scope-id c2)
        anchor-schema m2
        c2 (-> c2
               (update-in [:$dynamic-anchor anchor-uri]
                          (fn [old new] (if old old new)) schema-p2)
               (update :uri->path assoc anchor-uri schema-p2))]
    [c2
     m2
     ;; Also stash into c1 at runtime for document validation
     (fn [c1 p1 m1]
       (let [anchor-uri (inherit-uri (c1 :id-uri) (parse-uri (str "#" v2)))]
         [(-> c1
              (update :$dynamic-anchor assoc anchor-uri p1)
              (update :uri->path assoc anchor-uri p1)
              ;; Store outermost dynamic anchor per name for runtime dynamic scope.
              ;; Only the first (outermost) anchor wins — like $recursiveRef.
              (update-in [:$dynamic-anchor-schema v2]
                         (fn [existing]
                           (or existing {:schema anchor-schema :scope-id anchor-scope-id}))))
          m1 nil]))]))

(defn check-property-$comment [_property {quiet? :quiet? :as c2} _p2 m2 v2]
  [c2
   m2
   (if quiet?
     (fn [c1 _p1 m1] [c1 m1 nil])
     (fn [c1 _p1 m1]
       (log/info (str "$comment: " v2))
       [c1 m1 nil]))])

;; $ref resolution logic.
;; Resolution is LAZY (inside f1) to handle recursive schemas.
(defn make-check-property-$ref [{:keys [meld-fn ref-replaces-siblings? ref-scope-isolation?]}]
  (fn [_property {id-uri :id-uri :as c2} p2 m2 v2]
    (let [schema-p2 (vec (butlast p2))
          ;; For draft <= draft7, $ref ignores sibling $id.
          ;; Use schema-parent-id-uri saved by compile-m2 at entry — this is
          ;; the id-uri BEFORE this schema's $id changed it.
          effective-id-uri (if ref-replaces-siblings?
                             (or (:schema-parent-id-uri c2) id-uri)
                             id-uri)
          ref-uri (inherit-uri effective-id-uri (parse-uri v2))
          m2-no-ref (dissoc m2 "$ref")
          effective-c2 (assoc c2 :id-uri effective-id-uri)]
      [(if ref-replaces-siblings?
         ;; For old drafts, $ref overrides siblings — tell compile-m2 to skip them
         (assoc c2 :skip-keys (set (keys m2-no-ref)))
         c2)
       m2
       ;; f1: resolve and compile LAZILY at runtime
       (fn [c1 p1 m1]
         (if (present? m1)
           (let [check-schema-fn (get-check-schema)
                 ;; Use late-bound c2 for resolution: after compile-m2 finishes,
                 ;; the final c2 with stash entries from ALL property compilations
                 ;; is stored in c1 under [:$compile-scopes scope-id].
                 resolution-c2 (if-let [sid (:scope-id effective-c2)]
                                 (if-let [scope-c2 (get-in c1 [:$compile-scopes sid])]
                                   (assoc effective-c2
                                          :uri->path (:uri->path scope-c2)
                                          :path->uri (:path->uri scope-c2))
                                   effective-c2)
                                 effective-c2)
                 ;; Cross-resource fallback: when the current root is a sub-schema
                 ;; (e.g. from $recursiveRef dynamic resolution), the uri->path stash
                 ;; may contain paths relative to the original root that don't exist
                 ;; in the current root. Retry against the cross-resource-root.
                 resolution (or (resolve-uri resolution-c2 schema-p2 ref-uri v2 true)
                                (when-let [cr-root (:cross-resource-root resolution-c2)]
                                  (resolve-uri (assoc resolution-c2 :root cr-root)
                                               schema-p2 ref-uri v2)))]
             (if resolution
               (let [[new-c _new-p resolved-m] resolution
                     melded (meld-fn effective-c2 m2-no-ref resolved-m)
                     same-root? (identical? (:root effective-c2) (:root new-c))
                     ;; For draft2019+, $ref creates its own annotation scope.
                     ;; When unevaluated* keywords are involved (in either the
                     ;; ref'd schema or siblings), compile the ref'd schema alone
                     ;; so annotations don't leak across scope boundaries.
                     ;; Siblings are evaluated separately by compile-m2.
                     ;; For pre-2019 drafts, $ref replaces siblings — always meld.
                     needs-scope-isolation?
                     (and ref-scope-isolation?
                          (or (and (map? resolved-m)
                                   (or (contains? resolved-m "unevaluatedProperties")
                                       (contains? resolved-m "unevaluatedItems")))
                              (contains? m2-no-ref "unevaluatedProperties")
                              (contains? m2-no-ref "unevaluatedItems")))
                     ;; When scope-isolating, place resolved-m (not melded) in the
                     ;; root so that nested $refs (e.g. "#") see the ref'd schema,
                     ;; not the meld which would re-introduce parent keywords.
                     root-schema (if needs-scope-isolation? resolved-m melded)
                     ref-c2 (cond
                              (and same-root? (seq schema-p2)
                                   (some? (get-in (:root effective-c2) schema-p2)))
                              (-> effective-c2
                                  (assoc-in (into [:root] schema-p2) root-schema)
                                  (assoc :id-uri (:id-uri new-c)))
                              (and same-root? (get melded (:id-key effective-c2)))
                              (-> effective-c2
                                  (assoc :root root-schema)
                                  (assoc :id-uri (:id-uri new-c)))
                              same-root?
                              (assoc effective-c2 :id-uri (:id-uri new-c))
                              :else new-c)
                     compile-schema (if needs-scope-isolation? resolved-m melded)
                     [_c2 _m2 compiled-f1] (check-schema-fn (dissoc ref-c2 :scope-id) schema-p2 compile-schema)]
                 (compiled-f1 c1 p1 m1))
               ;; Resolution failed - compile schema without $ref
               (let [[_c2 _m2 compiled-f1] (check-schema-fn (dissoc effective-c2 :scope-id) schema-p2 m2-no-ref)]
                 (compiled-f1 c1 p1 m1))))
           [c1 m1 nil]))])))

;; check-property-$schema lives in m3.vocabulary to avoid deref/resolve circularity.
;; It needs make-dialect and draft->default-dialect which are defined there.

;; check-property-$vocabulary also lives in m3.vocabulary for the same reason.

(defn check-property-$recursiveRef [_property {id-uri :id-uri :as c2} p2 m2 v2]
  (let [schema-p2 (vec (butlast p2))]
    [c2
     m2
     ;; f1: resolve LAZILY at runtime
     (fn [c1 p1 m1]
       (if (present? m1)
         (if (= "#" v2)
           (let [check-schema-fn (get-check-schema)
                 m2-no-ref (dissoc m2 "$recursiveRef")
                 ;; Step 1: resolve "#" via c2 to find the initial target.
                 ;; Use late-bound c2 for resolution (same as $ref/$dynamicRef).
                 resolution-c2 (if-let [sid (:scope-id c2)]
                                 (if-let [scope-c2 (get-in c1 [:$compile-scopes sid])]
                                   (assoc c2
                                          :uri->path (:uri->path scope-c2)
                                          :path->uri (:path->uri scope-c2))
                                   c2)
                                 c2)
                 ref-uri (or id-uri (parse-uri v2))
                 [c2-uris c2-top] (:$recursive-anchor resolution-c2)
                 initial (or (try-path resolution-c2 schema-p2 (and c2-uris (c2-uris id-uri) c2-top) (:original-root resolution-c2))
                             (resolve-uri resolution-c2 schema-p2 ref-uri v2))
                 ;; Step 2: check runtime dynamic scope (c1) for outermost anchor.
                 ;; The c1 dynamic override is needed when the $recursiveRef is
                 ;; inside a schema whose c2 can't redirect to the correct outer
                 ;; anchor — e.g. separately compiled schemas, or when c2-top
                 ;; points to the wrong branch (multi-path if/then/else).
                 ;; We use scope-id to look up the FINAL compilation
                 ;; context with all stash entries from c1.
                 dynamic (:$recursive-anchor-schema c1)
                 initial-schema (when initial (nth initial 2))
                 use-dynamic? (and dynamic
                                   (map? initial-schema)
                                   (true? (get initial-schema "$recursiveAnchor")))
                 resolution (if use-dynamic?
                              (let [{dyn-schema :schema dyn-sid :scope-id} dynamic
                                    dyn-c2 (when dyn-sid (get-in c1 [:$compile-scopes dyn-sid]))]
                                (when dyn-c2
                                  [dyn-c2 schema-p2 dyn-schema]))
                              initial)]
             (if resolution
               (let [[new-c _new-p resolved-m] resolution
                     ;; Compile the resolved schema on its own, without melding
                     ;; with m2-no-ref. Melding would cause the resolved schema's
                     ;; inner $ref to cascade-meld and lose array-valued keywords
                     ;; (like items) from the resolved schema.
                     ;; Build c2 for compiling the resolved schema.
                     ;; Dynamic: the resolved schema is a complete root — treat
                     ;; it as a standalone schema compiled at [].
                     ;; Non-dynamic: graft resolved-m into the existing root
                     ;; at schema-p2 so sibling $refs still resolve.
                     resolved-c2 (if use-dynamic?
                                   (assoc new-c
                                          :root resolved-m
                                          :original-root resolved-m
                                          ;; Preserve the true original root for cross-resource
                                          ;; $ref resolution. The standalone root (resolved-m) may
                                          ;; not contain sibling definitions from the original root.
                                          :cross-resource-root (:original-root new-c))
                                   (if (and (= c2 new-c)
                                            (or (empty? schema-p2)
                                                (some? (get-in (:root c2) schema-p2))))
                                     (assoc-in c2 (into [:root] schema-p2) resolved-m)
                                     new-c))
                     compile-p2 (if use-dynamic? [] schema-p2)
                     [_c2a _m2a f1a] (check-schema-fn (dissoc resolved-c2 :scope-id) compile-p2 resolved-m)
                     ;; Apply the resolved schema to get evaluation state
                     [c1a _m1a es-a] (f1a c1 p1 m1)
                     ;; Then apply the non-ref sibling keywords (e.g.
                     ;; unevaluatedItems) using c1a which carries the
                     ;; evaluation state from the resolved schema.
                     [c1b _m1b es-b] (if (seq m2-no-ref)
                                       (let [[_c2b _m2b f1b] (check-schema-fn (dissoc c2 :scope-id) schema-p2 m2-no-ref)]
                                         (f1b c1a p1 m1))
                                       [c1a m1 nil])]
                 [c1b m1 (concatv es-a es-b)])
               (let [[_c2 _m2 compiled-f1] (check-schema-fn (dissoc c2 :scope-id) schema-p2 m2-no-ref)]
                 (compiled-f1 c1 p1 m1))))
           (do (log/warn "$recursiveRef: unexpected value:" (pr-str v2))
               [c1 m1 nil]))
         [c1 m1 nil]))]))

(defn- resolve-dynamic-anchor-from-root-c2
  "Fall back to the root schema's compile-time $dynamic-anchor map.
  Used when the $dynamicAnchor was in $defs (whose f1 is a no-op at runtime)
  so the anchor never made it into c1."
  [c1 anchor-name]
  (when-let [root-sid (:$root-scope-id c1)]
    (let [root-c2 (get-in c1 [:$compile-scopes root-sid])
          da (:$dynamic-anchor root-c2)]
      (when da
        (some (fn [[uri path]]
                (when (= (:fragment uri) anchor-name)
                  (let [schema (get-in (:root root-c2) path)]
                    (when (and (map? schema)
                               (= (get schema "$dynamicAnchor") anchor-name))
                      {:schema schema :scope-id root-sid}))))
              da)))))

(defn make-check-property-$dynamicRef [dynamic-ref-requires-bookend?]
  (fn [_property {id-uri :id-uri :as c2} p2 m2 v2]
    (let [schema-p2 (vec (butlast p2))
          uri (inherit-uri id-uri (parse-uri v2))
          ;; Extract anchor name from fragment (for dynamic scope lookup).
          ;; Only plain-name fragments are anchors; JSON pointers (starting
          ;; with "/") are NOT dynamic anchors.
          anchor-name (let [f (:fragment (parse-uri v2))]
                        (when (and f (not (starts-with? f "/")))
                          f))
          needs-bookend? dynamic-ref-requires-bookend?]
      [c2
       m2
     ;; f1: resolve LAZILY at runtime
     (fn [c1 p1 m1]
       (if (present? m1)
         (let [check-schema-fn (get-check-schema)
               m2-no-ref (dissoc m2 "$dynamicRef")
               ;; Use late-bound c2 for resolution: after compile-m2 finishes,
               ;; the final c2 with stash entries from ALL property compilations
               ;; is stored in c1 under [:$compile-scopes scope-id].
               resolution-c2 (if-let [sid (:scope-id c2)]
                               (if-let [scope-c2 (get-in c1 [:$compile-scopes sid])]
                                 (assoc c2
                                        :uri->path (:uri->path scope-c2)
                                        :path->uri (:path->uri scope-c2))
                                 c2)
                               c2)
               ;; Step 1: Static resolution (find initial target / bookend).
               ;; Use resolve-uri only — the $dynamic-anchor c2 shortcut can
               ;; find the wrong schema when meld mixes $defs from different
               ;; resources into the root.
               ;; Quiet: static resolution failure is expected when
               ;; $dynamicRef has a dynamic fallback path.
               initial (resolve-uri resolution-c2 schema-p2 uri v2 true)
               ;; Step 2: Check bookending — the initial target must have
               ;; a $dynamicAnchor whose name matches the fragment.
               ;; If not bookended, $dynamicRef behaves as a normal $ref.
               ;; draft-next: bookending always passes when anchor-name exists.
               initial-schema (when initial (nth initial 2))
               bookended? (and anchor-name
                               (if needs-bookend?
                                 (and (map? initial-schema)
                                      (= (get initial-schema "$dynamicAnchor") anchor-name))
                                 true))
               ;; Step 3: Dynamic resolution (if bookended).
               ;; Check c1 runtime scope first (outermost anchor wins),
               ;; then fall back to root schema's compile-time stash
               ;; (for anchors in $defs whose f1 never ran).
               dynamic-override (when bookended?
                                  (or (get-in c1 [:$dynamic-anchor-schema anchor-name])
                                      (resolve-dynamic-anchor-from-root-c2 c1 anchor-name)))
               use-dynamic? (boolean dynamic-override)
               resolution (if use-dynamic?
                            (let [{dyn-schema :schema dyn-sid :scope-id} dynamic-override
                                  dyn-c2 (when dyn-sid (get-in c1 [:$compile-scopes dyn-sid]))]
                              (when dyn-c2
                                [dyn-c2 schema-p2 dyn-schema]))
                            initial)]
           (if resolution
             (let [[new-c _new-p resolved-m] resolution
                   ;; Use separate compilation (like $recursiveRef) to avoid
                   ;; the meld cascade that loses array-valued keywords.
                   resolved-c2 (if use-dynamic?
                                 (assoc new-c
                                        :root resolved-m
                                        :original-root resolved-m
                                        :cross-resource-root (:original-root new-c))
                                 (if (and (= c2 new-c)
                                          (or (empty? schema-p2)
                                              (some? (get-in (:root c2) schema-p2))))
                                   (assoc-in c2 (into [:root] schema-p2) resolved-m)
                                   new-c))
                   compile-p2 (if use-dynamic? [] schema-p2)
                   [_c2a _m2a f1a] (check-schema-fn (dissoc resolved-c2 :scope-id) compile-p2 resolved-m)
                   ;; Apply the resolved schema to get evaluation state
                   [c1a _m1a es-a] (f1a c1 p1 m1)
                   ;; Then apply the non-ref sibling keywords (e.g.
                   ;; unevaluatedItems) using c1a which carries the
                   ;; evaluation state from the resolved schema.
                   [c1b _m1b es-b] (if (seq m2-no-ref)
                                     (let [[_c2b _m2b f1b] (check-schema-fn (dissoc c2 :scope-id) schema-p2 m2-no-ref)]
                                       (f1b c1a p1 m1))
                                     [c1a m1 nil])]
               [c1b m1 (concatv es-a es-b)])
             (let [[_c2 _m2 compiled-f1] (check-schema-fn (dissoc c2 :scope-id) schema-p2 m2-no-ref)]
               (compiled-f1 c1 p1 m1))))
         [c1 m1 nil]))])))

(def check-property-description noop-checker)
(def check-property-readOnly    noop-checker)
(def check-property-writeOnly   noop-checker)
(def check-property-title       noop-checker)
(def check-property-default     noop-checker)
(def check-property-examples    noop-checker)


(defn check-property-deprecated [_property {quiet? :quiet? :as c2} _p2 m2 v2]
  [c2
   m2
   (if quiet?
     (fn [c1 _p1 m1] [c1 m1 nil])
     (fn [c1 _p1 m1]
       (log/warn (str "deprecated: " v2))
       [c1 m1 nil]))])

;; standard number properties

(defn check-property-minimum-old [_property c2 p2 m2 v2]
  (let [e? (get m2 "exclusiveMinimum")
        p? (if e? < <=)]
    [c2
     m2
     (make-type-checker
      json-number?
      (fn [c1 p1 m1]
        [c1
         m1
         (when-not (p? v2 m1)
           [(make-error (str "minimum" (when e? "(with exclusiveMinimum)") ": value too low") p2 m2 p1 m1)])]))]))

(defn check-property-minimum-new [_property c2 p2 m2 v2]
  [c2
   m2
   (make-type-checker
    json-number?
    (fn [c1 p1 m1]
      [c1
       m1
       (when-not (<= v2 m1)
         [(make-error "minimum: value too low" p2 m2 p1 m1)])]))])

(defn check-property-exclusiveMinimum-old [_property c2 _p2 {m "minimum" :as m2} _v2]
  [c2
   m2
   (fn [c1 _p1 m1]
     (when-not m (log/warn "exclusiveMinimum: no minimum present to modify"))
     [c1 m1 []])])

(defn check-property-exclusiveMinimum-new [_property c2 p2 m2 v2]
  [c2
   m2
   (make-type-checker
    json-number?
    (fn [c1 p1 m1]
      [c1
       m1
       (when-not (< v2 m1)
         [(make-error "exclusiveMinimum: value too low" p2 m2 p1 m1)])]))])

(defn check-property-maximum-old [_property c2 p2 m2 v2]
  (let [e? (get m2 "exclusiveMaximum")
        p? (if e? > >=)]
    [c2
     m2
     (make-type-checker
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
   (make-type-checker
    json-number?
    (fn [c1 _p1 m1]
      (when-not m (log/warn "exclusiveMaximum: no maximum present to modify"))
      [c1 m1 []]))])

(defn check-property-exclusiveMaximum-new [_property c2 p2 m2 v2]
  [c2
   m2
   (make-type-checker
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
     (make-type-checker
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
     (make-type-checker
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
     (make-type-checker
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
     (make-type-checker
      json-string?
      (fn [c1 p1 m1]
        [c1
         m1
         (when (or
                (> (count m1) ml2) ;; precheck before using expensive json-length
                (> (json-length m1) v2))
           [(make-error "maxLength: string too long" p2 m2 p1 m1)])]))]))

(defn make-check-property-format [strict? format->checker]
  (fn [_property {cfs :check-format :or {cfs {}} strict-format? :strict-format? :as c2} p2 m2 v2]
    (let [f (if (or strict? strict-format?)
              (fn [f2] (make-type-checker json-string? (fn [c p m] [c m (f2 c p m)])))
              (fn [f2] (make-type-checker json-string? (fn [c p m] (when-let [[{m :message}] (f2 c p m)] [c m (log/warn m)])))))]
      ;; we do this here so that user may override default format checkers...
      ;; First check for custom override, then draft-specific checker from closure
      [c2
       m2
       (if-let [checker (or (cfs v2)
                            (get format->checker v2)
                            (fn [_ _ _] (fn [_ _ _] (log/warn "format: not recognised:" (:draft c2) (pr-str v2)))))]
         (f (checker c2 p2 m2))
        ;; Unknown format - return identity function
         (f (constantly nil)))])))

(defn make-check-property-pattern [format->checker]
  (let [format-checker (make-check-property-format false format->checker)]
    (fn [_property c2 p2 m2 v2]
      (if (starts-with? v2 "$format:")
        ;; N.B.
        ;; this is an extension to allow patternProperties to
        ;; leverage formats since the spec does not provide a
        ;; formatProperties...
        (format-checker "format" c2 p2 m2 (subs v2 (count "$format:"))) ;; TODO: decide strictness from context somehow
        (let [p (ecma-pattern v2)]
          [c2
           m2
           (make-type-checker
            json-string?
            (fn [c1 p1 m1]
              [c1
               m1
               (when (false? (ecma-match p m1))
                 [(make-error "pattern: doesn't match" p2 m2 p1 m1)])]))])))))

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
       (make-type-checker
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
       (make-type-checker
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
    (let [[c2 m2 f1] ((get-check-schema) c2 p2 v2)
          pp2 (butlast p2)]
      [c2
       m2
       (fn [c1 p1 m1]
         (let [old-m1 (or (get (get c1 :content) pp2) m1)
               old-m1 (if cmt old-m1 (json-decode old-m1))] ;; TODO: error handling
           (try
             (let [[c1 _m1 es] (f1 c1 p1 old-m1)]
               [c1
                m1
                (when (seq es)
                  (if strict?
                    es
                    (do (log/warn "contentSchema: failed validation - " (pr-str es)) nil)))])
             (catch #?(:cljs js/Error :clj Exception) e
               [c1 m1 (:errors (ex-data e))]))))])))

(defn check-property-dependencies [_property c2 p2 m2 v2]
  (let [[c2 property->checker]
        (reduce
         (fn [[c2 acc] [k v]]
           (let [[c2 checker]
                 (cond
                   (json-string? v) ;; a single property dependency
                   [c2 (fn [c1 _p1 m1] [c1 m1 (when (not (contains? m1 v)) [v v])])]
                   (json-array? v) ;; a multiple property dependency
                   [c2 (fn [c1 _p1 m1] [c1 m1 (reduce (fn [acc2 k2] (if (contains? m1 k2) acc2 (conj acc2 [k k2]))) [] v)])]
                   (or (json-object? v) (boolean? v)) ;; a schema dependency
                   (let [[c2 _m2 f1] ((get-check-schema) c2 p2 v)]
                     [c2 f1])
                   :else [c2 nil])]
             [c2 (assoc acc k checker)]))
         [c2 {}]
         v2)]
    [c2
     m2
     (make-type-checker
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
  (let [[c2 property->checker]
        (reduce
         (fn [[c2 acc] [k v]]
           (let [[c2 _m2 f1] ((get-check-schema) c2 p2 v)]
             [c2 (assoc acc k f1)]))
         [c2 {}]
         v2)]
    [c2
     m2
     (make-type-checker
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
        [c2 checkers]
        (reduce
         (fn [[c2 acc] [k1 vs]]
           (reduce
            (fn [[c2 acc] [k2 s]]
              (let [[c2 _m2 f1] (f2 c2 p2 s)]
                [c2 (assoc acc [k1 k2] f1)]))
            [c2 acc]
            vs))
         [c2 {}]
         v2)
        ks (keys v2)]
    [c2
     m2
     (make-type-checker
      json-object?
      (fn [c1 p1 m1]
        (reduce
         (fn [[c1 m1 old-es] k]
           (let [v (get m1 k)]
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
     (make-type-checker
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
         [(-> (if success? new-c1 old-c1)
              (update :if assoc pp2 success?)
              ;; Don't let if's $dynamicAnchor stashes leak to sibling
              ;; keywords (then/else). After if completes, its schema
              ;; resources have left the dynamic scope.
              (assoc :$dynamic-anchor-schema (:$dynamic-anchor-schema old-c1)))
          m1 []]))]))

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
(defn check-property-definitions [_property c2 p2 m2 v2]
  (let [[c2 _] (compile-sub-schemas c2 p2 v2)]
    [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])]))

(defn check-property-$defs [_property c2 p2 m2 v2]
  (let [[c2 _] (compile-sub-schemas c2 p2 v2)]
    [c2 m2 (fn [c1 _p1 m1] [c1 m1 nil])]))

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
              (mapv (fn [[k :as k-and-cs]] [k-and-cs (get m1 k)]) k-and-css))]
         [(let [ks (mapv first k-and-css)]
            (-> c1
           ;; TODO: only record matched if additonalProperties needed later ?
                (update :matched update pp2 into-set ks)
           ;; TODO: only record evaluated if unevaluatedProperties needed later ?
                (update :evaluated update p1 into-set ks)))
          m1
          (make-error-on-failure message p2 m2 p1 m1 es)]))]))

(defn check-property-properties [_property c2 p2 m2 ps]
  (let [[c2 k-and-css] (compile-sub-schemas c2 p2 ps)
        [c2 m2 f1] (check-properties c2 p2 m2)]
    [c2
     m2
     (make-type-checker
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
;; Required keys could be gathered during M3/M2 validation pass instead of
;; scanning sub-schemas here.
(defn check-property-properties-draft3 [_property c2 p2 m2 ps]
  (let [[c2 k-and-css] (compile-sub-schemas c2 p2 ps)
        required-keys (vec (keep (fn [[k v]] (when (true? (get v "required")) k)) ps))
        [c2 m2 f1] (check-properties c2 p2 m2)]
    [c2
     m2
     (make-type-checker
      json-object?
      (fn [c1 p1 m1]
        (let [k-and-css (filter (fn [[k]] (contains? m1 k)) k-and-css)
              [c1 m1 prop-es] (f1 c1 p1 m1 k-and-css "properties: at least one property did not conform to respective schema")
              missing (seq (remove #(contains? m1 %) required-keys))
              req-es (when missing [(make-error ["required: missing properties (at least):" (vec missing)] p2 m2 p1 m1)])]
          [c1 m1 (concatv prop-es req-es)])))]))

;; what is opposite of "additional" - "matched" - used by spec to refer to properties matched by "properties" or "patternProperties"

(defn check-property-patternProperties [_property c2 p2 m2 pps]
  (let [[c2 k-and-css] (compile-sub-schemas c2 p2 pps)
        cp-and-pattern-and-ks (mapv (fn [[k f1]] [f1 (ecma-pattern k) k]) k-and-css)
        [c2 m2 f1] (check-properties c2 p2 m2)]
    [c2
     m2
     (make-type-checker
      json-object?
      (fn [c1 p1 m1]
        (let [k-and-css (into [] cat (keep (fn [[k]] (keep (fn [[cs p]] (when (ecma-match p k) [k cs])) cp-and-pattern-and-ks)) m1))]
          (f1 c1 p1 m1 k-and-css "patternProperties: at least one property did not conform to respective schema"))))]))

(defn check-property-additionalProperties [_property c2 p2 m2 v2]
  (let [f2 (get-check-schema)
        [c2 m2 cs] (f2 c2 p2 v2)
        pp2 (butlast p2)
        [c2 m2 f1] (check-properties c2 p2 m2)]
    [c2
     m2
     (make-type-checker
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
     (make-type-checker
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
     (if (json-object? m1)
       (let [[c1 all-es]
             (reduce-kv
              (fn [[c1 acc] k1 _v1]
                (let [[_c2 _m2 f1] ((get-check-schema) c2 (conj p2 k1) v2)
                      [c1 _m1 es] (f1 c1 (conj p1 k1) k1)]
                  [c1 (concatv acc es)]))
              [c1 []]
              m1)]
         [c1
          m1
          (make-error-on-failure
           "propertyNames: at least one property's name failed to conform to relevant schema"
           p2 m2 p1 m1
           all-es)])
       [c1 m1 nil]))])

(defn check-property-required [_property c2 p2 m2 v2]
  [c2
   m2
   (make-type-checker
    json-object?
    (fn [c1 p1 m1]
      [c1
       m1
       (when-let [missing (seq (filterv #(not (contains? m1 %)) v2))]
         [(make-error ["required: missing properties (at least):" missing] p2 m2 p1 m1)])]))])

(defn check-property-minProperties [_property c2 p2 m2 v2]
  [c2
   m2
   (make-type-checker
    json-object?
    (fn [c1 p1 m1]
      [c1
       m1
       (when (< (count m1) v2)
         [(make-error "minProperties: document contains too few properties" p2 m2 p1 m1)])]))])

(defn check-property-maxProperties [_property c2 p2 m2 v2]
  [c2
   m2
   (make-type-checker
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
  (let [[c2 i-and-css] (compile-sub-schemas c2 p2 (map-indexed vector v2))
        [c2 m2 f1] (check-items c2 p2 m2)]
    [c2
     m2
     (make-type-checker
      json-array?
      (fn [c1 p1 m1]
        (f1 c1 p1 m1 i-and-css "prefixItems: at least one item did not conform to respective schema")))]))

(defn check-property-items [_property c2 p2 m2 v2]
  (let [n (count (get m2 "prefixItems")) ;; TODO: achieve this by looking at c1 ?
        [c2 m css] (if (json-array? v2)
                     (let [[c2 i-and-css] (compile-sub-schemas c2 p2 (map-indexed vector v2))]
                       [c2 "respective " (mapv second i-and-css)])
                     (let [[c2 _m2 f1] ((get-check-schema) c2 p2 v2)]
                       [c2 "" (repeat f1)]))
        [c2 m2 f1] (check-items c2 p2 m2)]
    [c2
     m2
     (make-type-checker
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
       (make-type-checker
        json-array?
        (fn [c1 p1 m1]
          (let [n (count is)
                ais (drop n m1)
                i-and-css (mapv (fn [i cs _] [(+ i n) cs]) (range) (repeat cs) ais)]
            (f1 c1 p1 ais i-and-css "additionalItems: at least one item did not conform to schema"))))])
    [c2
     m2
     (fn [c1 _p1 m1]
       [c1 m1 nil])]))

(defn check-property-unevaluatedItems [_property c2 p2 m2 v2]
  (let [f2 (get-check-schema)
        [c2 _m2 cs] (f2 c2 p2 v2)
        css (repeat cs)
        [c2 m2 f1] (check-items c2 p2 m2)]
    [c2
     m2
     (make-type-checker
      json-array?
      (fn [{p->eis :evaluated :as c1} p1 m1]
        (let [eis (or (get p->eis p1) #{})
              index-and-items (filterv (fn [[k]] (not (eis k))) (map-indexed vector m1))
              i-and-css (mapv (fn [cs [i]] [i cs]) css index-and-items)]
          (f1 c1 p1 (mapv second index-and-items) i-and-css "unevaluatedItems: at least one item did not conform to schema"))))]))

(defn check-property-contains [_property c2 p2 {mn "minContains" :as m2} v2]
  (let [f2 (get-check-schema)
        [c2 m2 cs] (f2 c2 p2 v2)
        base (if mn mn 1)
        [c2 _v2 f1] (check-items c2 p2 v2)]
    [c2
     m2
     (make-type-checker
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
     (make-type-checker
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
     (make-type-checker
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
   (make-type-checker
    json-array?
    (fn [c1 p1 m1]
      [c1
       m1
       (when (< (count m1) v2)
         [(make-error "minItems: document contains too few items" p2 m2 p1 m1)])]))])

(defn check-property-maxItems [_property c2 p2 m2 v2]
  [c2
   m2
   (make-type-checker
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
     (make-type-checker
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
  (let [[c2 i-and-css] (compile-sub-schemas c2 p2 (map-indexed vector v2))]
    [c2
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
          (make-error-on message p2 m2 p1 m1 failed? es)]))]))

(defn check-property-oneOf [_property c2 p2 m2 v2]
  (let [[c2 co] (check-of c2 p2 m2 v2)
        m2-count (count v2)]
    [c2
     m2
     (fn [c1 p1 m1]
       (tweak
        m1
        (co
         c1 p1 m1
         (fn [es] (let [matched (- m2-count (count es))]
                    (str "oneOf: " matched " out of " m2-count " schemas matched; exactly 1 must match")))
         (fn [es] (not= 1 (- m2-count (count es)))))))]))

(defn check-property-anyOf [_property c2 p2 m2 v2]
  (let [[c2 co] (check-of c2 p2 m2 v2)
        m2-count (count v2)]
    [c2
     m2
     (fn [c1 p1 m1]
       (tweak
        m1
        (co
         c1 p1 m1
         (fn [es] (let [matched (- m2-count (count es))]
                    (str "anyOf: " matched " out of " m2-count " schemas matched; at least 1 must match")))
         (fn [es] (not (< (count es) m2-count))))))]))

(defn check-property-allOf [_property c2 p2 m2 v2]
  (let [[c2 co] (check-of c2 p2 m2 v2)
        m2-count (count v2)]
    [c2
     m2
     (fn [c1 p1 m1]
       (tweak
        m1
        (co
         c1 p1 m1
         (fn [es] (let [matched (- m2-count (count es))]
                    (str "allOf: " matched " out of " m2-count " schemas matched; all must match")))
         seq)))]))

(defn check-property-extends [_property c2 p2 m2 v2]
  (let [schemas (if (sequential? v2) v2 [v2])
        [c2 co] (check-of c2 p2 m2 schemas)
        m2-count (count schemas)]
    [c2
     m2
     (fn [c1 p1 m1]
       (tweak
        m1
        (co
         c1 p1 m1
         (fn [es] (let [matched (- m2-count (count es))]
                    (str "extends: " matched " out of " m2-count " schemas matched; all must match")))
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
