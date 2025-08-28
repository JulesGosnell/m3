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

(ns m3.type
  (:require
   [m3.platform :refer [pformat]]
   [m3.util :refer [make-error make-error-on]]))

;;------------------------------------------------------------------------------

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

;;------------------------------------------------------------------------------

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
                
                ;; TODO: the way we are recursing back into
                ;; m3.validate/check-schema is not ideal but...
                (json-object? t)
                ((deref (resolve 'm3.validate/check-schema)) c2 (conj p2 i) t)

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
          (pformat "type: none matched: %s" ts)
          p2 m2 p1 m1
          (fn [es] (not (some nil? es)))
          (mapv (fn [checker] (second (checker c1 p1 m1))) checkers))]))
    (fn [c1 p1 m1]
      [c1 [(make-error (pformat "type: unrecognised: %s" ts) p2 m2 p1 m1)]])))

;;------------------------------------------------------------------------------

(defn make-type-checker [predicate? m1-function]
  (fn [c1 p1 m1]
    (if (predicate? m1)
      (m1-function c1 p1 m1)
      [c1 []])))
