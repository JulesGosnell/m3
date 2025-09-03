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

(defn json-= [l r]
  (cond
    (and (json-number? l) (json-number? r)) (zero? (- l r))
    ;; TODO: could be more efficient...
    (and (json-array? l) (json-array? r) (= (count l) (count r))) (every? (partial apply json-=) (map vector l r))
    ;; TODO: json-objects
    :else
    (= l r)))

;;------------------------------------------------------------------------------
;; Individual type checker functions

(defn check-type-3 [p? t _c2 p2 m2]
  (fn [c1 p1 m1]
    [c1 (when-not (p? m1) [(make-error (str "type: not a[n] " t) p2 m2 p1 m1)])]))

(defn check-type-object [_c2 p2 m2]
  (check-type-3 json-object? "object" _c2 p2 m2))

(defn check-type-array [_c2 p2 m2]
  (check-type-3 json-array? "array" _c2 p2 m2))

(defn check-type-string [_c2 p2 m2]
  (check-type-3 json-string? "string" _c2 p2 m2))

(defn check-type-integer [c2 p2 m2]
  (let [si? (:strict-integer? c2)]
    (check-type-3 (if si? integer? json-integer?) "integer" c2 p2 m2)))

(defn check-type-number [_c2 p2 m2]
  (check-type-3 json-number? "number" _c2 p2 m2))

(defn check-type-boolean [_c2 p2 m2]
  (check-type-3 boolean? "boolean" _c2 p2 m2))

(defn check-type-null [_c2 p2 m2]
  (check-type-3 nil? "null" _c2 p2 m2))

(defn check-type-any [_c2 _p2 _m2]
  (fn [c1 _p1 _m1] [c1 nil]))

;;------------------------------------------------------------------------------
;; Type checking table - no draft variation needed for basic types
;; (strict-integer is handled via c2 context)

(def type->checker
  {"object" check-type-object
   "array" check-type-array
   "string" check-type-string
   "integer" check-type-integer
   "number" check-type-number
   "boolean" check-type-boolean
   "null" check-type-null
   "any" check-type-any})

;;------------------------------------------------------------------------------
;; Top-level dispatch function with re-entrance support

(defn check-type [type-spec c2 p2 m2]
  (cond

    (json-string? type-spec)
    (if-let [checker (get type->checker type-spec)]
      (checker c2 p2 m2)
      (fn [c1 p1 m1]
        [c1 [(make-error (pformat "type: unrecognised name: %s" type-spec) p2 m2 p1 m1)]]))
    
    (json-array? type-spec)
    (let [cs (map-indexed (fn [i t] (check-type t c2 (conj p2 i) m2)) type-spec)]
      (fn [c1 p1 m1]
        [c1
         (make-error-on
          (pformat "type: none matched: %s" type-spec)
          p2 m2 p1 m1
          (fn [es] (not (some nil? es)))
          (mapv (fn [checker] (second (checker c1 p1 m1))) cs))]))
    
    (json-object? type-spec)
    ((deref (resolve 'm3.validate/check-schema)) c2 p2 type-spec)
    
    :else
    (fn [c1 p1 m1]
      [c1 [(make-error (pformat "type: unrecognised description: %s" type-spec) p2 m2 p1 m1)]])))

;;------------------------------------------------------------------------------

(defn make-type-checker [predicate? m1-function]
  (fn [c1 p1 m1]
    (if (predicate? m1)
      (m1-function c1 p1 m1)
      [c1 []])))
