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

(defn check-type-2 [p? t _c2 p2 m2]
  (fn [c1 p1 m1]
    [c1 (when-not (p? m1) [(make-error (str "type: not a[n] " t) p2 m2 p1 m1)])]))

;;------------------------------------------------------------------------------

(defn check-type-integer [t {si? :strict-integer? :as c2} p2 m2]
  (check-type-2 (if si? integer? json-integer?) t c2 p2 m2))

(def draft3-type->checker
  {"any"     (constantly (fn [c1 _p1 _m1] [c1 nil]))
   "array"   (partial check-type-2 json-array?)
   "boolean" (partial check-type-2 boolean?)
   "integer" check-type-integer
   "null"    (partial check-type-2 nil?)
   "number"  (partial check-type-2 json-number?)
   "object"  (partial check-type-2 json-object?)
   "string"  (partial check-type-2 json-string?)})

(def draft4-type->checker
  (dissoc draft3-type->checker "any"))

(def draft->type->checker
  {:draft3        draft3-type->checker
   :draft4        draft4-type->checker
   :draft6        draft4-type->checker
   :draft7        draft4-type->checker
   :draft2019-09  draft4-type->checker
   :draft2020-12  draft4-type->checker
   :draft-next    draft4-type->checker})

;;------------------------------------------------------------------------------

(defn check-type [t c2 p2 m2]
  (let [draft (or (:draft c2) :draft7)] ; Default to draft7 if not specified
    (cond

      (json-string? t)
      (if-let [c (get-in draft->type->checker [draft t])]
        (c t c2 p2 m2)
        (fn [c1 p1 m1]
          [c1 [(make-error (pformat "type: unrecognised name: %s" t) p2 m2 p1 m1)]]))

      (json-array? t)
      (let [cs (map-indexed (fn [i t] (check-type t c2 (conj p2 i) m2)) t)]
        (fn [c1 p1 m1]
          [c1
           (make-error-on
            (pformat "type: none matched: %s" t)
            p2 m2 p1 m1
            (fn [es] (not (some nil? es)))
            (mapv (fn [c] (second (c c1 p1 m1))) cs))]))

      (json-object? t)
      ((deref (resolve 'm3.validate/check-schema)) c2 p2 t)

      :else
      (fn [c1 p1 m1]
        [c1 [(make-error (pformat "type: unrecognised description: %s" t) p2 m2 p1 m1)]]))))

;;------------------------------------------------------------------------------

(defn make-type-checker [predicate? m1-function]
  (fn [c1 p1 m1]
    (if (predicate? m1)
      (m1-function c1 p1 m1)
      [c1 []])))

(defn make-new-type-checker [predicate? m1-function]
  (fn [c1 p1 m1]
    (if (predicate? m1)
      (m1-function c1 p1 m1)
      [c1 m1 []])))
