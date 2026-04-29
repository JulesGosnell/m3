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
   [m3.util :refer [make-error make-error-on third get-check-schema]]))

;;------------------------------------------------------------------------------

(defn json-integer? [i]
  ;; Draft-06+ definition of integer: any number with a zero fractional
  ;; part counts (so e.g. 2.0 satisfies type:integer).  This is the
  ;; behaviour the test suite asserts in those drafts and what other
  ;; spec-conforming validators (NetworkNt, SchemaFriend, …) do.
  (or (integer? i)
      (and (number? i)
           (zero? (mod i 1)))))

(defn json-integer-strict? [i]
  ;; Draft-03 / draft-04 definition: only language-level integers count.
  ;; The optional/zeroTerminatedFloats test in those drafts asserts that
  ;; 1.0 (a JSON number with a fractional point) is NOT an integer.
  ;; On CLJS we can't tell — JavaScript has a single Number type, so
  ;; JSON.parse(\"1.0\") and JSON.parse(\"1\") both return the same value
  ;; — fall back to the draft-06+ behaviour and document the test as
  ;; an exclusion.
  #?(:clj  (integer? i)
     :cljs (or (integer? i)
               (and (number? i)
                    (zero? (mod i 1))))))

(defn json-number? [n]
  (number? n))

(defn json-string? [v]
  (string? v))

(defn json-array? [v]
  (or (vector? v) #?(:clj (instance? java.util.List v) :cljs false)))

(defn json-object? [v]
  (or (map? v) #?(:clj (instance? java.util.Map v) :cljs false)))

(defn json-= [l r]
  (cond
    (and (json-number? l) (json-number? r)) (zero? (- l r))
    ;; TODO: could be more efficient...
    (and (json-array? l) (json-array? r) (= (count l) (count r))) (every? (partial apply json-=) (map vector l r))
    ;; TODO: json-objects
    :else
    (= l r)))

;;------------------------------------------------------------------------------

(defn check-type-2 [p? t c2 p2 m2]
  (fn [c1 p1 m1]
    [c1 m1 (when-not (p? m1) [(make-error (str "type: not a[n] " t) p2 m2 p1 m1)])]))

;;------------------------------------------------------------------------------

(defn check-type-integer [t c2 p2 m2]
  (check-type-2 json-integer? t c2 p2 m2))

(defn check-type-integer-strict [t c2 p2 m2]
  (check-type-2 json-integer-strict? t c2 p2 m2))

(def draft3-type->checker
  ;; Draft-03 used the strict integer definition (1.0 is NOT an integer);
  ;; draft-06 redefined integer to include any zero-fraction number.
  {"any"     (constantly (fn [c1 _p1 m1] [c1 m1 nil]))
   "array"   (partial check-type-2 json-array?)
   "boolean" (partial check-type-2 boolean?)
   "integer" check-type-integer-strict
   "null"    (partial check-type-2 nil?)
   "number"  (partial check-type-2 json-number?)
   "object"  (partial check-type-2 json-object?)
   "string"  (partial check-type-2 json-string?)})

(def draft4-type->checker
  ;; Draft-04: same strict integer rule as draft-03, no "any" type.
  (dissoc draft3-type->checker "any"))

(def draft6-type->checker
  ;; Draft-06 onwards: integer accepts any zero-fraction number.
  (assoc draft4-type->checker "integer" check-type-integer))

;;------------------------------------------------------------------------------

(defn check-type [type->checker t c2 p2 m2]
  [c2
   m2
   (cond

     (json-string? t)
     (if-let [f2 (get type->checker t)]
       (f2 t c2 p2 m2)
       (fn [c1 p1 m1]
         [c1 m1 [(make-error (pformat "type: unrecognised name: %s" t) p2 m2 p1 m1)]]))

     (json-array? t)
     (let [f1s (map-indexed (fn [i t] (third (check-type type->checker t c2 (conj p2 i) m2))) t)]
       (fn [c1 p1 m1]
         [c1
          m1
          (make-error-on
           (pformat "type: none matched: %s" t)
           p2 m2 p1 m1
           (fn [es] (not (some nil? es)))
           (mapv (fn [f1] (third (f1 c1 p1 m1))) f1s))]))

     (json-object? t)
     (let [[c2 m2 f1] ((get-check-schema) c2 p2 t)]
       (fn [c1 p1 m1]
         (let [[c1 m1 es] (f1 c1 p1 m1)]
           [c1 m1 es])))

     :else
     (fn [c1 p1 m1]
       [c1 m1 [(make-error (pformat "type: unrecognised description: %s" t) p2 m2 p1 m1)]]))])

;;------------------------------------------------------------------------------

(defn make-type-checker [predicate? m1-function]
  (fn [c1 p1 m1]
    (if (predicate? m1)
      (m1-function c1 p1 m1)
      [c1 m1 []])))
