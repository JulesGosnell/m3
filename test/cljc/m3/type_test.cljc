(ns m3.type-test
  (:require
   #?(:clj [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   [m3.type :refer [check-type json-integer? json-number? json-string?
                    json-array? json-object? json-= make-type-checker]]))

;; =============================================================================
;; Tests for coverage of edge cases not covered by JSON Schema Test Suite
;; =============================================================================

(deftest test-unrecognized-type-names
  (testing "Unrecognized type string names"
    ;; Test line 91-92: error case for unrecognized type name
    (let [checker (check-type "not-a-real-type" {} [] {})]
      (let [[_c1 errors] (checker {} [] "some value")]
        (is (= 1 (count errors)) "Should return exactly one error")
        (is (re-find #"type: unrecognised name: not-a-real-type"
                     (:message (first errors)))
            "Error message should mention the unrecognized type")))

    ;; Test with draft context (ensures the default draft path is tested)
    (let [checker (check-type "unknown" {:draft :draft7} [] {})]
      (let [[_c1 errors] (checker {} [] 42)]
        (is (= 1 (count errors)) "Should return error for unknown type")))

    ;; Test that draft-03 "any" type works in draft3 context
    (let [checker (check-type "any" {:draft :draft3} [] {})]
      (let [[_c1 errors] (checker {} [] "anything")]
        (is (nil? errors) "any type should pass in draft-03")))

    ;; Test that "any" type fails in draft4+ context
    (let [checker (check-type "any" {:draft :draft4} [] {})]
      (let [[_c1 errors] (checker {} [] "anything")]
        (is (= 1 (count errors)) "any type should fail in draft-04")))))

(deftest test-unrecognized-type-descriptions
  (testing "Unrecognized type descriptions (non-string, non-array, non-object)"
    ;; Test line 108-109: error case for unrecognized type description
    (let [checker (check-type 123 {} [] {})] ; number as type
      (let [[_c1 errors] (checker {} [] "some value")]
        (is (= 1 (count errors)) "Should return exactly one error")
        (is (re-find #"type: unrecognised description: 123"
                     (:message (first errors)))
            "Error message should mention the unrecognized type description")))

    (let [checker (check-type true {} [] {})] ; boolean as type
      (let [[_c1 errors] (checker {} [] "some value")]
        (is (= 1 (count errors)) "Should return error for boolean type spec")))

    (let [checker (check-type nil {} [] {})] ; nil as type
      (let [[_c1 errors] (checker {} [] "some value")]
        (is (= 1 (count errors)) "Should return error for nil type spec")))))

(deftest test-json-predicates
  (testing "json-integer? predicate"
    (is (json-integer? 42) "Integer should be json-integer")
    (is (json-integer? 0) "Zero should be json-integer")
    (is (json-integer? -10) "Negative integer should be json-integer")
    (is (json-integer? 1.0) "Float with zero decimal should be json-integer")
    (is (not (json-integer? 1.5)) "Float with decimal should not be json-integer")
    (is (not (json-integer? "42")) "String should not be json-integer"))

  (testing "json-number? predicate"
    (is (json-number? 42) "Integer should be json-number")
    (is (json-number? 3.14) "Float should be json-number")
    (is (not (json-number? "42")) "String should not be json-number"))

  (testing "json-string? predicate"
    (is (json-string? "hello") "String should be json-string")
    (is (json-string? "") "Empty string should be json-string")
    (is (not (json-string? 42)) "Number should not be json-string"))

  (testing "json-array? predicate"
    (is (json-array? []) "Empty vector should be json-array")
    (is (json-array? [1 2 3]) "Vector should be json-array")
    (is (not (json-array? '(1 2 3))) "List should not be json-array"))

  (testing "json-object? predicate"
    (is (json-object? {}) "Empty map should be json-object")
    (is (json-object? {"key" "value"}) "Map should be json-object")
    (is (not (json-object? [])) "Vector should not be json-object")))

(deftest test-json-equals
  (testing "json-= for numbers"
    (is (json-= 42 42) "Same integers should be equal")
    (is (json-= 42 42.0) "Integer and equivalent float should be equal")
    (is (json-= 3.14 3.14) "Same floats should be equal")
    (is (not (json-= 42 43)) "Different numbers should not be equal"))

  (testing "json-= for arrays"
    (is (json-= [] []) "Empty arrays should be equal")
    (is (json-= [1 2 3] [1 2 3]) "Same arrays should be equal")
    (is (json-= [1 2.0] [1.0 2]) "Arrays with equivalent numbers should be equal")
    (is (not (json-= [1 2] [1 2 3])) "Different length arrays should not be equal")
    (is (not (json-= [1 2] [2 1])) "Different order arrays should not be equal"))

  (testing "json-= for other types"
    (is (json-= "hello" "hello") "Same strings should be equal")
    (is (json-= true true) "Same booleans should be equal")
    (is (json-= nil nil) "nil should equal nil")
    (is (not (json-= "hello" "world")) "Different strings should not be equal")))

(deftest test-make-type-checker
  (testing "make-type-checker utility"
    (let [string-checker (make-type-checker string? (fn [c p m] [c nil]))]
      (let [[c1 result] (string-checker {} [] "hello")]
        (is (nil? result) "String should pass string checker"))
      (let [[c1 result] (string-checker {} [] 42)]
        (is (= [] result) "Number should fail string checker with empty errors")))

    (let [custom-checker (make-type-checker
                          #(> % 10)
                          (fn [c p m] [c [{:error "passed"}]]))]
      (let [[c1 result] (custom-checker {} [] 20)]
        (is (= [{:error "passed"}] result) "Value > 10 should run m1-function"))
      (let [[c1 result] (custom-checker {} [] 5)]
        (is (= [] result) "Value <= 10 should return empty errors")))))
