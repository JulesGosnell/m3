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

(ns m3.format-test
  (:require
   #?(:clj [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   [m3.format :refer [check-format-style check-format-phone check-format-utc-millisec]]))

;; =============================================================================
;; Draft-03 Format Tests
;; =============================================================================
;; These tests exist solely to achieve code coverage for three draft-03 formats
;; that were never properly standardized and are not tested by the official 
;; JSON Schema Test Suite.
;;
;; Historical Context:
;; - "style", "phone", and "utc-millisec" were defined in JSON Schema draft-03
;; - They were dropped in draft-04 (2013) due to being too vague or locale-specific
;; - No official test suite includes tests for these formats
;; - They are implemented as no-ops (always pass) to maintain draft-03 compatibility
;;
;; These formats were problematic because:
;; - "style": What constitutes valid CSS? Inline? Full stylesheet?
;; - "phone": Which country's format? International? With/without country codes?
;; - "utc-millisec": Redundant with date-time and ambiguous about precision
;; =============================================================================

(deftest test-draft3-format-style
  (testing "style format (draft-03 only, not validated)"
    ;; Style format checker always returns nil (passes validation)
    (let [checker (check-format-style {} [] {})]
      (is (nil? (checker {} [] "color: red;"))
          "Valid CSS style passes")
      (is (nil? (checker {} [] "not-really-css"))
          "Invalid CSS also passes - format not validated")
      (is (nil? (checker {} [] 123))
          "Non-string also passes - type checking happens elsewhere"))))

(deftest test-draft3-format-phone
  (testing "phone format (draft-03 only, not validated)"
    ;; Phone format checker always returns nil (passes validation)
    (let [checker (check-format-phone {} [] {})]
      (is (nil? (checker {} [] "+1-555-555-5555"))
          "US phone format passes")
      (is (nil? (checker {} [] "not-a-phone"))
          "Invalid phone also passes - format not validated")
      (is (nil? (checker {} [] 5555555555))
          "Number also passes - type checking happens elsewhere"))))

(deftest test-draft3-format-utc-millisec
  (testing "utc-millisec format (draft-03 only, not validated)"
    ;; UTC millisec format checker always returns nil (passes validation)
    (let [checker (check-format-utc-millisec {} [] {})]
      (is (nil? (checker {} [] "1234567890123"))
          "Timestamp string passes")
      (is (nil? (checker {} [] "not-a-timestamp"))
          "Invalid timestamp also passes - format not validated")
      (is (nil? (checker {} [] 1234567890123))
          "Number also passes - type checking happens elsewhere"))))
