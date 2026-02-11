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
   [m3.format :refer [check-format-style check-format-phone check-format-utc-millisec]]
   [m3.pattern :as pat]))

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

;; =============================================================================
;; Pattern Regression Tests
;; =============================================================================
;; Edge cases not covered by the official JSON Schema Test Suite.

(deftest test-email-pattern
  (testing "single-character local parts (RFC 5321)"
    (is (re-matches pat/email-pattern "a@b.com"))
    (is (re-matches pat/email-pattern "x@example.org")))
  (testing "dot boundaries"
    (is (not (re-matches pat/email-pattern ".a@b.com")))
    (is (not (re-matches pat/email-pattern "a.@b.com")))
    (is (not (re-matches pat/email-pattern "a..b@c.com"))))
  (testing "multi-char local parts"
    (is (re-matches pat/email-pattern "joe.bloggs@example.com"))
    (is (re-matches pat/email-pattern "te~st@example.com"))))

(deftest test-ipv6-pattern
  (testing "uppercase hex (RFC 4291)"
    (is (re-matches pat/ipv6-pattern "::ABEF"))
    (is (re-matches pat/ipv6-pattern "FE80::1"))
    (is (re-matches pat/ipv6-pattern "2001:DB8::FF00:42:8329")))
  (testing "mixed case"
    (is (re-matches pat/ipv6-pattern "2001:db8::Ff00:42:8329")))
  (testing "basic valid addresses"
    (is (re-matches pat/ipv6-pattern "::1"))
    (is (re-matches pat/ipv6-pattern "::"))
    (is (re-matches pat/ipv6-pattern "1:2:3:4:5:6:7:8")))
  (testing "invalid addresses"
    (is (not (re-matches pat/ipv6-pattern "12345::")))
    (is (not (re-matches pat/ipv6-pattern "::laptop")))
    (is (not (re-matches pat/ipv6-pattern "1::2::3")))))

(deftest test-ipv4-pattern
  (testing "valid addresses"
    (is (re-matches pat/ipv4-pattern "192.168.1.1"))
    (is (re-matches pat/ipv4-pattern "0.0.0.0"))
    (is (re-matches pat/ipv4-pattern "255.255.255.255")))
  (testing "out-of-range octets"
    (is (not (re-matches pat/ipv4-pattern "256.0.0.0")))
    (is (not (re-matches pat/ipv4-pattern "192.168.1.999"))))
  (testing "wrong number of octets"
    (is (not (re-matches pat/ipv4-pattern "192.168.1")))
    (is (not (re-matches pat/ipv4-pattern "192.168.1.1.1")))))

(deftest test-hostname-pattern
  (testing "valid hostnames"
    (is (re-matches pat/hostname-pattern "example.com"))
    (is (re-matches pat/hostname-pattern "sub.example.com"))
    (is (re-matches pat/hostname-pattern "a")))
  (testing "hyphens"
    (is (re-matches pat/hostname-pattern "my-host.example.com"))
    (is (not (re-matches pat/hostname-pattern "-starts-with-hyphen")))
    (is (not (re-matches pat/hostname-pattern "ends-with-hyphen-")))))

(deftest test-json-pointer-pattern
  (testing "valid pointers"
    (is (re-matches pat/json-pointer-pattern ""))
    (is (re-matches pat/json-pointer-pattern "/foo"))
    (is (re-matches pat/json-pointer-pattern "/foo/bar"))
    (is (re-matches pat/json-pointer-pattern "/foo/0"))
    (is (re-matches pat/json-pointer-pattern "/~0"))
    (is (re-matches pat/json-pointer-pattern "/~1")))
  (testing "invalid pointers"
    (is (not (re-matches pat/json-pointer-pattern "foo")))
    (is (not (re-matches pat/json-pointer-pattern "/foo/bar~")))))

(deftest test-uuid-pattern
  (testing "valid UUIDs"
    (is (re-matches pat/uuid-pattern "2eb8aa08-aa98-11ea-b4aa-73b441d16380"))
    (is (re-matches pat/uuid-pattern "00000000-0000-0000-0000-000000000000")))
  (testing "invalid UUIDs"
    (is (not (re-matches pat/uuid-pattern "2eb8aa08-aa98-11ea-b4aa-73b441d1638")))
    (is (not (re-matches pat/uuid-pattern "not-a-uuid")))))
