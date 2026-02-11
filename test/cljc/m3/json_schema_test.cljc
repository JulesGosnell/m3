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

(ns m3.json-schema-test
  (:require
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   [m3.json-schema :as m3]))

(deftest test-latest-draft
  (testing ":latest behaves like :draft2020-12"
    (let [schema {"type" "object"
                  "properties" {"name" {"type" "string"}
                                "age"  {"type" "integer"}}
                  "required" ["name"]}
          valid-doc   {"name" "Alice" "age" 30}
          invalid-doc {"age" "not a number"}]
      (is (= (m3/validate schema valid-doc {:draft :latest})
             (m3/validate schema valid-doc {:draft :draft2020-12})))
      (is (= (m3/validate schema invalid-doc {:draft :latest})
             (m3/validate schema invalid-doc {:draft :draft2020-12}))))))

(deftest test-latest-validator
  (testing ":latest works with compiled validator"
    (let [v-latest (m3/validator {"type" "string" "minLength" 1} {:draft :latest})
          v-2020   (m3/validator {"type" "string" "minLength" 1} {:draft :draft2020-12})]
      (is (:valid? (v-latest "hello")))
      (is (not (:valid? (v-latest ""))))
      (is (= (v-latest "hello") (v-2020 "hello")))
      (is (= (v-latest "") (v-2020 ""))))))

;;------------------------------------------------------------------------------
;; validate — 2-arity (no opts)

(deftest test-validate-2-arity
  (testing "2-arity defaults work"
    (is (:valid? (m3/validate {"type" "string"} "hello")))
    (is (not (:valid? (m3/validate {"type" "string"} 42))))))

;;------------------------------------------------------------------------------
;; validate — JSON string form

(deftest test-validate-json-strings
  (testing "both schema and document as JSON strings"
    (let [result (m3/validate "{\"type\":\"string\"}" "\"hello\"")]
      (is (:valid? result)))
    (let [result (m3/validate "{\"type\":\"number\"}" "\"oops\"")]
      (is (not (:valid? result))))))

;;------------------------------------------------------------------------------
;; validate — different drafts

(deftest test-validate-drafts
  (testing "explicit draft selection"
    (doseq [draft [:draft3 :draft4 :draft6 :draft7 :draft2019-09 :draft2020-12 :draft-next]]
      (is (:valid? (m3/validate {"type" "string"} "hello" {:draft draft}))
          (str "should validate with " draft)))))

;;------------------------------------------------------------------------------
;; validate — strict-format?

(deftest test-validate-strict-format
  (testing "format is annotation-only by default"
    (is (:valid? (m3/validate {"type" "string" "format" "email"} "not-an-email"))))
  (testing "strict-format? makes format an assertion"
    (is (not (:valid? (m3/validate {"type" "string" "format" "email"} "not-an-email"
                                   {:strict-format? true}))))))

;;------------------------------------------------------------------------------
;; validate — strict-integer?

(deftest test-validate-strict-integer
  (testing "1.0 passes as integer by default"
    (is (:valid? (m3/validate {"type" "integer"} 1.0))))
  (testing "strict-integer? rejects 1.0"
    (is (not (:valid? (m3/validate {"type" "integer"} 1.0
                                   {:strict-integer? true}))))))

;;------------------------------------------------------------------------------
;; validate — error shape

(deftest test-validate-error-shape
  (testing "error contains expected keys"
    (let [result (m3/validate {"type" "string"} 42)]
      (is (not (:valid? result)))
      (is (seq (:errors result)))
      (let [e (first (:errors result))]
        (is (contains? e :schema-path))
        (is (contains? e :message))
        (is (contains? e :document))
        (is (string? (:message e)))))))

;;------------------------------------------------------------------------------
;; validator — 1-arity (no opts)

(deftest test-validator-1-arity
  (testing "1-arity defaults work"
    (let [v (m3/validator {"type" "integer" "minimum" 0})]
      (is (:valid? (v 42)))
      (is (not (:valid? (v -1))))
      (is (not (:valid? (v "hello")))))))

;;------------------------------------------------------------------------------
;; validator — JSON string schema

(deftest test-validator-json-string
  (testing "schema as JSON string"
    (let [v (m3/validator "{\"type\":\"string\",\"minLength\":1}")]
      (is (:valid? (v "hello")))
      (is (not (:valid? (v "")))))))
