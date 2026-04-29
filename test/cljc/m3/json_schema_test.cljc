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
    (doseq [draft [:draft3 :draft4 :draft6 :draft7 :draft2019-09 :draft2020-12 :draft-v1]]
      (is (:valid? (m3/validate {"type" "string"} "hello" {:draft draft}))
          (str "should validate with " draft)))))

;;------------------------------------------------------------------------------
;; validate — format is annotation-only in draft2020-12 by default

(deftest test-validate-format-annotation
  (testing "format is annotation-only by default (draft2020-12)"
    (is (:valid? (m3/validate {"type" "string" "format" "email"} "not-an-email"))))
  (testing "1.0 passes as integer (JSON has no integer/float distinction)"
    (is (:valid? (m3/validate {"type" "integer"} 1.0)))))

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
;; Regression: malformed schemas must not crash (corpus stress-test bugs)

(deftest test-malformed-schemas-no-crash
  (testing "draft-03 top-level required:true doesn't crash"
    (is (some? (m3/validate
                {"$schema" "http://json-schema.org/draft-03/schema"
                 "type" "object" "required" true
                 "properties" {"name" {"type" "string" "required" true}}}
                {"name" "test"}))))
  (testing "boolean id value doesn't crash"
    (is (some? (m3/validate
                {"$schema" "http://json-schema.org/draft-04/schema#"
                 "properties" {"id" {"type" "string" "id" true}}}
                {"id" "test"}))))
  (testing "string where sub-schema expected doesn't crash"
    (is (some? (m3/validate
                {"$schema" "http://json-schema.org/draft-04/schema#"
                 "type" "object"
                 "properties" {"x" {"type" "object"
                                    "properties" {"$ref" "#/definitions/Foo"}}}}
                {}))))
  (testing "non-map schema values don't crash"
    (is (some? (m3/validate
                {"$schema" "http://json-schema.org/draft-04/schema#"
                 "type" "object"
                 "properties" {"x" "not-a-schema"}}
                {})))))

;;------------------------------------------------------------------------------
;; Regression: recursive $ref in applicator keywords must not StackOverflow (#49)

(deftest test-recursive-ref-in-allOf
  (testing "allOf with $ref to root returns error, doesn't crash"
    (let [schema {"allOf" [{"$ref" "#"}]
                  "properties" {"name" {"type" "string"}}
                  "type" "object"}
          result (m3/validate schema {"name" "Alice"})]
      (is (not (:valid? result)))
      (is (seq (:errors result)))))
  (testing "recursive $ref in properties still works (not a cycle)"
    (let [schema {"type" "object"
                  "properties" {"child" {"$ref" "#"}}}]
      (is (:valid? (m3/validate schema {"child" {"child" {}}})))
      (is (not (:valid? (m3/validate schema {"child" "not-object"})))))))

;;------------------------------------------------------------------------------
;; validate/validator — :registry option for $ref resolution

(deftest test-registry-option
  (testing "$ref resolved from registry"
    (let [schema   {"$ref" "http://example.com/defs/name"}
          registry {"http://example.com/defs/name" {"type" "string" "minLength" 1}}]
      (is (:valid? (m3/validate schema "Alice" {:registry registry})))
      (is (not (:valid? (m3/validate schema "" {:registry registry}))))
      (is (not (:valid? (m3/validate schema 42 {:registry registry}))))))
  (testing "validator with registry"
    (let [v (m3/validator {"$ref" "http://example.com/int"}
                          {:registry {"http://example.com/int" {"type" "integer"}}})]
      (is (:valid? (v 42)))
      (is (not (:valid? (v "hello")))))))

;;------------------------------------------------------------------------------
;; validator — JSON string schema

(deftest test-validator-json-string
  (testing "schema as JSON string"
    (let [v (m3/validator "{\"type\":\"string\",\"minLength\":1}")]
      (is (:valid? (v "hello")))
      (is (not (:valid? (v "")))))))

;;------------------------------------------------------------------------------
;; Warnings system (#52)

(deftest test-warnings-format-annotation
  (testing "format annotation produces warning, not error"
    (let [result (m3/validate {"type" "string" "format" "email"} "not-an-email")]
      (is (:valid? result))
      (is (nil? (:errors result)))
      (is (seq (:warnings result)))
      (let [w (first (:warnings result))]
        (is (contains? w :schema-path))
        (is (contains? w :document-path))
        (is (contains? w :message))
        (is (contains? w :document))
        (is (contains? w :schema))
        (is (string? (:message w))))))
  (testing "valid format produces no warning"
    (let [result (m3/validate {"type" "string" "format" "email"} "alice@example.com")]
      (is (:valid? result))
      (is (nil? (:warnings result))))))

(deftest test-warnings-deprecated
  (testing "deprecated schema produces runtime warning"
    (let [result (m3/validate {"type" "string" "deprecated" true} "hello")]
      (is (:valid? result))
      (is (seq (:warnings result)))
      (let [w (first (:warnings result))]
        (is (contains? w :schema-path))
        (is (contains? w :document-path))
        (is (contains? w :message))
        (is (contains? w :document))
        (is (contains? w :schema))
        (is (string? (:message w))))))
  (testing "deprecated warning persists across validator calls"
    (let [v (m3/validator {"type" "string" "deprecated" true})]
      (is (seq (:warnings (v "hello"))))
      (is (seq (:warnings (v "world")))))))

(deftest test-warnings-unknown-format
  (testing "unknown format produces compile-time warning"
    (let [result (m3/validate {"type" "string" "format" "foobar"} "hello")]
      (is (:valid? result))
      (is (seq (:warnings result)))
      (let [w (first (:warnings result))]
        (is (contains? w :schema-path))
        (is (contains? w :message))
        (is (string? (:message w)))))))

(deftest test-warnings-absent-when-clean
  (testing "no :warnings key when no warnings"
    (let [result (m3/validate {"type" "string"} "hello")]
      (is (:valid? result))
      (is (not (contains? result :warnings)))))
  (testing "no :warnings key on validation failure without warnings"
    (let [result (m3/validate {"type" "string"} 42)]
      (is (not (:valid? result)))
      (is (not (contains? result :warnings))))))

(deftest test-warnings-content-annotation
  (testing "contentEncoding failure in non-strict mode produces warning"
    (let [result (m3/validate {"type" "string" "contentEncoding" "base64"} "!!!")]
      (is (:valid? result))
      (is (seq (:warnings result)))
      (let [w (first (:warnings result))]
        (is (contains? w :schema-path))
        (is (contains? w :message))
        (is (string? (:message w)))))))

(deftest test-infos-$comment
  (testing "$comment produces compile-time info"
    (let [result (m3/validate {"type" "string" "$comment" "this is a note"} "hello")]
      (is (:valid? result))
      (is (not (contains? result :warnings)))
      (is (seq (:infos result)))
      (let [i (first (:infos result))]
        (is (contains? i :schema-path))
        (is (contains? i :message))
        (is (string? (:message i)))
        (is (re-find #"this is a note" (:message i))))))
  (testing "$comment info persists across validator calls"
    (let [v (m3/validator {"type" "string" "$comment" "a note"})]
      (is (seq (:infos (v "hello"))))
      (is (seq (:infos (v "world"))))))
  (testing "no :infos key when no $comment"
    (let [result (m3/validate {"type" "string"} "hello")]
      (is (not (contains? result :infos))))))

(deftest test-warnings-format-invalid-and-valid-doc
  (testing "format warning on invalid doc (doc has errors AND warnings)"
    (let [result (m3/validate {"type" "string" "format" "email" "minLength" 100} "not-an-email")]
      (is (not (:valid? result)))
      (is (seq (:errors result)))
      (is (seq (:warnings result)))))
  (testing "format warning only for annotation drafts, not assertion drafts"
    (let [result (m3/validate {"type" "string" "format" "email"} "not-an-email" {:draft :draft7})]
      ;; draft7 treats format as assertion — produces error, not warning
      (is (not (:valid? result)))
      (is (seq (:errors result)))
      (is (not (contains? result :warnings))))))

(deftest test-warnings-same-shape-as-errors
  (testing "warnings have same base keys as errors"
    (let [result (m3/validate {"type" "string" "format" "email"} "not-an-email")
          w (first (:warnings result))
          err-result (m3/validate {"type" "string"} 42)
          e (first (:errors err-result))
          base-keys #{:schema-path :document-path :message :document :schema}]
      (is (= base-keys (set (keys w))))
      (is (every? (set (keys e)) base-keys)))))
