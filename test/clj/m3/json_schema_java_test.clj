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

(ns m3.json-schema-java-test
  "Tests for the Java facade (m3.JsonSchema).
   Exercises all four overloads and verifies output is java.util.Map/List."
  (:require [clojure.test :refer [deftest testing is]]
            [m3.JsonSchema :as js]))

;;------------------------------------------------------------------------------
;; (String, String) overload

(deftest test-validate-string-string
  (testing "valid string type"
    (let [result (js/-validate "{\"type\":\"string\"}" "\"hello\"")]
      (is (instance? java.util.Map result))
      (is (true? (.get ^java.util.Map result "valid")))))
  (testing "invalid string type"
    (let [result (js/-validate "{\"type\":\"string\"}" "42")]
      (is (instance? java.util.Map result))
      (is (false? (.get ^java.util.Map result "valid")))
      (let [errors (.get ^java.util.Map result "errors")]
        (is (instance? java.util.List errors))
        (is (pos? (.size ^java.util.List errors)))))))

;;------------------------------------------------------------------------------
;; (Map, Object) overload

(deftest test-validate-map-object
  (testing "valid with map schema"
    (let [schema (java.util.LinkedHashMap. {"type" "integer" "minimum" 0})
          result (js/-validate schema 42)]
      (is (instance? java.util.Map result))
      (is (true? (.get ^java.util.Map result "valid")))))
  (testing "invalid with map schema"
    (let [schema (java.util.LinkedHashMap. {"type" "integer"})
          result (js/-validate schema "not-an-int")]
      (is (false? (.get ^java.util.Map result "valid"))))))

;;------------------------------------------------------------------------------
;; (String, String, Map) overload

(deftest test-validate-string-string-opts
  (testing "with draft option"
    (let [opts (java.util.HashMap. {"draft" "draft7"})
          result (js/-validate "{\"type\":\"string\"}" "\"hi\"" opts)]
      (is (true? (.get ^java.util.Map result "valid")))))
  (testing "with strictFormat option"
    (let [opts (java.util.HashMap. {"strictFormat" true})
          result (js/-validate "{\"type\":\"string\",\"format\":\"email\"}" "\"not-email\"" opts)]
      (is (false? (.get ^java.util.Map result "valid")))))
  (testing "with strictInteger option"
    (let [opts (java.util.HashMap. {"strictInteger" true})
          result (js/-validate "{\"type\":\"integer\"}" "1.0" opts)]
      (is (false? (.get ^java.util.Map result "valid"))))))

;;------------------------------------------------------------------------------
;; (Map, Object, Map) overload

(deftest test-validate-map-object-opts
  (testing "with draft option"
    (let [schema (java.util.LinkedHashMap. {"type" "object" "required" ["name"]})
          opts (java.util.HashMap. {"draft" "draft2020-12"})
          result (js/-validate schema {"name" "Alice"} opts)]
      (is (true? (.get ^java.util.Map result "valid")))))
  (testing "nil opts works"
    (let [schema (java.util.LinkedHashMap. {"type" "string"})
          result (js/-validate schema "hello" nil)]
      (is (true? (.get ^java.util.Map result "valid"))))))

;;------------------------------------------------------------------------------
;; Error structure is java.util.Map/List all the way down

(deftest test-error-structure
  (let [result (js/-validate "{\"type\":\"object\",\"properties\":{\"age\":{\"type\":\"integer\"}},\"required\":[\"age\"]}"
                             "{\"age\":\"old\"}")]
    (is (false? (.get ^java.util.Map result "valid")))
    (let [errors (.get ^java.util.Map result "errors")]
      (is (instance? java.util.List errors))
      (let [e (.get ^java.util.List errors 0)]
        (is (instance? java.util.Map e))
        (is (string? (.get ^java.util.Map e "message")))
        (is (instance? java.util.List (.get ^java.util.Map e "schemaPath")))))))
