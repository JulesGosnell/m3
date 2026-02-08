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

(ns m3.interop-test
  "JVM interop tests: verify M3 validates java.util.Map/List from Jackson and Gson
   without any Clojure conversion."
  (:require [clojure.test :refer [deftest testing is]]
            [m3.validate :as v])
  (:import [com.fasterxml.jackson.databind ObjectMapper]
           [com.google.gson Gson]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(def ^ObjectMapper jackson (ObjectMapper.))
(def ^Gson gson (Gson.))

(defn jackson-parse
  "Parse a JSON string via Jackson into java.util.Map / java.util.List / primitives."
  ^Object [^String json]
  (.readValue jackson json Object))

(defn gson-parse
  "Parse a JSON string via Gson into java.util.Map / java.util.List / primitives."
  ^Object [^String json]
  (.fromJson gson json Object))

(def c2 {:draft :draft2020-12 :quiet? true})
(def c1 {:draft :draft2020-12})

(defn valid? [schema document]
  (:valid? (v/validate c2 schema c1 document)))

(defn invalid? [schema document]
  (not (valid? schema document)))

;; ---------------------------------------------------------------------------
;; Schemas (plain Clojure maps -- schemas are always authored in Clojure)
;; ---------------------------------------------------------------------------

(def type-string  {"type" "string"})
(def type-number  {"type" "number"})
(def type-integer {"type" "integer"})
(def type-boolean {"type" "boolean"})
(def type-null    {"type" "null"})
(def type-object  {"type" "object"})
(def type-array   {"type" "array"})

(def object-schema
  {"type" "object"
   "properties" {"name" {"type" "string"}
                  "age"  {"type" "integer"}}
   "required" ["name"]})

(def array-schema
  {"type" "array"
   "items" {"type" "integer"}})

(def nested-schema
  {"type" "object"
   "properties" {"user" {"type" "object"
                          "properties" {"name" {"type" "string"}
                                        "tags" {"type" "array"
                                                "items" {"type" "string"}}}
                          "required" ["name"]}}
   "required" ["user"]})

(def ref-schema
  {"type" "object"
   "properties" {"primary"   {"$ref" "#/$defs/color"}
                  "secondary" {"$ref" "#/$defs/color"}}
   "$defs" {"color" {"type" "string"
                      "enum" ["red" "green" "blue"]}}})

;; ---------------------------------------------------------------------------
;; Jackson tests
;; ---------------------------------------------------------------------------

(deftest jackson-simple-types
  (testing "string"
    (is (valid? type-string (jackson-parse "\"hello\"")))
    (is (invalid? type-string (jackson-parse "42"))))

  (testing "number"
    (is (valid? type-number (jackson-parse "3.14")))
    (is (valid? type-number (jackson-parse "42")))
    (is (invalid? type-number (jackson-parse "\"nope\""))))

  (testing "integer"
    (is (valid? type-integer (jackson-parse "42")))
    (is (invalid? type-integer (jackson-parse "3.14"))))

  (testing "boolean"
    (is (valid? type-boolean (jackson-parse "true")))
    (is (valid? type-boolean (jackson-parse "false")))
    (is (invalid? type-boolean (jackson-parse "1"))))

  (testing "null"
    (is (valid? type-null (jackson-parse "null")))
    (is (invalid? type-null (jackson-parse "0")))))

(deftest jackson-object-validation
  (testing "valid object"
    (let [doc (jackson-parse "{\"name\": \"Alice\", \"age\": 30}")]
      (is (instance? java.util.Map doc))
      (is (valid? object-schema doc))))

  (testing "missing required property"
    (is (invalid? object-schema (jackson-parse "{\"age\": 30}"))))

  (testing "wrong property type"
    (is (invalid? object-schema (jackson-parse "{\"name\": 42}")))))

(deftest jackson-array-validation
  (testing "valid array"
    (let [doc (jackson-parse "[1, 2, 3]")]
      (is (instance? java.util.List doc))
      (is (valid? array-schema doc))))

  (testing "invalid array element"
    (is (invalid? array-schema (jackson-parse "[1, \"two\", 3]"))))

  (testing "empty array is valid"
    (is (valid? array-schema (jackson-parse "[]")))))

(deftest jackson-nested-objects
  (testing "valid nested"
    (let [doc (jackson-parse "{\"user\": {\"name\": \"Bob\", \"tags\": [\"admin\", \"dev\"]}}")]
      (is (valid? nested-schema doc))))

  (testing "missing nested required"
    (is (invalid? nested-schema (jackson-parse "{\"user\": {\"tags\": [\"x\"]}}"))))

  (testing "wrong nested array item type"
    (is (invalid? nested-schema
                  (jackson-parse "{\"user\": {\"name\": \"Bob\", \"tags\": [1, 2]}}")))))

(deftest jackson-numeric-edge-cases
  (testing "large integer"
    (is (valid? type-integer (jackson-parse "9999999999999"))))

  (testing "negative number"
    (is (valid? type-number (jackson-parse "-42.5"))))

  (testing "zero"
    (is (valid? type-integer (jackson-parse "0")))
    (is (valid? type-number (jackson-parse "0"))))

  (testing "float is not integer"
    (is (invalid? type-integer (jackson-parse "1.5")))))

(deftest jackson-ref-schema
  (testing "valid $ref"
    (is (valid? ref-schema (jackson-parse "{\"primary\": \"red\", \"secondary\": \"blue\"}"))))

  (testing "invalid $ref value"
    (is (invalid? ref-schema (jackson-parse "{\"primary\": \"yellow\"}")))))

;; ---------------------------------------------------------------------------
;; Gson tests
;; ---------------------------------------------------------------------------

(deftest gson-simple-types
  (testing "string"
    (is (valid? type-string (gson-parse "\"hello\"")))
    (is (invalid? type-string (gson-parse "42"))))

  (testing "number"
    (is (valid? type-number (gson-parse "3.14")))
    (is (valid? type-number (gson-parse "42")))
    (is (invalid? type-number (gson-parse "\"nope\""))))

  (testing "integer -- Gson parses all numbers as Double"
    ;; Gson parses 42 as 42.0 (Double); the validator should still accept it
    ;; as an integer since it has no fractional part.
    (is (valid? type-integer (gson-parse "42")))
    (is (invalid? type-integer (gson-parse "3.14"))))

  (testing "boolean"
    (is (valid? type-boolean (gson-parse "true")))
    (is (valid? type-boolean (gson-parse "false")))
    (is (invalid? type-boolean (gson-parse "1"))))

  (testing "null"
    (is (valid? type-null (gson-parse "null")))
    (is (invalid? type-null (gson-parse "0")))))

(deftest gson-object-validation
  (testing "valid object"
    (let [doc (gson-parse "{\"name\": \"Alice\", \"age\": 30}")]
      (is (instance? java.util.Map doc))
      (is (valid? object-schema doc))))

  (testing "missing required property"
    (is (invalid? object-schema (gson-parse "{\"age\": 30}"))))

  (testing "wrong property type"
    (is (invalid? object-schema (gson-parse "{\"name\": 42}")))))

(deftest gson-array-validation
  (testing "valid array"
    (let [doc (gson-parse "[1, 2, 3]")]
      (is (instance? java.util.List doc))
      (is (valid? array-schema doc))))

  (testing "invalid array element"
    (is (invalid? array-schema (gson-parse "[1, \"two\", 3]"))))

  (testing "empty array is valid"
    (is (valid? array-schema (gson-parse "[]")))))

(deftest gson-nested-objects
  (testing "valid nested"
    (let [doc (gson-parse "{\"user\": {\"name\": \"Bob\", \"tags\": [\"admin\", \"dev\"]}}")]
      (is (valid? nested-schema doc))))

  (testing "missing nested required"
    (is (invalid? nested-schema (gson-parse "{\"user\": {\"tags\": [\"x\"]}}"))))

  (testing "wrong nested array item type"
    (is (invalid? nested-schema
                  (gson-parse "{\"user\": {\"name\": \"Bob\", \"tags\": [1, 2]}}")))))

(deftest gson-numeric-edge-cases
  (testing "negative number"
    (is (valid? type-number (gson-parse "-42.5"))))

  (testing "zero"
    (is (valid? type-integer (gson-parse "0")))
    (is (valid? type-number (gson-parse "0"))))

  (testing "float is not integer"
    (is (invalid? type-integer (gson-parse "1.5")))))

(deftest gson-ref-schema
  (testing "valid $ref"
    (is (valid? ref-schema (gson-parse "{\"primary\": \"red\", \"secondary\": \"blue\"}"))))

  (testing "invalid $ref value"
    (is (invalid? ref-schema (gson-parse "{\"primary\": \"yellow\"}")))))
