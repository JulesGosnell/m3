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

(ns m3.ref-test
  (:require 
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   [m3.ref :refer [resolve-uri deep-meld ->int-or-string]]
   [m3.json-schema :as m3]))

;;------------------------------------------------------------------------------

(deftest test-resolve-uri

  (testing "matches stashed $id"
    (let [uri {:type :path :path "/a/b"}
          c {"$id" "c"}
          m2 {"a" {"b" c}}
          path ["a"]
          ;; path ["a" "b"] has its own $id (it IS in path->uri), so try-path
          ;; walks to the parent ["a"]. No parent scope → falls back to ctx's id-uri.
          ;; Provide a parent scope so the test exercises the full resolution path.
          parent-uri {:type :path :path "/a"}
          ctx {:root m2 :id-uri parent-uri
               :uri->path {uri ["a" "b"]}
               :path->uri {["a" "b"] uri ["a"] parent-uri}}]
    (is (=
         [(merge ctx {:id-uri parent-uri}) path c]
         (resolve-uri ctx path uri :ref)))))
  )

;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; ->int-or-string — regression: must not choke on non-numeric strings that
;; start with a digit (e.g. hex hashes "9bffe", version strings "1.0")

(deftest test-int-or-string
  (testing "pure digits become integers"
    (is (= 0 (->int-or-string "0")))
    (is (= 42 (->int-or-string "42"))))
  (testing "non-numeric strings stay strings"
    (is (= "foo" (->int-or-string "foo")))
    (is (= "" (->int-or-string ""))))
  (testing "digit-prefixed non-numeric strings stay strings"
    (is (= "9bffe" (->int-or-string "9bffe")))
    (is (= "1.0" (->int-or-string "1.0")))
    (is (= "0ea16" (->int-or-string "0ea16")))
    (is (= "1edb" (->int-or-string "1edb")))))

;;------------------------------------------------------------------------------
;; Regression: schemas with non-numeric definition keys (hex hashes, version
;; strings) must compile and resolve $refs correctly.

(deftest test-non-numeric-definition-keys
  (testing "hex hash definition key with $ref"
    (is (:valid? (m3/validate
                  {"$schema" "http://json-schema.org/draft-07/schema#"
                   "definitions" {"9bffe" {"type" "object"
                                           "properties" {"name" {"type" "string"}}}}
                   "$ref" "#/definitions/9bffe"}
                  {"name" "test"}))))
  (testing "version string definition key with $ref"
    (is (:valid? (m3/validate
                  {"$schema" "http://json-schema.org/draft-07/schema#"
                   "definitions" {"1.0" {"type" "object"
                                         "properties" {"v" {"type" "string"}}}}
                   "$ref" "#/definitions/1.0"}
                  {"v" "hello"}))))
  (testing "invalid data against hex-keyed definition"
    (is (not (:valid? (m3/validate
                       {"$schema" "http://json-schema.org/draft-07/schema#"
                        "definitions" {"9bffe" {"type" "string"}}
                        "$ref" "#/definitions/9bffe"}
                       42))))))

;;------------------------------------------------------------------------------

(deftest test-meld-deep
  (testing "take first $id, last everything else"
    (is (=
         {"$id" 1, :a 1, :b 2, :c 3}
         (deep-meld {:id-key "$id"} {"$id" 1 :a 1 :b 1 :c 1} {"$id" 2 :b 2 :c 2} {"$id" 3 :c 3})))
    )
  )

