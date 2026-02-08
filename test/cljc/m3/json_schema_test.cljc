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
