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

(ns m3.validate-test
  (:require 
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   [m3.validate :refer [compile-m2]]))


;; compile orders and groups schema pairs ready for validation

;; integrate into validate

;; - change group ids to group itself
;; change singleton id to [singleton]

;; once we have ordering, with unevaluatedProperties last, we can go back to implementing it...

(deftest compile-m2-test
  (testing "simple"

    (is (=
         [[["type"]                                                                          ["object"]]
          [["required"]                                                                      [[]]]
          [["properties" "patternProperties" "additionalProperties" "unevaluatedProperties"] [{} :absent false :absent]]]
         (compile-m2
          {"type" "object",
           "properties" {},
           "additionalProperties" false,
           "required" []})))))
