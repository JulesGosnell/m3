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

(ns m3.vocabulary-test
  (:require
   [m3.property :refer [check-property-const check-property-type]]
   [m3.vocabulary :refer [make-dialect-2]]
   #?(:clj [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer [deftest testing is] :include-macros true])))

(def v
  {"https://json-schema.org/draft/2020-12/vocab/core"              true
   "https://json-schema.org/draft/2020-12/vocab/applicator"        true
   "https://json-schema.org/draft/2020-12/vocab/unevaluated"       true
   "https://json-schema.org/draft/2020-12/vocab/validation"        true
   "https://json-schema.org/draft/2020-12/vocab/meta-data"         true
   "https://json-schema.org/draft/2020-12/vocab/format-annotation" true
   "https://json-schema.org/draft/2020-12/vocab/content"           true})

;; (deftest test-make-dialect-2
;;   (let [dialect (make-dialect-2 :draft2020-12 v)]
;;     (testing "simple"
;;       (is (=
;;            [[["const" "hello"] check-property-const]
;;             [["type" "string"] check-property-type]]
;;            (dialect {"type" "string" "const" "hello"}))))))
    
