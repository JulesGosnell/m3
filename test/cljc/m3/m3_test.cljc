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

(ns m3.m3-test
  "M3-specific regression tests (JSON testsuite format).
  Tests in test-resources/m3-tests/ exercise edge cases discovered
  during development that aren't covered by the official JSON Schema
  Test Suite."
  [:require
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   #?(:clj  [clojure.java.io :refer [file]])
   [clojure.string :refer [ends-with?]]
   [m3.platform :refer [json-decode]]
   [m3.testsuite-test :as ts]]
  [:import
   #?(:clj  [java.io File])])

;;------------------------------------------------------------------------------

#?(:cljs (defn slurp [path] (.readFileSync (js/require "fs") path "utf8")))

#?(:cljs (def Throwable js/Error))

;;------------------------------------------------------------------------------

(defn test-file
  "Run a JSON testsuite-format file. Draft is detected from each
  schema's $schema property (no explicit draft parameter needed)."
  [f]
  (testing (ts/file-name f)
    (doseq [{d1 "description" m2 "schema" ts "tests"} (json-decode (slurp f))]
      (testing d1
        (doseq [{d2 "description" m1 "data" v? "valid"} ts]
          (testing d2
            (try
              (ts/is-validated {:uri->schema ts/test-uri->schema :quiet? true}
                               m2 {} m1 v?)
              (catch Throwable e
                (is false (str "Exception: " e))))))))))

(defn list-json-files [dir]
  #?(:clj  (sort (filter #(ends-with? (.getName ^File %) ".json")
                          (.listFiles ^File (file dir))))
     :cljs (->> (.readdirSync (js/require "fs") dir)
                (into [])
                (filter #(ends-with? % ".json"))
                (map #(.resolve (js/require "path") dir %))
                sort)))

(def m3-test-root "test-resources/m3-tests/")

(deftest m3-regression-tests
  (doseq [f (list-json-files m3-test-root)]
    (test-file f)))
