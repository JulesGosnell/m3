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

(ns m3.testsuite-test
  [:require
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   #?(:clj  [clojure.java.io :refer [file]])
   #?(:cljs [goog.string :as gstring])
   #?(:cljs [goog.string.format])
   [clojure.pprint :refer [pprint]]
   [clojure.string :refer [ends-with?]]
   [m3.platform :refer [json-decode]]
   [m3.validate :refer [validate uri->continuation]]]
  [:import
   #?(:clj  [java.io File])])

;;------------------------------------------------------------------------------
;; file helpers

#?(:cljs (def fs (js/require "fs")))

#?(:cljs (def node-path (js/require "path")))

#?(:cljs (defn file [^String s] s))

(defn file-name [f]
  #?(:clj (.getName ^File f)
     :cljs (.basename node-path f)))

(defn directory-name [f]
  #?(:clj (.getParent ^File f)
     :cljs (.dirname node-path f)))

(defn directory? [f]
  #?(:clj (.isDirectory ^File f)
     :cljs (-> (.statSync fs f) .isDirectory)))

(defn symlink? [f]
  #?(:clj (java.nio.file.Files/isSymbolicLink (.toPath ^File f))
     :cljs (.isSymbolicLink (.lstatSync fs f))))

(defn list-children [d]
  #?(:clj  (sort (.listFiles ^File d))
     :cljs (->> (.readdirSync fs d) (into []) (map #(.resolve node-path d %)) sort)))

#?(:cljs (defn slurp [path] (.readFileSync fs path "utf8")))

#?(:cljs (def Throwable js/Error))

#?(:cljs (defn format [fmt & args] (apply gstring/format fmt args)))

;;------------------------------------------------------------------------------
;; JSON Schema Test Suite runner
;; https://github.com/json-schema-org/JSON-Schema-Test-Suite

(def exclude-test?
  #{;; CLJS: JS has no integer/float distinction — JSON.parse("1.0") === JSON.parse("1")
    #?@(:cljs
        [["zeroTerminatedFloats.json" "some languages do not distinguish between different types of numeric value" "a float is not an integer even without fractional part"]])
    })

(def uri-base->dir
  {"http://json-schema.org" "resources/schemas"
   "https://json-schema.org" "resources/schemas"
   "http://localhost:1234" "test-resources/JSON-Schema-Test-Suite/remotes"})

(defn is-validated [c2 m2 c1 m1 expected-valid?]
  (let [{v? :valid? es :errors} (validate c2 m2 c1 m1)]
    (if expected-valid?
      (is v? (with-out-str (pprint es)))
      (is (not v?) (str "should not be valid: " (pr-str m2 m1))))))

(defn test-file [f draft]
  (let [feature (file-name f)]
    (testing feature
      (doseq [{d1 "description" m2 "schema" ts "tests"} (json-decode (slurp f))]
        (testing d1
          (doseq [{d2 "description" m1 "data" v? "valid"} ts]
            (testing d2
              (when-not (exclude-test? [feature d1 d2])
                (let [c2 {:draft draft
                          :uri->schema (uri->continuation uri-base->dir)
                          :quiet? true
                          :strict-format? (ends-with? (directory-name f) "optional/format")
                          :strict-integer? (and (= "zeroTerminatedFloats.json" feature)
                                                (= "a float is not an integer even without fractional part" d2))}
                      c1 {:draft draft}]
                  (try
                    (is-validated c2 m2 c1 m1 v?)
                    (catch Throwable e
                      (prn [feature d1 d2] e))))))))))))

(defn test-directory [d draft]
  (testing (file-name d)
    (doall
     (#?(:clj pmap :cljs map)
      (fn [f]
        (if (directory? f)
          (test-directory f draft)
          (test-file f draft)))
      (list-children d)))))

(def json-schema-test-suite-root "test-resources/JSON-Schema-Test-Suite/tests/")

(deftest json-schema-test-suite
  (doseq [d (list-children (file json-schema-test-suite-root))
          :when (and (directory? d) (not (symlink? d)))]
    (test-directory d (keyword (file-name d)))))

;;------------------------------------------------------------------------------
;; REPL utilities

(defn index-by [k ms]
  (into (sorted-map) (map (fn [{v k :as m}][v m]) ms)))

(defn find-test [draft feature description test-name]
  (let [{m2 "schema" m1s "tests"}
        ((index-by
          "description"
          (json-decode
           (slurp
            (format
             "test-resources/JSON-Schema-Test-Suite/tests/%s/%s"
             draft
             feature))))
         description)
        m1 (((index-by "description" m1s) test-name) "data")]
    [m2 m1]))

;;------------------------------------------------------------------------------
