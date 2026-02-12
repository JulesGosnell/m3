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
   [clojure.string]
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
  #{;; JSON has no integer/float distinction — 1.0 is a valid integer.
    ;; This optional test tests language-specific integer semantics.
    ["zeroTerminatedFloats.json" "some languages do not distinguish between different types of numeric value" "a float is not an integer even without fractional part"]
    ;; CLJS: JS has no integer/float distinction — JSON.parse("1.0") === JSON.parse("1")
    #?@(:cljs
        [["zeroTerminatedFloats.json" "some languages do not distinguish between different types of numeric value" "a float is not an integer even without fractional part"]])
    })

;; Drafts where format is annotation-only by vocabulary.
;; draft3: metaschema uses "format":"uri" on $ref — relative refs fail assertion.
;; draft2020-12/draft-next/latest: format is annotation per spec.
;; The optional/format test suite tests format as assertion, so skip for these.
(def format-annotation-draft?
  #{:draft3 :draft2020-12 :draft-next :latest})

(def uri-base->dir
  {"http://json-schema.org" "resources/schemas"
   "https://json-schema.org" "resources/schemas"
   "http://localhost:1234" "test-resources/JSON-Schema-Test-Suite/remotes"})

(def test-uri->schema (uri->continuation uri-base->dir))

;;------------------------------------------------------------------------------
;; Warning tracking — assert that the test suite produces expected warning counts.
;; Warnings come from: $comment (compile-time), format annotations (runtime),
;; and content annotations (runtime). If a change adds or removes warnings,
;; these counts will break — update them deliberately.

(def warning-counts (atom {}))
(def info-counts (atom {}))

;; Expected per-draft individual warning counts from the test suite.
;; Warnings come from: format annotations, content annotations, deprecated, etc.
;; Infos come from: $comment.
;; draft3-draft6: no warnings or infos ($comment not in vocab, no annotation keywords).
;; draft7: $comment produces infos only (no annotation warnings).
;; draft2019-09: content annotation warnings + $comment infos.
;; draft2020-12/draft-next: format + content annotation warnings + $comment infos.
(def expected-warning-counts
  {:draft2019-09 12
   :draft2020-12 31
   :draft-next   31})

(def expected-info-counts
  {:draft7       12
   :draft2019-09 31
   :draft2020-12 57
   :draft-next   41})

;;------------------------------------------------------------------------------

(defn is-validated [c2 m2 c1 m1 expected-valid?]
  (let [{v? :valid? es :errors ws :warnings infos :infos} (validate c2 m2 c1 m1)]
    (when (seq ws)
      (swap! warning-counts update (:draft c2) (fnil + 0) (count ws)))
    (when (seq infos)
      (swap! info-counts update (:draft c2) (fnil + 0) (count infos)))
    (if expected-valid?
      (is v? (with-out-str (pprint es)))
      (is (not v?) (str "should not be valid: " (pr-str m2 m1))))))

(defn test-file [f draft]
  (let [feature (file-name f)
        dir (directory-name f)
        ;; Skip optional/format tests for drafts where format is annotation-only per spec.
        ;; These tests expect format as assertion; annotation drafts can't pass them.
        skip-format? (and (format-annotation-draft? draft)
                          (clojure.string/includes? (str dir) "optional/format"))]
    (when-not skip-format?
      (testing feature
        (doseq [{d1 "description" m2 "schema" ts "tests"} (json-decode (slurp f))]
          (testing d1
            (doseq [{d2 "description" m1 "data" v? "valid"} ts]
              (testing d2
                (when-not (exclude-test? [feature d1 d2])
                  (let [c2 {:draft draft
                            :uri->schema test-uri->schema
                            :quiet? true}
                        c1 {:draft draft}]
                    (try
                      (is-validated c2 m2 c1 m1 v?)
                      (catch Throwable e
                        (prn [feature d1 d2] e)))))))))))))

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
  (reset! warning-counts {})
  (reset! info-counts {})
  (doseq [d (list-children (file json-schema-test-suite-root))
          :when (directory? d)
          :when (not (symlink? d))]
    (let [draft (keyword (file-name d))]
      (test-directory d draft)))
  (testing "warning counts match expectations"
    (let [wc @warning-counts]
      (doseq [draft [:draft3 :draft4 :draft6 :draft7]]
        (is (nil? (wc draft)) (str draft " should produce no warnings")))
      (doseq [[draft expected] expected-warning-counts]
        (is (= expected (wc draft))
            (str draft " expected " expected " warnings, got " (wc draft))))))
  (testing "info counts match expectations"
    (let [ic @info-counts]
      (doseq [draft [:draft3 :draft4 :draft6]]
        (is (nil? (ic draft)) (str draft " should produce no infos")))
      (doseq [[draft expected] expected-info-counts]
        (is (= expected (ic draft))
            (str draft " expected " expected " infos, got " (ic draft)))))))
