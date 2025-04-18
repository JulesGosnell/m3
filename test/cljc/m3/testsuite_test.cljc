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
   [m3.validate :refer [validate uri->continuation json-decode]]]
  [:import
   #?(:clj  [java.io File])])

;;------------------------------------------------------------------------------
;; file stuff

#?(:cljs (def fs (js/require "fs")))

#?(:cljs (def node-path (js/require "path")))

#?(:cljs (defn file [^String s] s))

(defn file-name [f]
  #?(:clj (.getName ^File f)
     :cljs (.basename node-path f)))   

(defn directory? [f]
  #?(:clj (.isDirectory ^File f)
     :cljs (-> (.statSync fs f) .isDirectory)))

#?(:cljs (defn slurp [path] (.readFileSync fs path "utf8")))

#?(:cljs (def Throwable js/Error))

#?(:cljs
   (defn file-seq [d]
     (letfn [(node-file-seq [dir]
               ;; Each directory yields itself plus its children
               (lazy-seq
                (cons dir
                      (mapcat
                       (fn [entry]
                         (let [full-path (.resolve node-path dir entry)
                               stats (.statSync fs full-path)]
                           (if (.isDirectory stats)
                             (node-file-seq full-path)
                             (list full-path))))
                       (.readdirSync fs dir)))))]
       (node-file-seq d))))

#?(:cljs (defn format [fmt & args] (apply gstring/format fmt args)))

;;------------------------------------------------------------------------------
;; run our validator against: https://github.com/json-schema-org/JSON-Schema-Test-Suite.git JSON-Schema-Test-Suite

(defn json->string [json]
  (with-out-str (pprint json)))

;; TODO:
;; - make sure that we are recursing into all subdirectories
;; - implement vocabularies
;; - evaluation - unevaluated properties, items, dynamic anchors, refs...
;; - maybe fix remaining tests

(def exclude-module?
  #{
    ;; difficult - see comment in validate.cljc
    "idn-hostname.json"

    ;; leave these til last - maybe a big job
    "unevaluatedItems.json"
    ;;"unevaluatedProperties.json"
    })

(def exclude-test?
  #{;; I think we need $vocabulary working as the remote schema is draft7 but contains a dependentRequired in it...

    ;; [{"type" "object",
    ;;   "allOf"
    ;;   [{"properties" {"foo" true}}
    ;;    {"$ref"
    ;;     "http://localhost:1234/draft7/ignore-dependentRequired.json"}]}
    ;;  {"foo" "any value"}]

    ;; {"$id": "http://localhost:1234/draft7/integer.json",
    ;;  "$schema": "http://json-schema.org/draft-07/schema#",
    ;;  "dependentRequired": {"foo": ["bar"]}}

    ["cross-draft.json" "refs to historic drafts are processed as historic drafts" "missing bar is valid"]
    ["cross-draft.json" "refs to historic drafts are processed as historic drafts" "first item not a string is valid"]

    ["defs.json" "validate definition against metaschema" "invalid definition schema"]
    ["definitions.json" "validate definition against metaschema" "invalid definition schema"] ;; new

    ["dynamicRef.json" "A $dynamicRef resolves to the first $dynamicAnchor still in scope that is encountered when the schema is evaluated" "An array containing non-strings is invalid"]
    ["dynamicRef.json" "A $dynamicRef with intermediate scopes that don't include a matching $dynamicAnchor does not affect dynamic scope resolution" "An array containing non-strings is invalid"]
    ["dynamicRef.json" "A $dynamicRef that initially resolves to a schema with a matching $dynamicAnchor resolves to the first $dynamicAnchor in the dynamic scope" "The recursive part is not valid against the root"]
    ["dynamicRef.json" "multiple dynamic paths to the $dynamicRef keyword" "number list with string values"]
    ["dynamicRef.json" "multiple dynamic paths to the $dynamicRef keyword" "string list with number values"]
    ["dynamicRef.json" "after leaving a dynamic scope, it is not used by a $dynamicRef" "string matches /$defs/thingy, but the $dynamicRef does not stop here"]
    ["dynamicRef.json" "after leaving a dynamic scope, it is not used by a $dynamicRef" "first_scope is not in dynamic scope for the $dynamicRef"]
    ["dynamicRef.json" "strict-tree schema, guards against misspelled properties" "instance with misspelled field"]
    ["dynamicRef.json" "tests for implementation dynamic anchor and reference link" "incorrect extended schema"]
    ["dynamicRef.json" "$ref and $dynamicAnchor are independent of order - $defs first" "incorrect extended schema"]
    ["dynamicRef.json" "$ref and $dynamicAnchor are independent of order - $ref first" "incorrect extended schema"]
    ["dynamicRef.json" "$ref to $dynamicRef finds detached $dynamicAnchor" "non-number is invalid"]
    ["dynamicRef.json" "A $dynamicRef resolves to the first $dynamicAnchor still in scope that is encountered when the schema is evaluated" "An array of strings is valid"]
    ["dynamicRef.json" "A $dynamicRef with intermediate scopes that don't include a matching $dynamicAnchor does not affect dynamic scope resolution" "An array of strings is valid"]
    ["dynamicRef.json" "$dynamicAnchor inside propertyDependencies" "expected strings - additional property as string is valid"]
    ["dynamicRef.json" "$dynamicAnchor inside propertyDependencies" "expected integers - additional property as integer is valid"]

    ;; 'stash' needs to understand json to fix these issues...
    ["anchor.json" "$anchor inside an enum is not a real identifier" "match $ref to $anchor"] ;; schema would fail validation
    ["id.json" "$id inside an enum is not a real identifier" "match $ref to $id"] ;; schema would fail validation
    ["id.json" "id inside an enum is not a real identifier" "match $ref to id"] ;; schema would fail validation
    ["unknownKeyword.json" "$id inside an unknown keyword is not a real identifier" "type matches second anyOf, which has a real schema in it"] ;; schema would fail validation
    ["unknownKeyword.json" "$id inside an unknown keyword is not a real identifier" "type matches non-schema in first anyOf"]
    ["unknownKeyword.json" "$id inside an unknown keyword is not a real identifier" "type matches non-schema in third anyOf"]

    ["recursiveRef.json" "$recursiveRef without $recursiveAnchor works like $ref" "recursive mismatch"]
    ["recursiveRef.json" "$recursiveRef with nesting" "integer now matches as a property value"]
    ["recursiveRef.json" "$recursiveRef with nesting" "two levels, properties match with $recursiveRef"]
    ["recursiveRef.json" "multiple dynamic paths to the $recursiveRef keyword" "recurse to integerNode - floats are not allowed"]
    ["recursiveRef.json" "dynamic $recursiveRef destination (not predictable at schema compile time)" "integer node"]

    ;; we need unevaluatedProperties/items for these
    ["not.json" "collect annotations inside a 'not', even if collection is disabled" "unevaluated property"] ;; needs unevaluatedProperties
    ["ref.json" "ref creates new scope when adjacent to keywords" "referenced subschema doesn't see annotations from properties"]  ;; needs unevaluatedProperties
    ["ref.json" "$ref with $recursiveAnchor" "extra items allowed for inner arrays"] ;; needs unevaluatedItems

    ;; new - after I decided to choose first rather than last $id in a meld
    ["dynamicRef.json" "An $anchor with the same name as a $dynamicAnchor is not used for dynamic scope resolution" "Any array is valid"]
    ["dynamicRef.json" "A $dynamicRef without a matching $dynamicAnchor in the same schema resource behaves like a normal $ref to $anchor" "Any array is valid"]
    ["dynamicRef.json" "A $dynamicRef with a non-matching $dynamicAnchor in the same schema resource behaves like a normal $ref to $anchor" "Any array is valid"]
    ["dynamicRef.json" "$dynamicAnchor inside propertyDependencies" "expected strings - additional property as not string is invalid"]
    ["dynamicRef.json" "$dynamicAnchor inside propertyDependencies" "expected integers - additional property as not integer is invalid"]

    ["refRemote.json" "retrieved nested refs resolve relative to their URI not $id" "number is invalid"]

    ;; leap-second support seems to be an issue with java-time
    ;; https://coderanch.com/t/667087/java/Reconciling-Java-date-time-GPS
    ["date-time.json" "validation of date-time strings" "a valid date-time with a leap second, UTC"]
    ["date-time.json" "validation of date-time strings" "a valid date-time with a leap second, with minus offset"]
    ["time.json" "validation of time strings" "a valid time string with leap second, Zulu"]
    ["time.json" "validation of time strings" "valid leap second, zero time-offset"]
    ["time.json" "validation of time strings" "valid leap second, positive time-offset"]
    ["time.json" "validation of time strings" "valid leap second, large positive time-offset"]
    ["time.json" "validation of time strings" "valid leap second, negative time-offset"]
    ["time.json" "validation of time strings" "valid leap second, large negative time-offset"]

    ;; needs $vocabulary support
    ["vocabulary.json" "schema that uses custom metaschema with with no validation vocabulary" "no validation: invalid number, but it still validates"]

    ;; issues with cljs
    #?@(:cljs
        [;; javascript treats numbers differently - grrr
         ["zeroTerminatedFloats.json" "some languages do not distinguish between different types of numeric value" "a float is not an integer even without fractional part"]])

      ["not.json" "collect annotations inside a 'not', even if collection is disabled" "annotations are still collected inside a 'not'"]
      ["unevaluatedProperties.json" "dependentSchemas with unevaluatedProperties" "unevaluatedProperties sees bar when foo2 is present"]
      ["unevaluatedProperties.json" "dynamic evalation inside nested refs" "a is valid"]
      ["unevaluatedProperties.json" "dynamic evalation inside nested refs" "all + foo is valid"]
      ["unevaluatedProperties.json" "dynamic evalation inside nested refs" "all is valid"]
      ["unevaluatedProperties.json" "dynamic evalation inside nested refs" "b is valid"]
      ["unevaluatedProperties.json" "dynamic evalation inside nested refs" "c is valid"]
      ["unevaluatedProperties.json" "dynamic evalation inside nested refs" "d is valid"]
      ["unevaluatedProperties.json" "dynamic evalation inside nested refs" "xx + foox is valid"]
      ["unevaluatedProperties.json" "dynamic evalation inside nested refs" "xx is valid"]
      ["unevaluatedProperties.json" "dynamic evaluation inside nested refs" "a is valid"]
      ["unevaluatedProperties.json" "dynamic evaluation inside nested refs" "all + foo is valid"]
      ["unevaluatedProperties.json" "dynamic evaluation inside nested refs" "all is valid"]
      ["unevaluatedProperties.json" "dynamic evaluation inside nested refs" "b is valid"]
      ["unevaluatedProperties.json" "dynamic evaluation inside nested refs" "c is valid"]
      ["unevaluatedProperties.json" "dynamic evaluation inside nested refs" "d is valid"]
      ["unevaluatedProperties.json" "dynamic evaluation inside nested refs" "xx + foox is valid"]
      ["unevaluatedProperties.json" "dynamic evaluation inside nested refs" "xx is valid"]
      ["unevaluatedProperties.json" "nested unevaluatedProperties, outer false, inner true, properties inside" "with nested unevaluated properties"]
      ["unevaluatedProperties.json" "nested unevaluatedProperties, outer false, inner true, properties inside" "with no nested unevaluated properties"]
      ["unevaluatedProperties.json" "nested unevaluatedProperties, outer false, inner true, properties outside" "with nested unevaluated properties"]
      ["unevaluatedProperties.json" "propertyDependencies with unevaluatedProperties" "unevaluatedProperties sees buz when foo2 is present"]
      ["unevaluatedProperties.json" "unevaluatedProperties + ref inside allOf / oneOf" "a and b and x are valid"]
      ["unevaluatedProperties.json" "unevaluatedProperties + ref inside allOf / oneOf" "a and b and y are valid"]
      ["unevaluatedProperties.json" "unevaluatedProperties + ref inside allOf / oneOf" "a and x are valid"]
      ["unevaluatedProperties.json" "unevaluatedProperties + ref inside allOf / oneOf" "a and y are valid"]
      ["unevaluatedProperties.json" "unevaluatedProperties can see annotations from if without then and else" "valid in case if is evaluated"]
      ["unevaluatedProperties.json" "unevaluatedProperties can see inside propertyDependencies" "allows bar if foo = foo1"]
      ["unevaluatedProperties.json" "unevaluatedProperties with $dynamicRef" "with no unevaluated properties"]
      ["unevaluatedProperties.json" "unevaluatedProperties with anyOf" "when one matches and has no unevaluated properties"]
      ["unevaluatedProperties.json" "unevaluatedProperties with anyOf" "when two match and has no unevaluated properties"]
      ["unevaluatedProperties.json" "unevaluatedProperties with dependentSchemas" "with no unevaluated properties"]
      ["unevaluatedProperties.json" "unevaluatedProperties with if/then/else" "when if is false and has no unevaluated properties"]
      ["unevaluatedProperties.json" "unevaluatedProperties with if/then/else" "when if is true and has no unevaluated properties"]
      ["unevaluatedProperties.json" "unevaluatedProperties with if/then/else, else not defined" "when if is true and has no unevaluated properties"]
      ["unevaluatedProperties.json" "unevaluatedProperties with if/then/else, then not defined" "when if is false and has no unevaluated properties"]
      ["unevaluatedProperties.json" "unevaluatedProperties with nested additionalProperties" "with additional properties"]
      ["unevaluatedProperties.json" "unevaluatedProperties with nested patternProperties" "with no additional properties"]
      ["unevaluatedProperties.json" "unevaluatedProperties with nested properties" "with no additional properties"]
      ["unevaluatedProperties.json" "unevaluatedProperties with nested unevaluatedProperties" "with nested unevaluated properties"]
      ["unevaluatedProperties.json" "unevaluatedProperties with oneOf" "with no unevaluated properties"]
    })

(defn load-schema [s]
  (json-decode (slurp (format "resources/schemas/%s/schema.json" s))))

;; TODO: remote schemas should:
;; - default to the base-uri from which they loaded
;; - have their own self-contained context inheriting nothing from the client schema
;; - indicate their draft via $schema
;; - be treated according to their draft (hmmm... but what happens if you import a bit of schema of a different draft - I guess you switch the :draft in the context)
;; - be loaded as resources not files
;; hmmm... this means that context etc should be threaded through ref resolution so schema below the ref can switch its context etc...
;; I ithink uri->schema might need to become base -> path -> uri -> schema ...
;; we should validate remoteSchemas as we load them
;; see validate.uri->schema - TODO - share


(def uri-base->dir
  {"http://json-schema.org" "resources/schemas"
   "https://json-schema.org" "resources/schemas"
   "http://localhost:1234" "test-resources/JSON-Schema-Test-Suite/remotes"})

(defn is-validated [c2 m2 c1 m1 & [sign]]
  (let [{v? :valid? es :errors} (validate c2 m2 c1 m1)]
    (if (or (nil? sign) (true? sign))
      (is v? (json->string es))
      (is (not v?) (str "should not be valid: " (pr-str m2 m1))))))

(defn test-file [dname f draft]
  (let [feature (file-name f)]
    (testing (str "\"" feature "\"")
      (if (not (exclude-module? (file-name f)))
        (doseq [{d1 "description" m2 "schema" ts "tests"} (json-decode (slurp f))]
          (let [c2 {:draft draft :uri->schema (uri->continuation uri-base->dir)}
                c1 {:draft draft}]
            (testing (str "\"" d1 "\"")
              (doseq [{d2 "description" m1 "data" v? "valid"} ts]
                (testing (str "\"" d2 "\"" ":")
                  (if (not (exclude-test? [(file-name f) d1 d2]))
                    (do
                      (let [c2 (-> c2
                                   (assoc
                                    :strict-format?
                                    ;; there are a bunch of tests which insist that a bad format should not break validation
                                    (not (and (= "format.json" feature)
                                              (re-matches #"^invalid .* string is only an annotation by default$" d2))))
                                   (assoc
                                    :strict-integer?
                                    (and (= "zeroTerminatedFloats.json" feature)
                                         (= "a float is not an integer even without fractional part" d2))))]
                        ;;(prn "testing: " dname (file-name f) d1 d2)
                        (try (testing "m2/m1" (is-validated c2 m2 c1 m1 v?)) (catch Throwable e (prn [(file-name f) d1 d2] e)))))
                    ;;(println "skipping:" d2)
                    ))))))))))
  
(defn test-directory [d draft]
  (let [dname (file-name d)]
    (testing (str dname ":")
      (doseq [f (drop 1 (file-seq d))]
        (if (directory? f)
          (test-directory f draft)
          (test-file dname f draft))))))

(def json-schema-test-suite-root "test-resources/JSON-Schema-Test-Suite/tests/")

(deftest json-schema-test-suite
  (doseq [[draft dir] [;;["draft3"       "draft3" "draft3/optional" "draft3/optional/format"] - exists but we are not setup for it yet - need to reclassify stuff out of draft4
                       ["draft4"       "draft4"]
                       ;;"draft5" - does not exist !
                       ["draft6"       "draft6"]
                       ["draft6"       "draft6/optional"]
                       ["draft6"       "draft6/optional/format"]
                       ["draft7"       "draft7"]
                       ["draft7"       "draft7/optional"]
                       ["draft7"       "draft7/optional/format"]
                       ["draft2019-09" "draft2019-09"]
                       ["draft2019-09" "draft2019-09/optional"]
                       ["draft2019-09" "draft2019-09/optional/format"]
                       ["draft2020-12" "draft2020-12"]
                       ["draft2020-12" "draft2020-12/optional"]
                       ["draft2020-12" "draft2020-12/optional/format"]
                       ["draft-next"  "draft-next"]
                       ["draft-next"  "draft-next/optional"]
                       ["draft-next"  "draft-next/optional/format"]]]
    (test-directory (file (str json-schema-test-suite-root dir)) draft)))

;;------------------------------------------------------------------------------

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

(defn test-m1 [c2 m2 {d "description" m1 "data" v? "valid"}]
  (testing d
    (is-validated c2 m2 {} m1 v?)))
    

(defn test-m2 [{d "description" m2 "schema" tests "tests"}]
  (testing d
    (doseq [test tests] (test-m1 {:draft "draft2019-09"} m2 test))))
