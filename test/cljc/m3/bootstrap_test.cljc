(ns m3.bootstrap-test
  (:require
   #?(:clj [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   [m3.uri :refer [parse-uri]]
   [m3.property :refer [check-property-properties check-property-$ref check-property-id make-check-property-format check-property-type]]
   [m3.draft :refer [$schema-uri->draft]]
   [m3.vocabulary :refer [draft->vocab sort-vocab new-make-dialect]]
   [m3.validate :refer [$schema-uri->schema check-schema]]))

;;------------------------------------------------------------------------------

;; we need to be able to inject:
;; - custom meta-schema
;; - custom $vocabulary

;; lets just schema recursion and dialects going first

;; we can assume $schema as it is part of the core vocab for all drafts
(defn new-validate [{$schema-uri->schema :$schema-uri->schema
                     $schema-uri->draft :$schema-uri->draft
                     draft->vocab :draft->vocab
                     :or {$schema-uri->schema $schema-uri->schema
                          $schema-uri->draft $schema-uri->draft
                          draft->vocab draft->vocab}
                     :as c2} {s "$schema" :as m2}]
  (prn "$schema : " s)
  (let [$schema-uri (parse-uri s)
        {$vocabulary "$vocabulary" :as m3} ($schema-uri->schema $schema-uri)]
    (if (= m3 m2)
      (let [c3 {:$schema-uri->schema $schema-uri->schema
                :$schema-uri->draft $schema-uri->draft
                :draft->vocab draft->vocab
                ;; we have to make the dialect for the m3/m2 validation;; this dialect can make another that it a beter fit to the m2 - consider how...
                :dialect (new-make-dialect (draft->vocab ($schema-uri->draft $schema-uri)) $vocabulary)}
            ;; we need to mke the dialect in order to find the first $schema and $vocabulary and yet we need them to make the dialect - how do we break out of this circularity ?
            ;; make them manually for the m3 and then let the dialect rebuild itself with the m2
            [c3 m3 f2] (check-schema c3 [] m3)]
        (prn "M3:" c3 m3 f2)
        (f2 c3 [] m2)
        ;; TODO: check results
        ;; we are at the top of the schema hierarchy
        ;; TODO:
        ;; m3 need to self-validate
        
        ;; m3 needs contain $vocabulary and we need to make dialect
        ;; needs to return f1 into which to plug m2 for validation
        f2)

      (do
        ;; returns you the f1
        ;; now plug in and validate m2
        ((new-validate c2 m3) {} [] m2)))))

;;------------------------------------------------------------------------------

(def m3-id "http://megalodon/schemas/metaschema")

(def m3
  {"$id" m3-id
   "$schema" m3-id
   "$vocabulary" {"https://json-schema.org/draft-03/vocab/core" true}
   "type" "object"
   "properties"
   {"$id" {"type" "string"}
    "$schema" {"type" "string"}
    ;; TODO: $vocabulary
    "type" {"type" "string"}
    "properties" {"type" "object"}}})

(def m2-id "http://megalodon/schemas/schema")

(def m2 {"$schema" m3-id
         "$id" m2-id
         "type" "string"
         ;;"const" "hello"
         })

(def custom-$schema-uri->schema
  {(parse-uri m3-id) m3
   (parse-uri m2-id) m2})

(def custom-$schema-uri->draft
  {(parse-uri m3-id) :draft-custom})

;; TODO: recurse to top of schema hierarchy bulding a dialect from most recent $vocabulary on way down
;; we will need a bootstrap dialect, marker index and definitions index too do this properly

(defn check-property-$schema [_property c2 _p2 m2 v2]
  (prn "HERE $SCHEMA L2:" m2 v2)
  ;; TODO:
  ;; in f2 we should:
  ;; - recurse to top of schema hierrchy
  ;; - descend hierarchy with a new :dialect, :draft and :schema-uri
  ;; - this should all be memoised so take no time
  ;; - validate our given m2 against our given $schema
  ;; - copy the given :dialect, :draft and :schema-uri into our c2 for subsequent checkers...
  [c2
   ;; (if-let [d ($schema->draft v2)]
   ;;   (update
   ;;    c2
   ;;    :draft
   ;;    (fn [old-d new-d]
   ;;      (when (not= old-d new-d) (log/info (str "switching draft: " old-d " -> " new-d)))
   ;;      new-d)
   ;;    d)
   ;;   c2)
   m2
   (fn [c1 _p1 m1]
     (prn "HERE $SCHEMA L1:" m1)
     [c1 m1 nil])])

(def custom-draft->vocab
  {:draft-custom
   (sort-vocab
    [["https://json-schema.org/draft-03/vocab/applicator"  "properties"    check-property-properties          #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/core"        "$ref"          check-property-$ref                #{"$schema" "id"}]
     ["https://json-schema.org/draft-03/vocab/core"        "$schema"       check-property-$schema             #{"$ref"}]
     ["https://json-schema.org/draft-03/vocab/core"        "id"            check-property-id                  #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"  "format"        (make-check-property-format false) #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"  "type"          check-property-type                #{"$schema"}]])})

(def m1 "test")

(deftest bootstrap_test
  ;; one test to check dialect is set up correctly
  (testing "bootstrap $schema"
    (let [[c2 m2 f1]
          (new-validate
           {:$schema-uri->schema custom-$schema-uri->schema
            :$schema-uri->draft custom-$schema-uri->draft
            :draft->vocab custom-draft->vocab}
           m2)]
      (prn "C2:" c2)
      (is (=
           ;; {:id-key "$id"
           ;;  :id-uri {:type :url :origin "http://megalodon" :path "/schemas/schema"}
           ;;  :original-root {"$schema" "http://megalodon/schemas/metaschema" "$id" "http://megalodon/schemas/schema" "type" "string"}
           ;;  :recursive-anchor []
           ;;  :root {"$schema" "http://megalodon/schemas/metaschema" "$id" "http://megalodon/schemas/schema" "type" "string"}
           ;;  :draft :latest
           ;;  :melder nil}
           {}
           c2))
      (prn "M2:" m2)
      (is (=
           {"$id" "http://megalodon/schemas/schema",
            "$schema" "http://megalodon/schemas/metaschema",
            "type" "string"}
           m2))
      (prn "F1:" f1)
      (is (=
           nil
           f1))

      ;; (let [[c1 m2 es] (f1 {} m1)]
      ;;   ;; TODO: add assertions
      ;;   )
                                        ;
      ))

  ;; TODO: a test to check marker index is being built correctly

  ;; TODO: a test to check that definition index is being built correctly
  )

;; what do we expect to come down ... ?
