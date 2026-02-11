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

(ns m3.vocabulary
  (:require
   [#?(:clj clojure.tools.logging :cljs m3.log) :as log]
   [m3.util :refer [map-values topo-sort-by make-stable-sort-by third fourth]]
   [m3.uri :refer [parse-uri]]
   [m3.draft :refer [$schema-uri->draft]]
   [m3.ref :refer [meld-replace meld-deep-over]]
   [m3.type :refer [draft3-type->checker draft4-type->checker]]
   [m3.format :refer [draft3-format->checker draft4-format->checker
                       draft6-format->checker draft7-format->checker
                       draft2019-09-format->checker draft2020-12-format->checker
                       draft-next-format->checker]]
   [m3.property :refer
    [check-property-$anchor
     check-property-$comment
     check-property-$defs
     check-property-$dynamicAnchor
     check-property-$id
     check-property-$recursiveAnchor
     check-property-$recursiveRef
     check-property-additionalItems
     check-property-additionalProperties
     check-property-allOf
     check-property-anyOf
     check-property-const
     check-property-contains
     check-property-default
     check-property-definitions
     check-property-dependencies
     check-property-dependentRequired
     check-property-dependentSchemas
     check-property-deprecated
     check-property-description
     check-property-divisibleBy
     check-property-else
     check-property-enum
     check-property-examples
     check-property-exclusiveMaximum-new
     check-property-exclusiveMaximum-old
     check-property-exclusiveMinimum-new
     check-property-exclusiveMinimum-old
     check-property-extends
     check-property-id
     check-property-if
     check-property-items
     check-property-maxContains
     check-property-maxItems
     check-property-maxLength
     check-property-maxProperties
     check-property-maximum-new
     check-property-maximum-old
     check-property-minContains
     check-property-minItems
     check-property-minLength
     check-property-minProperties
     check-property-minimum-new
     check-property-minimum-old
     check-property-multipleOf
     check-property-not
     check-property-oneOf
     check-property-patternProperties
     check-property-prefixItems
     check-property-properties
     check-property-properties-draft3
     check-property-propertyDependencies
     check-property-propertyNames
     check-property-readOnly
     check-property-required
     check-property-required-draft3
     check-property-then
     check-property-title
     check-property-unevaluatedItems
     check-property-unevaluatedProperties
     check-property-uniqueItems
     check-property-writeOnly
     make-check-property-$dynamicRef
     make-check-property-$ref
     make-check-property-contentEncoding
     make-check-property-contentMediaType
     make-check-property-contentSchema
     make-check-property-disallow
     make-check-property-format
     make-check-property-pattern
     make-check-property-type]]))

;;------------------------------------------------------------------------------
;; $schema and $vocabulary property checkers live here (not in property.cljc)
;; because they need make-dialect and draft->default-dialect, and property.cljc
;; cannot require vocabulary.cljc (vocabulary requires property).
;; Forward-declared: the function bodies close over vars, resolved at call time.

(declare draft->config make-dialect draft->default-dialect)

(defn check-property-$schema [_property c2 _p2 m2 v2]
  ;; Dialect switching: parse $schema to determine draft, load metaschema
  ;; for $vocabulary, build dialect, return updated c2 for compile-m2's loop.
  (let [uri (parse-uri v2)
        draft (or ($schema-uri->draft uri)
                  (:draft c2))
        uri->schema (:uri->schema c2)
        metaschema (when uri->schema
                     (try
                       (let [[_ _ ms] (uri->schema c2 [] uri)]
                         ms)
                       (catch #?(:clj Exception :cljs js/Error) e
                         (log/info (str "Could not load metaschema: " v2 " - " #?(:clj (.getMessage e) :cljs (.-message e))))
                         nil)))
        vocab-map (get metaschema "$vocabulary")
        new-dialect (if vocab-map
                      (make-dialect draft vocab-map)
                      (draft->default-dialect draft))
        new-c2 (merge c2 (draft->config draft) {:dialect new-dialect :draft draft})]
    [new-c2
     m2
     (fn [c1 _p1 m1]
       [c1 m1 nil])]))

(defn check-property-$vocabulary [_property {d :draft :as c2} _p2 m2 v2]
  ;; $vocabulary specifies which vocabularies are active for this schema.
  ;; v2 is a map like: {"vocab-uri-1" true, "vocab-uri-2" false, ...}
  (let [new-dialect (make-dialect d v2)
        new-c2 (assoc c2 :dialect new-dialect)]
    [new-c2
     m2
     (fn [c1 _p1 m1]
       [c1 m1 nil])]))

;;------------------------------------------------------------------------------

(def draft->config
  {:draft3       {:id-key "id"}
   :draft4       {:id-key "id"}
   :draft6       {:id-key "$id"}
   :draft7       {:id-key "$id"}
   :draft2019-09 {:id-key "$id"}
   :draft2020-12 {:id-key "$id"}
   :draft-next   {:id-key "$id"}})

;; Pre-built ref configs
(def old-ref-config {:meld-fn meld-replace :ref-replaces-siblings? true :ref-scope-isolation? false})
(def new-ref-config {:meld-fn meld-deep-over :ref-replaces-siblings? false :ref-scope-isolation? true})

;; Instantiated property checkers with draft-specific config baked in
(def check-property-$ref-old (make-check-property-$ref old-ref-config))
(def check-property-$ref-new (make-check-property-$ref new-ref-config))

(def check-property-$dynamicRef-bookend (make-check-property-$dynamicRef true))
(def check-property-$dynamicRef-no-bookend (make-check-property-$dynamicRef false))

(def check-property-type-draft3 (make-check-property-type draft3-type->checker))
(def check-property-type-draft4 (make-check-property-type draft4-type->checker))

(def check-property-disallow-draft3 (make-check-property-disallow draft3-type->checker))

(def check-property-format-draft3 (make-check-property-format false draft3-format->checker))
(def check-property-format-draft4 (make-check-property-format true draft4-format->checker))
(def check-property-format-draft6 (make-check-property-format true draft6-format->checker))
(def check-property-format-draft7 (make-check-property-format true draft7-format->checker))
(def check-property-format-draft2019-09 (make-check-property-format true draft2019-09-format->checker))
(def check-property-format-draft2020-12-annotation (make-check-property-format false draft2020-12-format->checker))
(def check-property-format-draft2020-12-assertion (make-check-property-format true draft2020-12-format->checker))
(def check-property-format-draft-next-annotation (make-check-property-format false draft-next-format->checker))
(def check-property-format-draft-next-assertion (make-check-property-format true draft-next-format->checker))

(def check-property-pattern-draft3 (make-check-property-pattern draft3-format->checker))
(def check-property-pattern-draft4 (make-check-property-pattern draft4-format->checker))
(def check-property-pattern-draft6 (make-check-property-pattern draft6-format->checker))
(def check-property-pattern-draft7 (make-check-property-pattern draft7-format->checker))
(def check-property-pattern-draft2019-09 (make-check-property-pattern draft2019-09-format->checker))
(def check-property-pattern-draft2020-12 (make-check-property-pattern draft2020-12-format->checker))
(def check-property-pattern-draft-next (make-check-property-pattern draft-next-format->checker))

;;------------------------------------------------------------------------------

(defn sort-vocab [vs]
  (mapcat identity (topo-sort-by (comp (partial mapcat fourth) second) second (group-by second vs))))

(def draft->vocab
  {:draft3
   (sort-vocab
    [["https://json-schema.org/draft-03/vocab/applicator"             "additionalItems"        check-property-additionalItems                           #{"$schema" "items"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "additionalProperties"   check-property-additionalProperties                      #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "allOf"                  check-property-allOf                                     #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "anyOf"                  check-property-anyOf                                     #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "dependencies"           check-property-dependencies                              #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "extends"                check-property-extends                                   #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "items"                  check-property-items                                     #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "not"                    check-property-not                                       #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "oneOf"                  check-property-oneOf                                     #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "patternProperties"      check-property-patternProperties                         #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "properties"             check-property-properties-draft3                         #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "propertyDependencies"   check-property-propertyDependencies                      #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/core"                   "$ref"                   check-property-$ref-old                                  #{"$schema" "id" "definitions"}]
     ["https://json-schema.org/draft-03/vocab/core"                   "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft-03/vocab/core"                   "definitions"            check-property-definitions                               #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/core"                   "id"                     check-property-id                                        #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/meta-data"              "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/meta-data"              "deprecated"             check-property-deprecated                                #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/meta-data"              "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/meta-data"              "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "disallow"               check-property-disallow-draft3                           #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "divisibleBy"            check-property-divisibleBy                               #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "exclusiveMaximum"       check-property-exclusiveMaximum-old                      #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "exclusiveMinimum"       check-property-exclusiveMinimum-old                      #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "format"                 check-property-format-draft3                             #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "maxItems"               check-property-maxItems                                  #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "maxLength"              check-property-maxLength                                 #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "maxProperties"          check-property-maxProperties                             #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "maximum"                check-property-maximum-old                               #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "minItems"               check-property-minItems                                  #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "minLength"              check-property-minLength                                 #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "minProperties"          check-property-minProperties                             #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "minimum"                check-property-minimum-old                               #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "multipleOf"             check-property-multipleOf                                #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "pattern"                check-property-pattern-draft3                            #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "required"               check-property-required-draft3                           #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "type"                   check-property-type-draft3                               #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "uniqueItems"            check-property-uniqueItems                               #{"$schema"}]])
   :draft4
   (sort-vocab
    [["https://json-schema.org/draft-04/vocab/applicator"             "additionalItems"        check-property-additionalItems                           #{"$schema" "items"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "additionalProperties"   check-property-additionalProperties                      #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "allOf"                  check-property-allOf                                     #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "anyOf"                  check-property-anyOf                                     #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "dependencies"           check-property-dependencies                              #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "items"                  check-property-items                                     #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "not"                    check-property-not                                       #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "oneOf"                  check-property-oneOf                                     #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "patternProperties"      check-property-patternProperties                         #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "properties"             check-property-properties                                #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "propertyDependencies"   check-property-propertyDependencies                      #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/core"                   "$ref"                   check-property-$ref-old                                  #{"$schema" "id" "definitions"}]
     ["https://json-schema.org/draft-04/vocab/core"                   "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft-04/vocab/core"                   "definitions"            check-property-definitions                               #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/core"                   "id"                     check-property-id                                        #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/meta-data"              "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/meta-data"              "deprecated"             check-property-deprecated                                #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/meta-data"              "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/meta-data"              "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "exclusiveMaximum"       check-property-exclusiveMaximum-old                      #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "exclusiveMinimum"       check-property-exclusiveMinimum-old                      #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "format"                 check-property-format-draft4                             #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "maxItems"               check-property-maxItems                                  #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "maxLength"              check-property-maxLength                                 #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "maxProperties"          check-property-maxProperties                             #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "maximum"                check-property-maximum-old                               #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "minItems"               check-property-minItems                                  #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "minLength"              check-property-minLength                                 #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "minProperties"          check-property-minProperties                             #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "minimum"                check-property-minimum-old                               #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "multipleOf"             check-property-multipleOf                                #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "pattern"                check-property-pattern-draft4                            #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "required"               check-property-required                                  #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "uniqueItems"            check-property-uniqueItems                               #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "type"                   check-property-type-draft4                               #{"$schema"}]])
   :draft6
   (sort-vocab
    [["https://json-schema.org/draft-06/vocab/applicator"             "additionalItems"        check-property-additionalItems                           #{"$schema" "items"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "additionalProperties"   check-property-additionalProperties                      #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "allOf"                  check-property-allOf                                     #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "anyOf"                  check-property-anyOf                                     #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "contains"               check-property-contains                                  #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "dependencies"           check-property-dependencies                              #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "items"                  check-property-items                                     #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "not"                    check-property-not                                       #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "oneOf"                  check-property-oneOf                                     #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "patternProperties"      check-property-patternProperties                         #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "properties"             check-property-properties                                #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "propertyDependencies"   check-property-propertyDependencies                      #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "propertyNames"          check-property-propertyNames                             #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/core"                   "$id"                    check-property-$id                                       #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/core"                   "$ref"                   check-property-$ref-old                                  #{"$schema" "id" "$id" "definitions"}]
     ["https://json-schema.org/draft-06/vocab/core"                   "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft-06/vocab/core"                   "definitions"            check-property-definitions                               #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/core"                   "id"                     check-property-id                                        #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/meta-data"              "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/meta-data"              "deprecated"             check-property-deprecated                                #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/meta-data"              "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/meta-data"              "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "const"                  check-property-const                                     #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "exclusiveMaximum"       check-property-exclusiveMaximum-new                      #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "exclusiveMinimum"       check-property-exclusiveMinimum-new                      #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "format"                 check-property-format-draft6                             #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maxContains"            check-property-maxContains                               #{"$schema" "contains"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maxItems"               check-property-maxItems                                  #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maxLength"              check-property-maxLength                                 #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maxProperties"          check-property-maxProperties                             #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maximum"                check-property-maximum-new                               #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minContains"            check-property-minContains                               #{"$schema" "contains"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minItems"               check-property-minItems                                  #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minLength"              check-property-minLength                                 #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minProperties"          check-property-minProperties                             #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minimum"                check-property-minimum-new                               #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "multipleOf"             check-property-multipleOf                                #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "pattern"                check-property-pattern-draft6                            #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "required"               check-property-required                                  #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "uniqueItems"            check-property-uniqueItems                               #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "type"                   check-property-type-draft4                               #{"$schema"}]])
   :draft7
   (sort-vocab
    [["https://json-schema.org/draft-07/vocab/applicator"             "additionalItems"        check-property-additionalItems                           #{"$schema" "items"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "additionalProperties"   check-property-additionalProperties                      #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "allOf"                  check-property-allOf                                     #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "anyOf"                  check-property-anyOf                                     #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "contains"               check-property-contains                                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "dependencies"           check-property-dependencies                              #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "else"                   check-property-else                                      #{"$schema" "if"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "if"                     check-property-if                                        #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "items"                  check-property-items                                     #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "not"                    check-property-not                                       #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "oneOf"                  check-property-oneOf                                     #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "patternProperties"      check-property-patternProperties                         #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "properties"             check-property-properties                                #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "propertyDependencies"   check-property-propertyDependencies                      #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "propertyNames"          check-property-propertyNames                             #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "then"                   check-property-then                                      #{"$schema" "if"}]
     ["https://json-schema.org/draft-07/vocab/content"                "contentEncoding"        (make-check-property-contentEncoding true)               #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/content"                "contentMediaType"       (make-check-property-contentMediaType true)              #{"$schema" "contentEncoding"}]
     ["https://json-schema.org/draft-07/vocab/content"                "contentSchema"          (make-check-property-contentSchema true)                 #{"$schema" "contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "$comment"               check-property-$comment                                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "$id"                    check-property-$id                                       #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "$ref"                   check-property-$ref-old                                  #{"$schema" "id" "$id" "definitions"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "definitions"            check-property-definitions                               #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "id"                     check-property-id                                        #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "deprecated"             check-property-deprecated                                #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "examples"               check-property-examples                                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "readOnly"               check-property-readOnly                                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "writeOnly"              check-property-writeOnly                                 #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "const"                  check-property-const                                     #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "exclusiveMaximum"       check-property-exclusiveMaximum-new                      #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "exclusiveMinimum"       check-property-exclusiveMinimum-new                      #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "format"                 check-property-format-draft7                             #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maxContains"            check-property-maxContains                               #{"$schema" "contains"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maxItems"               check-property-maxItems                                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maxLength"              check-property-maxLength                                 #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maxProperties"          check-property-maxProperties                             #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maximum"                check-property-maximum-new                               #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minContains"            check-property-minContains                               #{"$schema" "contains"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minItems"               check-property-minItems                                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minLength"              check-property-minLength                                 #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minProperties"          check-property-minProperties                             #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minimum"                check-property-minimum-new                               #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "multipleOf"             check-property-multipleOf                                #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "pattern"                check-property-pattern-draft7                            #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "required"               check-property-required                                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "uniqueItems"            check-property-uniqueItems                               #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "type"                   check-property-type-draft4                               #{"$schema"}]])
   :draft2019-09
   (sort-vocab
    [["https://json-schema.org/draft/2019-09/vocab/applicator"        "additionalItems"        check-property-additionalItems                           #{"$schema" "items"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "additionalProperties"   check-property-additionalProperties                      #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "allOf"                  check-property-allOf                                     #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "anyOf"                  check-property-anyOf                                     #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "contains"               check-property-contains                                  #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "dependencies"           check-property-dependencies                              #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "dependentSchemas"       check-property-dependentSchemas                          #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "else"                   check-property-else                                      #{"$schema" "if"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "if"                     check-property-if                                        #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "items"                  check-property-items                                     #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "not"                    check-property-not                                       #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "oneOf"                  check-property-oneOf                                     #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "patternProperties"      check-property-patternProperties                         #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "properties"             check-property-properties                                #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "propertyDependencies"   check-property-propertyDependencies                      #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "propertyNames"          check-property-propertyNames                             #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "then"                   check-property-then                                      #{"$schema" "if"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "unevaluatedItems"       check-property-unevaluatedItems                          #{"$schema" "additionalItems" "uniqueItems" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "unevaluatedProperties"  check-property-unevaluatedProperties                     #{"$schema" "additionalProperties" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2019-09/vocab/content"           "contentEncoding"        (make-check-property-contentEncoding false)              #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/content"           "contentMediaType"       (make-check-property-contentMediaType false)             #{"$schema" "contentEncoding"}]
     ["https://json-schema.org/draft/2019-09/vocab/content"           "contentSchema"          (make-check-property-contentSchema false)                #{"$schema" "contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$anchor"                check-property-$anchor                                   #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$comment"               check-property-$comment                                  #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$defs"                  check-property-$defs                                     #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$id"                    check-property-$id                                       #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$recursiveAnchor"       check-property-$recursiveAnchor                          #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$recursiveRef"          check-property-$recursiveRef                             #{"$schema" "$recursiveAnchor"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$ref"                   check-property-$ref-new                                  #{"$schema" "id" "$id" "$anchor" "$defs" "$recursiveAnchor" "$dynamicAnchor"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$vocabulary"            check-property-$vocabulary                               #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "deprecated"             check-property-deprecated                                #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "examples"               check-property-examples                                  #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "readOnly"               check-property-readOnly                                  #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "writeOnly"              check-property-writeOnly                                 #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "const"                  check-property-const                                     #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "dependentRequired"      check-property-dependentRequired                         #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "exclusiveMaximum"       check-property-exclusiveMaximum-new                      #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "exclusiveMinimum"       check-property-exclusiveMinimum-new                      #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "format"                 check-property-format-draft2019-09                       #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maxContains"            check-property-maxContains                               #{"$schema" "contains"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maxItems"               check-property-maxItems                                  #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maxLength"              check-property-maxLength                                 #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maxProperties"          check-property-maxProperties                             #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maximum"                check-property-maximum-new                               #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minContains"            check-property-minContains                               #{"$schema" "contains"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minItems"               check-property-minItems                                  #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minLength"              check-property-minLength                                 #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minProperties"          check-property-minProperties                             #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minimum"                check-property-minimum-new                               #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "multipleOf"             check-property-multipleOf                                #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "pattern"                check-property-pattern-draft2019-09                      #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "required"               check-property-required                                  #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "uniqueItems"            check-property-uniqueItems                               #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "type"                   check-property-type-draft4                               #{"$schema"}]])
   :draft2020-12
   (sort-vocab
    [["https://json-schema.org/draft/2020-12/vocab/applicator"        "additionalItems"        check-property-additionalItems                           #{"$schema" "items"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "additionalProperties"   check-property-additionalProperties                      #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "allOf"                  check-property-allOf                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "anyOf"                  check-property-anyOf                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "contains"               check-property-contains                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "dependencies"           check-property-dependencies                              #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "dependentSchemas"       check-property-dependentSchemas                          #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "else"                   check-property-else                                      #{"$schema" "if"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "if"                     check-property-if                                        #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "items"                  check-property-items                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "not"                    check-property-not                                       #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "oneOf"                  check-property-oneOf                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "patternProperties"      check-property-patternProperties                         #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "prefixItems"            check-property-prefixItems                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "properties"             check-property-properties                                #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "propertyDependencies"   check-property-propertyDependencies                      #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "propertyNames"          check-property-propertyNames                             #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "then"                   check-property-then                                      #{"$schema" "if"}]
     ["https://json-schema.org/draft/2020-12/vocab/content"           "contentEncoding"        (make-check-property-contentEncoding false)              #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/content"           "contentMediaType"       (make-check-property-contentMediaType false)             #{"$schema" "contentEncoding"}]
     ["https://json-schema.org/draft/2020-12/vocab/content"           "contentSchema"          (make-check-property-contentSchema false)                #{"$schema" "contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$anchor"                check-property-$anchor                                   #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$comment"               check-property-$comment                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$defs"                  check-property-$defs                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$dynamicAnchor"         check-property-$dynamicAnchor                            #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$dynamicRef"            check-property-$dynamicRef-bookend                       #{"$schema" "$dynamicAnchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$id"                    check-property-$id                                       #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$recursiveAnchor"       check-property-$recursiveAnchor                          #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$recursiveRef"          check-property-$recursiveRef                             #{"$schema" "$recursiveAnchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$ref"                   check-property-$ref-new                                  #{"$schema" "id" "$id" "$anchor" "$defs" "$recursiveAnchor" "$dynamicAnchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$vocabulary"            check-property-$vocabulary                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/format-annotation" "format"                 check-property-format-draft2020-12-annotation            #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/format-assertion"  "format"                 check-property-format-draft2020-12-assertion             #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "deprecated"             check-property-deprecated                                #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "examples"               check-property-examples                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "readOnly"               check-property-readOnly                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "writeOnly"              check-property-writeOnly                                 #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/unevaluated"       "unevaluatedItems"       check-property-unevaluatedItems                          #{"$schema" "additionalItems" "uniqueItems" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2020-12/vocab/unevaluated"       "unevaluatedProperties"  check-property-unevaluatedProperties                     #{"$schema" "additionalProperties" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "const"                  check-property-const                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "dependentRequired"      check-property-dependentRequired                         #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "exclusiveMaximum"       check-property-exclusiveMaximum-new                      #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "exclusiveMinimum"       check-property-exclusiveMinimum-new                      #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxContains"            check-property-maxContains                               #{"$schema" "contains"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxItems"               check-property-maxItems                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxLength"              check-property-maxLength                                 #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxProperties"          check-property-maxProperties                             #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maximum"                check-property-maximum-new                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minContains"            check-property-minContains                               #{"$schema" "contains"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minItems"               check-property-minItems                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minLength"              check-property-minLength                                 #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minProperties"          check-property-minProperties                             #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minimum"                check-property-minimum-new                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "multipleOf"             check-property-multipleOf                                #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "pattern"                check-property-pattern-draft2020-12                      #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "required"               check-property-required                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "uniqueItems"            check-property-uniqueItems                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "type"                   check-property-type-draft4                               #{"$schema"}]])
   ;; draft-next reuses draft2020-12 vocabulary URIs because the draft-next
   ;; metaschema's $vocabulary declares 2020-12 URIs (the spec isn't finalized).
   ;; Where draft-next behavior differs (e.g. $dynamicRef bookending), the
   ;; checker instances are draft-next-specific.
   :draft-next
   (sort-vocab
    [["https://json-schema.org/draft/2020-12/vocab/applicator"        "additionalItems"        check-property-additionalItems                           #{"$schema" "items"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "additionalProperties"   check-property-additionalProperties                      #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "allOf"                  check-property-allOf                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "anyOf"                  check-property-anyOf                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "contains"               check-property-contains                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "dependencies"           check-property-dependencies                              #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "dependentSchemas"       check-property-dependentSchemas                          #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "else"                   check-property-else                                      #{"$schema" "if"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "if"                     check-property-if                                        #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "items"                  check-property-items                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "not"                    check-property-not                                       #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "oneOf"                  check-property-oneOf                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "patternProperties"      check-property-patternProperties                         #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "prefixItems"            check-property-prefixItems                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "properties"             check-property-properties                                #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "propertyDependencies"   check-property-propertyDependencies                      #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "propertyNames"          check-property-propertyNames                             #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "then"                   check-property-then                                      #{"$schema" "if"}]
     ["https://json-schema.org/draft/2020-12/vocab/content"           "contentEncoding"        (make-check-property-contentEncoding false)              #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/content"           "contentMediaType"       (make-check-property-contentMediaType false)             #{"$schema" "contentEncoding"}]
     ["https://json-schema.org/draft/2020-12/vocab/content"           "contentSchema"          (make-check-property-contentSchema false)                #{"$schema" "contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$anchor"                check-property-$anchor                                   #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$comment"               check-property-$comment                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$defs"                  check-property-$defs                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$dynamicAnchor"         check-property-$dynamicAnchor                            #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$dynamicRef"            check-property-$dynamicRef-no-bookend                    #{"$schema" "$dynamicAnchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$id"                    check-property-$id                                       #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$recursiveAnchor"       check-property-$recursiveAnchor                          #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$recursiveRef"          check-property-$recursiveRef                             #{"$schema" "$recursiveAnchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$ref"                   check-property-$ref-new                                  #{"$schema" "id" "$id" "$anchor" "$defs" "$recursiveAnchor" "$dynamicAnchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$vocabulary"            check-property-$vocabulary                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/format-annotation" "format"                 check-property-format-draft-next-annotation              #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/format-assertion"  "format"                 check-property-format-draft-next-assertion               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "deprecated"             check-property-deprecated                                #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "examples"               check-property-examples                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "readOnly"               check-property-readOnly                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "writeOnly"              check-property-writeOnly                                 #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/unevaluated"       "unevaluatedItems"       check-property-unevaluatedItems                          #{"$schema" "additionalItems" "uniqueItems" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2020-12/vocab/unevaluated"       "unevaluatedProperties"  check-property-unevaluatedProperties                     #{"$schema" "additionalProperties" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "const"                  check-property-const                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "dependentRequired"      check-property-dependentRequired                         #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "exclusiveMaximum"       check-property-exclusiveMaximum-new                      #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "exclusiveMinimum"       check-property-exclusiveMinimum-new                      #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxContains"            check-property-maxContains                               #{"$schema" "contains"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxItems"               check-property-maxItems                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxLength"              check-property-maxLength                                 #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxProperties"          check-property-maxProperties                             #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maximum"                check-property-maximum-new                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minContains"            check-property-minContains                               #{"$schema" "contains"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minItems"               check-property-minItems                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minLength"              check-property-minLength                                 #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minProperties"          check-property-minProperties                             #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minimum"                check-property-minimum-new                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "multipleOf"             check-property-multipleOf                                #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "pattern"                check-property-pattern-draft-next                        #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "required"               check-property-required                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "uniqueItems"            check-property-uniqueItems                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "type"                   check-property-type-draft4                               #{"$schema"}]])})

;; lets define a dialect as a function that given an m2 will return
;; you a correctly ordered sequence of pairs of m2-kv and
;; property-checker

;; should really convert strings to uris...
;; draft-next vocab URI aliasing: the draft-next metaschema's $vocabulary
;; declares 2020-12 URIs, but the JSON-Schema-Test-Suite's custom draft-next
;; metaschemas declare draft/next/ URIs. Both must work. Our draft-next vocab
;; table uses 2020-12 URIs (matching the real metaschema), so we normalize any
;; draft/next/ URIs in v->b to 2020-12 before filtering.
(defn- normalize-draft-next-vocab-uris [v->b]
  (into {}
    (map (fn [[uri required?]]
           [(clojure.string/replace uri "draft/next/vocab/" "draft/2020-12/vocab/")
            required?]))
    v->b))

(defn make-dialect-2 [d v->b]
  (let [effective-v->b (if (= d :draft-next) (normalize-draft-next-vocab-uris v->b) v->b)
        uri-set (into #{} (keys effective-v->b))
        entries (filter (comp uri-set first) (draft->vocab d))]
    (partial
     (make-stable-sort-by second entries)
     first
     (juxt first (comp third second)))))

(def make-dialect (memoize make-dialect-2))

(def draft->default-dialect
  (map-values
   (fn [k v]
     (make-dialect k (into {} (map (fn [v] [v true])) (distinct (map first v)))))
   draft->vocab))


