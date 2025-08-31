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
   [m3.util :refer [map-values topo-sort-by make-stable-sort-by third fourth concatv]]
   [m3.property :refer
    [check-property-$anchor
     check-property-$comment
     check-property-$defs
     check-property-$dynamicAnchor
     check-property-$dynamicRef
     check-property-$id
     check-property-$recursiveAnchor
     check-property-$recursiveRef
     check-property-$ref
     check-property-$schema
     check-property-$vocabulary
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
     check-property-disallow
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
     check-property-pattern
     check-property-patternProperties
     check-property-prefixItems
     check-property-properties
     check-property-propertyDependencies
     check-property-propertyNames
     check-property-readOnly
     check-property-required
     check-property-then
     check-property-title
     check-property-type
     check-property-unevaluatedItems
     check-property-unevaluatedProperties
     check-property-uniqueItems
     check-property-writeOnly
     make-check-property-contentEncoding
     make-check-property-contentMediaType
     make-check-property-contentSchema
     make-check-property-format]]))

;;------------------------------------------------------------------------------
;; to adapt a c2/p2/m2->c1/p1/m1 (old) l2 fn into new composable shape l2 (new format)
;; also does l2 path management

(defn old->new [old-f2]
  (fn [property c2 p2 m2 v2]
    (let [old-f1 (old-f2 property c2 p2 m2 v2)]
      [c2
       m2
       ;; (fn [c1 p1 m1]
       ;;   (let [[c1 es] (old-f1 c1 p1 m1)]
       ;;     [c1 m1 es]))
       old-f1
       ])))

;; we should have enough now to adapt all vocab keyword fns to new
;; format, compose them into a single new format function and then
;; adapt the back to an old format function to plug into m3...
;;------------------------------------------------------------------------------

(defn sort-vocabulary [vs]
  (mapcat identity (topo-sort-by (comp (partial mapcat fourth) second) second (group-by second vs))))

(def draft->vocab-and-group-and-property-and-semantics
  {:draft3
   (sort-vocabulary
    [["https://json-schema.org/draft-03/vocab/applicator"             "additionalItems"        check-property-additionalItems                #{"items"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "additionalProperties"   check-property-additionalProperties           #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "allOf"                  check-property-allOf                          #{}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "anyOf"                  check-property-anyOf                          #{}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "dependencies"           check-property-dependencies                   #{}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "extends"                check-property-extends                        #{}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "items"                  check-property-items                          #{}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "not"                    check-property-not                            #{}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "oneOf"                  check-property-oneOf                          #{}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "patternProperties"      check-property-patternProperties              #{}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "properties"             check-property-properties                     #{}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "propertyDependencies"   check-property-propertyDependencies           #{}]
     ["https://json-schema.org/draft-03/vocab/core"                   "$ref"                   check-property-$ref                           #{"id"}]
     ["https://json-schema.org/draft-03/vocab/core"                   "$schema"                check-property-$schema                        #{}]
     ["https://json-schema.org/draft-03/vocab/core"                   "definitions"            check-property-definitions                    #{}]
     ["https://json-schema.org/draft-03/vocab/core"                   "id"                     check-property-id                             #{}]
     ["https://json-schema.org/draft-03/vocab/meta-data"              "default"                check-property-default                        #{}]
     ["https://json-schema.org/draft-03/vocab/meta-data"              "deprecated"             check-property-deprecated                     #{}]
     ["https://json-schema.org/draft-03/vocab/meta-data"              "description"            check-property-description                    #{}]
     ["https://json-schema.org/draft-03/vocab/meta-data"              "title"                  check-property-title                          #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "disallow"               check-property-disallow                       #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "divisibleBy"            check-property-divisibleBy                    #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "enum"                   check-property-enum                           #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "exclusiveMaximum"       check-property-exclusiveMaximum-old           #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "exclusiveMinimum"       check-property-exclusiveMinimum-old           #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "format"                 (make-check-property-format false)            #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "maxItems"               check-property-maxItems                       #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "maxLength"              check-property-maxLength                      #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "maxProperties"          check-property-maxProperties                  #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "maximum"                check-property-maximum-old                    #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "minItems"               check-property-minItems                       #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "minLength"              check-property-minLength                      #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "minProperties"          check-property-minProperties                  #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "minimum"                check-property-minimum-old                    #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "multipleOf"             check-property-multipleOf                     #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "pattern"                check-property-pattern                        #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "required"               check-property-required                       #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "type"                   check-property-type                           #{}]
     ["https://json-schema.org/draft-03/vocab/validation"             "uniqueItems"            check-property-uniqueItems                    #{}]])
   :draft4
   (sort-vocabulary
    [["https://json-schema.org/draft-04/vocab/applicator"             "additionalItems"        check-property-additionalItems                #{"items"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "additionalProperties"   check-property-additionalProperties           #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "allOf"                  check-property-allOf                          #{}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "anyOf"                  check-property-anyOf                          #{}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "dependencies"           check-property-dependencies                   #{}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "items"                  check-property-items                          #{}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "not"                    check-property-not                            #{}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "oneOf"                  check-property-oneOf                          #{}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "patternProperties"      check-property-patternProperties              #{}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "properties"             check-property-properties                     #{}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "propertyDependencies"   check-property-propertyDependencies           #{}]
     ["https://json-schema.org/draft-04/vocab/core"                   "$ref"                   check-property-$ref                           #{"id"}]
     ["https://json-schema.org/draft-04/vocab/core"                   "$schema"                check-property-$schema                        #{}]
     ["https://json-schema.org/draft-04/vocab/core"                   "definitions"            check-property-definitions                    #{}]
     ["https://json-schema.org/draft-04/vocab/core"                   "id"                     check-property-id                             #{}]
     ["https://json-schema.org/draft-04/vocab/meta-data"              "default"                check-property-default                        #{}]
     ["https://json-schema.org/draft-04/vocab/meta-data"              "deprecated"             check-property-deprecated                     #{}]
     ["https://json-schema.org/draft-04/vocab/meta-data"              "description"            check-property-description                    #{}]
     ["https://json-schema.org/draft-04/vocab/meta-data"              "title"                  check-property-title                          #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "enum"                   check-property-enum                           #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "exclusiveMaximum"       check-property-exclusiveMaximum-old           #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "exclusiveMinimum"       check-property-exclusiveMinimum-old           #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "format"                 (make-check-property-format true)             #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "maxItems"               check-property-maxItems                       #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "maxLength"              check-property-maxLength                      #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "maxProperties"          check-property-maxProperties                  #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "maximum"                check-property-maximum-old                    #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "minItems"               check-property-minItems                       #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "minLength"              check-property-minLength                      #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "minProperties"          check-property-minProperties                  #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "minimum"                check-property-minimum-old                    #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "multipleOf"             check-property-multipleOf                     #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "pattern"                check-property-pattern                        #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "required"               check-property-required                       #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "uniqueItems"            check-property-uniqueItems                    #{}]
     ["https://json-schema.org/draft-04/vocab/validation"             "type"                   check-property-type                           #{}]])
   :draft6
   (sort-vocabulary
    [["https://json-schema.org/draft-06/vocab/applicator"             "additionalItems"        check-property-additionalItems                #{"items"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "additionalProperties"   check-property-additionalProperties           #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "allOf"                  check-property-allOf                          #{}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "anyOf"                  check-property-anyOf                          #{}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "contains"               check-property-contains                       #{}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "dependencies"           check-property-dependencies                   #{}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "items"                  check-property-items                          #{}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "not"                    check-property-not                            #{}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "oneOf"                  check-property-oneOf                          #{}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "patternProperties"      check-property-patternProperties              #{}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "properties"             check-property-properties                     #{}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "propertyDependencies"   check-property-propertyDependencies           #{}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "propertyNames"          check-property-propertyNames                  #{}]
     ["https://json-schema.org/draft-06/vocab/core"                   "$id"                    check-property-$id                            #{}]
     ["https://json-schema.org/draft-06/vocab/core"                   "$ref"                   check-property-$ref                           #{"id" "$id"}]
     ["https://json-schema.org/draft-06/vocab/core"                   "$schema"                check-property-$schema                        #{}]
     ["https://json-schema.org/draft-06/vocab/core"                   "definitions"            check-property-definitions                    #{}]
     ["https://json-schema.org/draft-06/vocab/core"                   "id"                     check-property-id                             #{}]
     ["https://json-schema.org/draft-06/vocab/meta-data"              "default"                check-property-default                        #{}]
     ["https://json-schema.org/draft-06/vocab/meta-data"              "deprecated"             check-property-deprecated                     #{}]
     ["https://json-schema.org/draft-06/vocab/meta-data"              "description"            check-property-description                    #{}]
     ["https://json-schema.org/draft-06/vocab/meta-data"              "title"                  check-property-title                          #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "const"                  check-property-const                          #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "enum"                   check-property-enum                           #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "exclusiveMaximum"       check-property-exclusiveMaximum-new           #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "exclusiveMinimum"       check-property-exclusiveMinimum-new           #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "format"                 (make-check-property-format true)             #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maxContains"            check-property-maxContains                    #{"contains"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maxItems"               check-property-maxItems                       #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maxLength"              check-property-maxLength                      #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maxProperties"          check-property-maxProperties                  #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maximum"                check-property-maximum-new                    #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minContains"            check-property-minContains                    #{"contains"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minItems"               check-property-minItems                       #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minLength"              check-property-minLength                      #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minProperties"          check-property-minProperties                  #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minimum"                check-property-minimum-new                    #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "multipleOf"             check-property-multipleOf                     #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "pattern"                check-property-pattern                        #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "required"               check-property-required                       #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "uniqueItems"            check-property-uniqueItems                    #{}]
     ["https://json-schema.org/draft-06/vocab/validation"             "type"                   check-property-type                           #{}]])
   :draft7
   (sort-vocabulary
    [["https://json-schema.org/draft-07/vocab/applicator"             "additionalItems"        check-property-additionalItems                #{"items"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "additionalProperties"   check-property-additionalProperties           #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "allOf"                  check-property-allOf                          #{}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "anyOf"                  check-property-anyOf                          #{}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "contains"               check-property-contains                       #{}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "dependencies"           check-property-dependencies                   #{}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "else"                   check-property-else                           #{"if"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "if"                     check-property-if                             #{}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "items"                  check-property-items                          #{}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "not"                    check-property-not                            #{}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "oneOf"                  check-property-oneOf                          #{}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "patternProperties"      check-property-patternProperties              #{}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "properties"             check-property-properties                     #{}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "propertyDependencies"   check-property-propertyDependencies           #{}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "propertyNames"          check-property-propertyNames                  #{}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "then"                   check-property-then                           #{"if"}]
     ["https://json-schema.org/draft-07/vocab/content"                "contentEncoding"        (make-check-property-contentEncoding true)    #{}]
     ["https://json-schema.org/draft-07/vocab/content"                "contentMediaType"       (make-check-property-contentMediaType true)   #{"contentEncoding"}]
     ["https://json-schema.org/draft-07/vocab/content"                "contentSchema"          (make-check-property-contentSchema true)      #{"contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "$comment"               check-property-$comment                       #{}]
     ["https://json-schema.org/draft-07/vocab/core"                   "$id"                    check-property-$id                            #{}]
     ["https://json-schema.org/draft-07/vocab/core"                   "$ref"                   check-property-$ref                           #{"id" "$id"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "$schema"                check-property-$schema                        #{}]
     ["https://json-schema.org/draft-07/vocab/core"                   "definitions"            check-property-definitions                    #{}]
     ["https://json-schema.org/draft-07/vocab/core"                   "id"                     check-property-id                             #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "default"                check-property-default                        #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "deprecated"             check-property-deprecated                     #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "description"            check-property-description                    #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "examples"               check-property-examples                       #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "readOnly"               check-property-readOnly                       #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "title"                  check-property-title                          #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "writeOnly"              check-property-writeOnly                      #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "const"                  check-property-const                          #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "enum"                   check-property-enum                           #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "exclusiveMaximum"       check-property-exclusiveMaximum-new           #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "exclusiveMinimum"       check-property-exclusiveMinimum-new           #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "format"                 (make-check-property-format true)             #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maxContains"            check-property-maxContains                    #{"contains"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maxItems"               check-property-maxItems                       #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maxLength"              check-property-maxLength                      #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maxProperties"          check-property-maxProperties                  #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maximum"                check-property-maximum-new                    #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minContains"            check-property-minContains                    #{"contains"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minItems"               check-property-minItems                       #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minLength"              check-property-minLength                      #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minProperties"          check-property-minProperties                  #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minimum"                check-property-minimum-new                    #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "multipleOf"             check-property-multipleOf                     #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "pattern"                check-property-pattern                        #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "required"               check-property-required                       #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "uniqueItems"            check-property-uniqueItems                    #{}]
     ["https://json-schema.org/draft-07/vocab/validation"             "type"                   check-property-type                           #{}]])
   :draft2019-09
   (sort-vocabulary
    [["https://json-schema.org/draft/2019-09/vocab/applicator"        "additionalItems"        check-property-additionalItems                #{"items"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "additionalProperties"   check-property-additionalProperties           #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "allOf"                  check-property-allOf                          #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "anyOf"                  check-property-anyOf                          #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "contains"               check-property-contains                       #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "dependencies"           check-property-dependencies                   #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "dependentSchemas"       check-property-dependentSchemas               #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "else"                   check-property-else                           #{"if"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "if"                     check-property-if                             #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "items"                  check-property-items                          #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "not"                    check-property-not                            #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "oneOf"                  check-property-oneOf                          #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "patternProperties"      check-property-patternProperties              #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "properties"             check-property-properties                     #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "propertyDependencies"   check-property-propertyDependencies           #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "propertyNames"          check-property-propertyNames                  #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "then"                   check-property-then                           #{"if"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "unevaluatedItems"       check-property-unevaluatedItems               #{"additionalItems" "uniqueItems" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "unevaluatedProperties"  check-property-unevaluatedProperties          #{"additionalProperties" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2019-09/vocab/content"           "contentEncoding"        (make-check-property-contentEncoding false)   #{}]
     ["https://json-schema.org/draft/2019-09/vocab/content"           "contentMediaType"       (make-check-property-contentMediaType false)  #{"contentEncoding"}]
     ["https://json-schema.org/draft/2019-09/vocab/content"           "contentSchema"          (make-check-property-contentSchema false)     #{"contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$anchor"                check-property-$anchor                        #{"id" "$id"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$comment"               check-property-$comment                       #{}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$defs"                  check-property-$defs                          #{}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$id"                    check-property-$id                            #{}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$recursiveAnchor"       check-property-$recursiveAnchor               #{"id" "$id"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$recursiveRef"          check-property-$recursiveRef                  #{"$recursiveAnchor"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$ref"                   check-property-$ref                           #{"id" "$id" "$anchor"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$schema"                check-property-$schema                        #{}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$vocabulary"            check-property-$vocabulary                    #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "default"                check-property-default                        #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "deprecated"             check-property-deprecated                     #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "description"            check-property-description                    #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "examples"               check-property-examples                       #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "readOnly"               check-property-readOnly                       #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "title"                  check-property-title                          #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data"         "writeOnly"              check-property-writeOnly                      #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "const"                  check-property-const                          #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "dependentRequired"      check-property-dependentRequired              #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "enum"                   check-property-enum                           #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "exclusiveMaximum"       check-property-exclusiveMaximum-new           #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "exclusiveMinimum"       check-property-exclusiveMinimum-new           #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "format"                 (make-check-property-format true)             #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maxContains"            check-property-maxContains                    #{"contains"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maxItems"               check-property-maxItems                       #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maxLength"              check-property-maxLength                      #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maxProperties"          check-property-maxProperties                  #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maximum"                check-property-maximum-new                    #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minContains"            check-property-minContains                    #{"contains"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minItems"               check-property-minItems                       #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minLength"              check-property-minLength                      #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minProperties"          check-property-minProperties                  #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minimum"                check-property-minimum-new                    #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "multipleOf"             check-property-multipleOf                     #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "pattern"                check-property-pattern                        #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "required"               check-property-required                       #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "uniqueItems"            check-property-uniqueItems                    #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "type"                   check-property-type                           #{}]])
   :draft2020-12
   (sort-vocabulary
    [["https://json-schema.org/draft/2020-12/vocab/applicator"        "additionalItems"        check-property-additionalItems                #{"items"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "additionalProperties"   check-property-additionalProperties           #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "allOf"                  check-property-allOf                          #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "anyOf"                  check-property-anyOf                          #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "contains"               check-property-contains                       #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "dependencies"           check-property-dependencies                   #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "dependentSchemas"       check-property-dependentSchemas               #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "else"                   check-property-else                           #{"if"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "if"                     check-property-if                             #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "items"                  check-property-items                          #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "not"                    check-property-not                            #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "oneOf"                  check-property-oneOf                          #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "patternProperties"      check-property-patternProperties              #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "prefixItems"            check-property-prefixItems                    #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "properties"             check-property-properties                     #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "propertyDependencies"   check-property-propertyDependencies           #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "propertyNames"          check-property-propertyNames                  #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "then"                   check-property-then                           #{"if"}]
     ["https://json-schema.org/draft/2020-12/vocab/content"           "contentEncoding"        (make-check-property-contentEncoding false)   #{}]
     ["https://json-schema.org/draft/2020-12/vocab/content"           "contentMediaType"       (make-check-property-contentMediaType false)  #{"contentEncoding"}]
     ["https://json-schema.org/draft/2020-12/vocab/content"           "contentSchema"          (make-check-property-contentSchema false)     #{"contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$anchor"                check-property-$anchor                        #{"id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$comment"               check-property-$comment                       #{}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$defs"                  check-property-$defs                          #{}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$dynamicAnchor"         check-property-$dynamicAnchor                 #{"id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$dynamicRef"            check-property-$dynamicRef                    #{"$dynamicAnchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$id"                    check-property-$id                            #{}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$recursiveAnchor"       check-property-$recursiveAnchor               #{"id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$recursiveRef"          check-property-$recursiveRef                  #{"$recursiveAnchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$ref"                   check-property-$ref                           #{"id" "$id" "$anchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$schema"                check-property-$schema                        #{}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$vocabulary"            check-property-$vocabulary                    #{}]
     ["https://json-schema.org/draft/2020-12/vocab/format-annotation" "format"                 (make-check-property-format false)            #{}]
     ["https://json-schema.org/draft/2020-12/vocab/format-assertion"  "format"                 (make-check-property-format true)             #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "default"                check-property-default                        #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "deprecated"             check-property-deprecated                     #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "description"            check-property-description                    #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "examples"               check-property-examples                       #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "readOnly"               check-property-readOnly                       #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "title"                  check-property-title                          #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "writeOnly"              check-property-writeOnly                      #{}]
     ["https://json-schema.org/draft/2020-12/vocab/unevaluated"       "unevaluatedItems"       check-property-unevaluatedItems               #{"additionalItems" "uniqueItems" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2020-12/vocab/unevaluated"       "unevaluatedProperties"  check-property-unevaluatedProperties          #{"additionalProperties" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "const"                  check-property-const                          #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "dependentRequired"      check-property-dependentRequired              #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "enum"                   check-property-enum                           #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "exclusiveMaximum"       check-property-exclusiveMaximum-new           #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "exclusiveMinimum"       check-property-exclusiveMinimum-new           #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxContains"            check-property-maxContains                    #{"contains"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxItems"               check-property-maxItems                       #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxLength"              check-property-maxLength                      #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxProperties"          check-property-maxProperties                  #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maximum"                check-property-maximum-new                    #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minContains"            check-property-minContains                    #{"contains"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minItems"               check-property-minItems                       #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minLength"              check-property-minLength                      #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minProperties"          check-property-minProperties                  #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minimum"                check-property-minimum-new                    #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "multipleOf"             check-property-multipleOf                     #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "pattern"                check-property-pattern                        #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "required"               check-property-required                       #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "uniqueItems"            check-property-uniqueItems                    #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "type"                   check-property-type                           #{}]])
   :draft-next
   (sort-vocabulary
    [["https://json-schema.org/draft/next/vocab/applicator"           "additionalItems"        check-property-additionalItems                #{"items"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "additionalProperties"   check-property-additionalProperties           #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "allOf"                  check-property-allOf                          #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "anyOf"                  check-property-anyOf                          #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "contains"               check-property-contains                       #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "dependencies"           check-property-dependencies                   #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "dependentSchemas"       check-property-dependentSchemas               #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "else"                   check-property-else                           #{"if"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "if"                     check-property-if                             #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "items"                  check-property-items                          #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "not"                    check-property-not                            #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "oneOf"                  check-property-oneOf                          #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "patternProperties"      check-property-patternProperties              #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "prefixItems"            check-property-prefixItems                    #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "properties"             check-property-properties                     #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "propertyDependencies"   check-property-propertyDependencies           #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "propertyNames"          check-property-propertyNames                  #{}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "then"                   check-property-then                           #{"if"}]
     ["https://json-schema.org/draft/next/vocab/content"              "contentEncoding"        (make-check-property-contentEncoding false)   #{}]
     ["https://json-schema.org/draft/next/vocab/content"              "contentMediaType"       (make-check-property-contentMediaType false)  #{"contentEncoding"}]
     ["https://json-schema.org/draft/next/vocab/content"              "contentSchema"          (make-check-property-contentSchema false)     #{"contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$anchor"                check-property-$anchor                        #{"id" "$id"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$comment"               check-property-$comment                       #{}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$defs"                  check-property-$defs                          #{}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$dynamicAnchor"         check-property-$dynamicAnchor                 #{"id" "$id"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$dynamicRef"            check-property-$dynamicRef                    #{"$dynamicAnchor"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$id"                    check-property-$id                            #{}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$recursiveAnchor"       check-property-$recursiveAnchor               #{"id" "$id"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$recursiveRef"          check-property-$recursiveRef                  #{"$recursiveAnchor"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$ref"                   check-property-$ref                           #{"id" "$id" "$anchor"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$schema"                check-property-$schema                        #{}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$vocabulary"            check-property-$vocabulary                    #{}]
     ["https://json-schema.org/draft/next/vocab/format-annotation"    "format"                 (make-check-property-format false)            #{}]
     ["https://json-schema.org/draft/next/vocab/format-assertion"     "format"                 (make-check-property-format true)             #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "default"                check-property-default                        #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "deprecated"             check-property-deprecated                     #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "description"            check-property-description                    #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "examples"               check-property-examples                       #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "readOnly"               check-property-readOnly                       #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "title"                  check-property-title                          #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "writeOnly"              check-property-writeOnly                      #{}]
     ["https://json-schema.org/draft/next/vocab/unevaluated"          "unevaluatedItems"       check-property-unevaluatedItems               #{"additionalItems" "uniqueItems" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/next/vocab/unevaluated"          "unevaluatedProperties"  check-property-unevaluatedProperties          #{"additionalProperties" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "const"                  check-property-const                          #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "dependentRequired"      check-property-dependentRequired              #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "enum"                   check-property-enum                           #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "exclusiveMaximum"       check-property-exclusiveMaximum-new           #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "exclusiveMinimum"       check-property-exclusiveMinimum-new           #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "maxContains"            check-property-maxContains                    #{"contains"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "maxItems"               check-property-maxItems                       #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "maxLength"              check-property-maxLength                      #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "maxProperties"          check-property-maxProperties                  #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "maximum"                check-property-maximum-new                    #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "minContains"            check-property-minContains                    #{"contains"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "minItems"               check-property-minItems                       #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "minLength"              check-property-minLength                      #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "minProperties"          check-property-minProperties                  #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "minimum"                check-property-minimum-new                    #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "multipleOf"             check-property-multipleOf                     #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "pattern"                check-property-pattern                        #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "required"               check-property-required                       #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "uniqueItems"            check-property-uniqueItems                    #{}]
     ["https://json-schema.org/draft/next/vocab/validation"           "type"                   check-property-type                           #{}]])})

;; lets define a dialect as a function that given an m2 will return
;; you a correctly ordered sequence of pairs of m2-kv and
;; property-checker

;; should really convert strings to uris...
(defn make-dialect-2 [d v->b]
  (partial
   (make-stable-sort-by
    second
    (filter
     (comp (into #{} (keys v->b)) first)
     (draft->vocab-and-group-and-property-and-semantics d)))
   first
   (juxt first (comp third second))))

(def make-dialect (memoize make-dialect-2))

(def draft->default-dialect
  (map-values
   (fn [k v]
     (make-dialect k (into {} (map (fn [v] [v true]) (distinct (map first v))))))
   draft->vocab-and-group-and-property-and-semantics))
