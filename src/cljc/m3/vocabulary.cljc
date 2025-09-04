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
       (fn [c1 p1 m1]
         (let [[c1 es] (old-f1 c1 p1 m1)]
           [c1 m1 es]))])))

;; we should have enough now to adapt all vocab keyword fns to new
;; format, compose them into a single new format function and then
;; adapt the back to an old format function to plug into m3...
;;------------------------------------------------------------------------------

(defn sort-vocab [vs]
  (mapcat identity (topo-sort-by (comp (partial mapcat fourth) second) second (group-by second vs))))

(def draft->vocab
  {:draft3
   (sort-vocab
    [["https://json-schema.org/draft-03/vocab/applicator"             "additionalItems"        (old->new check-property-additionalItems)                #{"$schema" "items"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "additionalProperties"   (old->new check-property-additionalProperties)           #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "allOf"                  (old->new check-property-allOf)                          #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "anyOf"                  (old->new check-property-anyOf)                          #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "dependencies"           (old->new check-property-dependencies)                   #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "extends"                check-property-extends                                   #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "items"                  (old->new check-property-items)                          #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "not"                    (old->new check-property-not)                            #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "oneOf"                  (old->new check-property-oneOf)                          #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "patternProperties"      (old->new check-property-patternProperties)              #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "properties"             (old->new check-property-properties)                     #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/applicator"             "propertyDependencies"   (old->new check-property-propertyDependencies)           #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/core"                   "$ref"                   (old->new check-property-$ref)                           #{"$schema" "id"}]
     ["https://json-schema.org/draft-03/vocab/core"                   "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft-03/vocab/core"                   "definitions"            (old->new check-property-definitions)                    #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/core"                   "id"                     (old->new check-property-id)                             #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/meta-data"              "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/meta-data"              "deprecated"             check-property-deprecated                                #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/meta-data"              "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/meta-data"              "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "disallow"               check-property-disallow                                  #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "divisibleBy"            (old->new check-property-divisibleBy)                    #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "exclusiveMaximum"       (old->new check-property-exclusiveMaximum-old)           #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "exclusiveMinimum"       check-property-exclusiveMinimum-old                      #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "format"                 (old->new (make-check-property-format false))            #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "maxItems"               (old->new check-property-maxItems)                       #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "maxLength"              (old->new check-property-maxLength)                      #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "maxProperties"          (old->new check-property-maxProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "maximum"                (old->new check-property-maximum-old)                    #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "minItems"               (old->new check-property-minItems)                       #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "minLength"              (old->new check-property-minLength)                      #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "minProperties"          (old->new check-property-minProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "minimum"                check-property-minimum-old                               #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "multipleOf"             (old->new check-property-multipleOf)                     #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "pattern"                (old->new check-property-pattern)                        #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "required"               (old->new check-property-required)                       #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "type"                   (old->new check-property-type)                           #{"$schema"}]
     ["https://json-schema.org/draft-03/vocab/validation"             "uniqueItems"            (old->new check-property-uniqueItems)                    #{"$schema"}]])
   :draft4
   (sort-vocab
    [["https://json-schema.org/draft-04/vocab/applicator"             "additionalItems"        (old->new check-property-additionalItems)                #{"$schema" "items"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "additionalProperties"   (old->new check-property-additionalProperties)           #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "allOf"                  (old->new check-property-allOf)                          #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "anyOf"                  (old->new check-property-anyOf)                          #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "dependencies"           (old->new check-property-dependencies)                   #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "items"                  (old->new check-property-items)                          #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "not"                    (old->new check-property-not)                            #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "oneOf"                  (old->new check-property-oneOf)                          #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "patternProperties"      (old->new check-property-patternProperties)              #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "properties"             (old->new check-property-properties)                     #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/applicator"             "propertyDependencies"   (old->new check-property-propertyDependencies)           #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/core"                   "$ref"                   (old->new check-property-$ref)                           #{"$schema" "id"}]
     ["https://json-schema.org/draft-04/vocab/core"                   "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft-04/vocab/core"                   "definitions"            (old->new check-property-definitions)                    #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/core"                   "id"                     (old->new check-property-id)                             #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/meta-data"              "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/meta-data"              "deprecated"             check-property-deprecated                                #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/meta-data"              "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/meta-data"              "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "exclusiveMaximum"       (old->new check-property-exclusiveMaximum-old)           #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "exclusiveMinimum"       check-property-exclusiveMinimum-old                      #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "format"                 (old->new (make-check-property-format true))             #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "maxItems"               (old->new check-property-maxItems)                       #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "maxLength"              (old->new check-property-maxLength)                      #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "maxProperties"          (old->new check-property-maxProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "maximum"                (old->new check-property-maximum-old)                    #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "minItems"               (old->new check-property-minItems)                       #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "minLength"              (old->new check-property-minLength)                      #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "minProperties"          (old->new check-property-minProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "minimum"                check-property-minimum-old                               #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "multipleOf"             (old->new check-property-multipleOf)                     #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "pattern"                (old->new check-property-pattern)                        #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "required"               (old->new check-property-required)                       #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "uniqueItems"            (old->new check-property-uniqueItems)                    #{"$schema"}]
     ["https://json-schema.org/draft-04/vocab/validation"             "type"                   (old->new check-property-type)                           #{"$schema"}]])
   :draft6
   (sort-vocab
    [["https://json-schema.org/draft-06/vocab/applicator"             "additionalItems"        (old->new check-property-additionalItems)                #{"$schema" "items"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "additionalProperties"   (old->new check-property-additionalProperties)           #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "allOf"                  (old->new check-property-allOf)                          #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "anyOf"                  (old->new check-property-anyOf)                          #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "contains"               (old->new check-property-contains)                       #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "dependencies"           (old->new check-property-dependencies)                   #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "items"                  (old->new check-property-items)                          #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "not"                    (old->new check-property-not)                            #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "oneOf"                  (old->new check-property-oneOf)                          #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "patternProperties"      (old->new check-property-patternProperties)              #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "properties"             (old->new check-property-properties)                     #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "propertyDependencies"   (old->new check-property-propertyDependencies)           #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/applicator"             "propertyNames"          (old->new check-property-propertyNames)                  #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/core"                   "$id"                    (old->new check-property-$id)                            #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/core"                   "$ref"                   (old->new check-property-$ref)                           #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft-06/vocab/core"                   "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft-06/vocab/core"                   "definitions"            (old->new check-property-definitions)                    #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/core"                   "id"                     (old->new check-property-id)                             #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/meta-data"              "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/meta-data"              "deprecated"             check-property-deprecated                                #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/meta-data"              "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/meta-data"              "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "const"                  check-property-const                                     #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "exclusiveMaximum"       (old->new check-property-exclusiveMaximum-new)           #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "exclusiveMinimum"       (old->new check-property-exclusiveMinimum-new)           #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "format"                 (old->new (make-check-property-format true))             #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maxContains"            (old->new check-property-maxContains)                    #{"$schema" "contains"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maxItems"               (old->new check-property-maxItems)                       #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maxLength"              (old->new check-property-maxLength)                      #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maxProperties"          (old->new check-property-maxProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "maximum"                (old->new check-property-maximum-new)                    #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minContains"            (old->new check-property-minContains)                    #{"$schema" "contains"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minItems"               (old->new check-property-minItems)                       #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minLength"              (old->new check-property-minLength)                      #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minProperties"          (old->new check-property-minProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "minimum"                check-property-minimum-new                               #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "multipleOf"             (old->new check-property-multipleOf)                     #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "pattern"                (old->new check-property-pattern)                        #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "required"               (old->new check-property-required)                       #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "uniqueItems"            (old->new check-property-uniqueItems)                    #{"$schema"}]
     ["https://json-schema.org/draft-06/vocab/validation"             "type"                   (old->new check-property-type)                           #{"$schema"}]])
   :draft7
   (sort-vocab
    [["https://json-schema.org/draft-07/vocab/applicator"             "additionalItems"        (old->new check-property-additionalItems)                #{"$schema" "items"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "additionalProperties"   (old->new check-property-additionalProperties)           #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "allOf"                  (old->new check-property-allOf)                          #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "anyOf"                  (old->new check-property-anyOf)                          #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "contains"               (old->new check-property-contains)                       #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "dependencies"           (old->new check-property-dependencies)                   #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "else"                   (old->new check-property-else)                           #{"$schema" "if"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "if"                     (old->new check-property-if)                             #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "items"                  (old->new check-property-items)                          #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "not"                    (old->new check-property-not)                            #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "oneOf"                  (old->new check-property-oneOf)                          #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "patternProperties"      (old->new check-property-patternProperties)              #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "properties"             (old->new check-property-properties)                     #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "propertyDependencies"   (old->new check-property-propertyDependencies)           #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "propertyNames"          (old->new check-property-propertyNames)                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/applicator"             "then"                   (old->new check-property-then)                           #{"$schema" "if"}]
     ["https://json-schema.org/draft-07/vocab/content"                "contentEncoding"        (old->new (make-check-property-contentEncoding true))    #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/content"                "contentMediaType"       (old->new (make-check-property-contentMediaType true))   #{"$schema" "contentEncoding"}]
     ["https://json-schema.org/draft-07/vocab/content"                "contentSchema"          (old->new (make-check-property-contentSchema true))      #{"$schema" "contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "$comment"               check-property-$comment                                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "$id"                    (old->new check-property-$id)                            #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "$ref"                   (old->new check-property-$ref)                           #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "definitions"            (old->new check-property-definitions)                    #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/core"                   "id"                     (old->new check-property-id)                             #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "deprecated"             check-property-deprecated                                #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "examples"               check-property-examples                                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "readOnly"               check-property-readOnly                                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/meta-data"              "writeOnly"              check-property-writeOnly                                 #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "const"                  check-property-const                                     #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "exclusiveMaximum"       (old->new check-property-exclusiveMaximum-new)           #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "exclusiveMinimum"       (old->new check-property-exclusiveMinimum-new)           #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "format"                 (old->new (make-check-property-format true))             #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maxContains"            (old->new check-property-maxContains)                    #{"$schema" "contains"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maxItems"               (old->new check-property-maxItems)                       #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maxLength"              (old->new check-property-maxLength)                      #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maxProperties"          (old->new check-property-maxProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "maximum"                (old->new check-property-maximum-new)                    #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minContains"            (old->new check-property-minContains)                    #{"$schema" "contains"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minItems"               (old->new check-property-minItems)                       #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minLength"              (old->new check-property-minLength)                      #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minProperties"          (old->new check-property-minProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "minimum"                check-property-minimum-new                               #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "multipleOf"             (old->new check-property-multipleOf)                     #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "pattern"                (old->new check-property-pattern)                        #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "required"               (old->new check-property-required)                       #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "uniqueItems"            (old->new check-property-uniqueItems)                    #{"$schema"}]
     ["https://json-schema.org/draft-07/vocab/validation"             "type"                   (old->new check-property-type)                           #{"$schema"}]])
   :draft2019-09
   (sort-vocab
    [["https://json-schema.org/draft/2019-09/vocab/applicator"        "additionalItems"        (old->new check-property-additionalItems)                #{"$schema" "items"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "additionalProperties"   (old->new check-property-additionalProperties)           #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "allOf"                  (old->new check-property-allOf)                          #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "anyOf"                  (old->new check-property-anyOf)                          #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "contains"               (old->new check-property-contains)                       #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "dependencies"           (old->new check-property-dependencies)                   #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "dependentSchemas"       (old->new check-property-dependentSchemas)               #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "else"                   (old->new check-property-else)                           #{"$schema" "if"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "if"                     (old->new check-property-if)                             #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "items"                  (old->new check-property-items)                          #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "not"                    (old->new check-property-not)                            #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "oneOf"                  (old->new check-property-oneOf)                          #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "patternProperties"      (old->new check-property-patternProperties)              #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "properties"             (old->new check-property-properties)                     #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "propertyDependencies"   (old->new check-property-propertyDependencies)           #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "propertyNames"          (old->new check-property-propertyNames)                  #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "then"                   (old->new check-property-then)                           #{"$schema" "if"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "unevaluatedItems"       (old->new check-property-unevaluatedItems)               #{"$schema" "additionalItems" "uniqueItems" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator"        "unevaluatedProperties"  (old->new check-property-unevaluatedProperties)          #{"$schema" "additionalProperties" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2019-09/vocab/content"           "contentEncoding"        (old->new (make-check-property-contentEncoding false))   #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/content"           "contentMediaType"       (old->new (make-check-property-contentMediaType false))  #{"$schema" "contentEncoding"}]
     ["https://json-schema.org/draft/2019-09/vocab/content"           "contentSchema"          (old->new (make-check-property-contentSchema false))     #{"$schema" "contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$anchor"                (old->new check-property-$anchor)                        #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$comment"               check-property-$comment                                  #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$defs"                  (old->new check-property-$defs)                          #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$id"                    (old->new check-property-$id)                            #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$recursiveAnchor"       (old->new check-property-$recursiveAnchor)               #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$recursiveRef"          check-property-$recursiveRef                             #{"$schema" "$recursiveAnchor"}]
     ["https://json-schema.org/draft/2019-09/vocab/core"              "$ref"                   (old->new check-property-$ref)                           #{"$schema" "id" "$id" "$anchor"}]
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
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "dependentRequired"      (old->new check-property-dependentRequired)              #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "exclusiveMaximum"       (old->new check-property-exclusiveMaximum-new)           #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "exclusiveMinimum"       (old->new check-property-exclusiveMinimum-new)           #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "format"                 (old->new (make-check-property-format true))             #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maxContains"            (old->new check-property-maxContains)                    #{"$schema" "contains"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maxItems"               (old->new check-property-maxItems)                       #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maxLength"              (old->new check-property-maxLength)                      #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maxProperties"          (old->new check-property-maxProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "maximum"                (old->new check-property-maximum-new)                    #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minContains"            (old->new check-property-minContains)                    #{"$schema" "contains"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minItems"               (old->new check-property-minItems)                       #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minLength"              (old->new check-property-minLength)                      #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minProperties"          (old->new check-property-minProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "minimum"                check-property-minimum-new                               #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "multipleOf"             (old->new check-property-multipleOf)                     #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "pattern"                (old->new check-property-pattern)                        #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "required"               (old->new check-property-required)                       #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "uniqueItems"            (old->new check-property-uniqueItems)                    #{"$schema"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation"        "type"                   (old->new check-property-type)                           #{"$schema"}]])
   :draft2020-12
   (sort-vocab
    [["https://json-schema.org/draft/2020-12/vocab/applicator"        "additionalItems"        (old->new check-property-additionalItems)                #{"$schema" "items"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "additionalProperties"   (old->new check-property-additionalProperties)           #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "allOf"                  (old->new check-property-allOf)                          #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "anyOf"                  (old->new check-property-anyOf)                          #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "contains"               (old->new check-property-contains)                       #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "dependencies"           (old->new check-property-dependencies)                   #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "dependentSchemas"       (old->new check-property-dependentSchemas)               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "else"                   (old->new check-property-else)                           #{"$schema" "if"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "if"                     (old->new check-property-if)                             #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "items"                  (old->new check-property-items)                          #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "not"                    (old->new check-property-not)                            #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "oneOf"                  (old->new check-property-oneOf)                          #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "patternProperties"      (old->new check-property-patternProperties)              #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "prefixItems"            (old->new check-property-prefixItems)                    #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "properties"             (old->new check-property-properties)                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "propertyDependencies"   (old->new check-property-propertyDependencies)           #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "propertyNames"          (old->new check-property-propertyNames)                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator"        "then"                   (old->new check-property-then)                           #{"$schema" "if"}]
     ["https://json-schema.org/draft/2020-12/vocab/content"           "contentEncoding"        (old->new (make-check-property-contentEncoding false))   #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/content"           "contentMediaType"       (old->new (make-check-property-contentMediaType false))  #{"$schema" "contentEncoding"}]
     ["https://json-schema.org/draft/2020-12/vocab/content"           "contentSchema"          (old->new (make-check-property-contentSchema false))     #{"$schema" "contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$anchor"                (old->new check-property-$anchor)                        #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$comment"               check-property-$comment                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$defs"                  (old->new check-property-$defs)                          #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$dynamicAnchor"         (old->new check-property-$dynamicAnchor)                 #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$dynamicRef"            check-property-$dynamicRef                               #{"$schema" "$dynamicAnchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$id"                    (old->new check-property-$id)                            #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$recursiveAnchor"       (old->new check-property-$recursiveAnchor)               #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$recursiveRef"          check-property-$recursiveRef                             #{"$schema" "$recursiveAnchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$ref"                   (old->new check-property-$ref)                           #{"$schema" "id" "$id" "$anchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft/2020-12/vocab/core"              "$vocabulary"            check-property-$vocabulary                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/format-annotation" "format"                 (old->new (make-check-property-format false))            #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/format-assertion"  "format"                 (old->new (make-check-property-format true))             #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "deprecated"             check-property-deprecated                                #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "examples"               check-property-examples                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "readOnly"               check-property-readOnly                                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data"         "writeOnly"              check-property-writeOnly                                 #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/unevaluated"       "unevaluatedItems"       (old->new check-property-unevaluatedItems)               #{"$schema" "additionalItems" "uniqueItems" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2020-12/vocab/unevaluated"       "unevaluatedProperties"  (old->new check-property-unevaluatedProperties)          #{"$schema" "additionalProperties" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "const"                  check-property-const                                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "dependentRequired"      (old->new check-property-dependentRequired)              #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "exclusiveMaximum"       (old->new check-property-exclusiveMaximum-new)           #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "exclusiveMinimum"       (old->new check-property-exclusiveMinimum-new)           #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxContains"            (old->new check-property-maxContains)                    #{"$schema" "contains"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxItems"               (old->new check-property-maxItems)                       #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxLength"              (old->new check-property-maxLength)                      #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maxProperties"          (old->new check-property-maxProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "maximum"                (old->new check-property-maximum-new)                    #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minContains"            (old->new check-property-minContains)                    #{"$schema" "contains"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minItems"               (old->new check-property-minItems)                       #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minLength"              (old->new check-property-minLength)                      #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minProperties"          (old->new check-property-minProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "minimum"                check-property-minimum-new                               #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "multipleOf"             (old->new check-property-multipleOf)                     #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "pattern"                (old->new check-property-pattern)                        #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "required"               (old->new check-property-required)                       #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "uniqueItems"            (old->new check-property-uniqueItems)                    #{"$schema"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation"        "type"                   (old->new check-property-type)                           #{"$schema"}]])
   :draft-next
   (sort-vocab
    [["https://json-schema.org/draft/next/vocab/applicator"           "additionalItems"        (old->new check-property-additionalItems)                #{"$schema" "items"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "additionalProperties"   (old->new check-property-additionalProperties)           #{"$schema" "properties" "patternProperties"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "allOf"                  (old->new check-property-allOf)                          #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "anyOf"                  (old->new check-property-anyOf)                          #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "contains"               (old->new check-property-contains)                       #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "dependencies"           (old->new check-property-dependencies)                   #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "dependentSchemas"       (old->new check-property-dependentSchemas)               #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "else"                   (old->new check-property-else)                           #{"$schema" "if"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "if"                     (old->new check-property-if)                             #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "items"                  (old->new check-property-items)                          #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "not"                    (old->new check-property-not)                            #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "oneOf"                  (old->new check-property-oneOf)                          #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "patternProperties"      (old->new check-property-patternProperties)              #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "prefixItems"            (old->new check-property-prefixItems)                    #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "properties"             (old->new check-property-properties)                     #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "propertyDependencies"   (old->new check-property-propertyDependencies)           #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "propertyNames"          (old->new check-property-propertyNames)                  #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/applicator"           "then"                   (old->new check-property-then)                           #{"$schema" "if"}]
     ["https://json-schema.org/draft/next/vocab/content"              "contentEncoding"        (old->new (make-check-property-contentEncoding false))   #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/content"              "contentMediaType"       (old->new (make-check-property-contentMediaType false))  #{"$schema" "contentEncoding"}]
     ["https://json-schema.org/draft/next/vocab/content"              "contentSchema"          (old->new (make-check-property-contentSchema false))     #{"$schema" "contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$anchor"                (old->new check-property-$anchor)                        #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$comment"               check-property-$comment                                  #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$defs"                  (old->new check-property-$defs)                          #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$dynamicAnchor"         (old->new check-property-$dynamicAnchor)                 #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$dynamicRef"            check-property-$dynamicRef                               #{"$schema" "$dynamicAnchor"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$id"                    (old->new check-property-$id)                            #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$recursiveAnchor"       (old->new check-property-$recursiveAnchor)               #{"$schema" "id" "$id"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$recursiveRef"          check-property-$recursiveRef                             #{"$schema" "$recursiveAnchor"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$ref"                   (old->new check-property-$ref)                           #{"$schema" "id" "$id" "$anchor"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$schema"                check-property-$schema                                   #{"$ref"}]
     ["https://json-schema.org/draft/next/vocab/core"                 "$vocabulary"            check-property-$vocabulary                               #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/format-annotation"    "format"                 (old->new (make-check-property-format false))            #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/format-assertion"     "format"                 (old->new (make-check-property-format true))             #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "default"                check-property-default                                   #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "deprecated"             (old->new check-property-deprecated)                     #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "description"            check-property-description                               #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "examples"               check-property-examples                                  #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "readOnly"               check-property-readOnly                                  #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "title"                  check-property-title                                     #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/meta-data"            "writeOnly"              check-property-writeOnly                                 #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/unevaluated"          "unevaluatedItems"       (old->new check-property-unevaluatedItems)               #{"$schema" "additionalItems" "uniqueItems" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/next/vocab/unevaluated"          "unevaluatedProperties"  (old->new check-property-unevaluatedProperties)          #{"$schema" "additionalProperties" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "const"                  check-property-const                                     #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "dependentRequired"      (old->new check-property-dependentRequired)              #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "enum"                   check-property-enum                                      #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "exclusiveMaximum"       (old->new check-property-exclusiveMaximum-new)           #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "exclusiveMinimum"       (old->new check-property-exclusiveMinimum-new)           #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "maxContains"            (old->new check-property-maxContains)                    #{"$schema" "contains"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "maxItems"               (old->new check-property-maxItems)                       #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "maxLength"              (old->new check-property-maxLength)                      #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "maxProperties"          (old->new check-property-maxProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "maximum"                (old->new check-property-maximum-new)                    #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "minContains"            (old->new check-property-minContains)                    #{"$schema" "contains"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "minItems"               (old->new check-property-minItems)                       #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "minLength"              (old->new check-property-minLength)                      #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "minProperties"          (old->new check-property-minProperties)                  #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "minimum"                check-property-minimum-new                               #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "multipleOf"             (old->new check-property-multipleOf)                     #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "pattern"                (old->new check-property-pattern)                        #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "required"               (old->new check-property-required)                       #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "uniqueItems"            (old->new check-property-uniqueItems)                    #{"$schema"}]
     ["https://json-schema.org/draft/next/vocab/validation"           "type"                   (old->new check-property-type)                           #{"$schema"}]])})

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
     (draft->vocab d)))
   first
   (juxt first (comp third second))))

(def make-dialect (memoize make-dialect-2))

(def draft->default-dialect
  (map-values
   (fn [k v]
     (make-dialect k (into {} (map (fn [v] [v true]) (distinct (map first v))))))
   draft->vocab))

;;------------------------------------------------------------------------------

(defn new-make-dialect-2 [vocab v->b]
  (partial
   (make-stable-sort-by
    second
    (filter
     (comp (into #{} (keys v->b)) first)
     vocab))
   first
   (juxt first (comp third second))))

(def new-make-dialect (memoize new-make-dialect-2))
