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
   [m3.util :refer [map-values topo-sort-by make-stable-sort-by fourth]]
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

(defn fifth [v] (nth v 4))

(defn third [v] (nth v 2))

(defn sort-vocabulary [vs]
  (mapcat identity (topo-sort-by (comp (partial mapcat fifth) second) second (group-by third vs))))

(def draft->vocab-and-group-and-property-and-semantics
  {:draft3
   (sort-vocabulary
    [["https://json-schema.org/draft-03/vocab/applicator" :applicator "additionalItems" check-property-additionalItems #{"items"}]
     ["https://json-schema.org/draft-03/vocab/applicator" :applicator "additionalProperties" check-property-additionalProperties #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft-03/vocab/applicator" :applicator "allOf" check-property-allOf #{}]
     ["https://json-schema.org/draft-03/vocab/applicator" :applicator "anyOf" check-property-anyOf #{}]
     ["https://json-schema.org/draft-03/vocab/applicator" :applicator "dependencies" check-property-dependencies #{}]
     ["https://json-schema.org/draft-03/vocab/applicator" :applicator "extends" check-property-extends #{}]
     ["https://json-schema.org/draft-03/vocab/applicator" :applicator "items" check-property-items #{}]
     ["https://json-schema.org/draft-03/vocab/applicator" :applicator "not" check-property-not #{}]
     ["https://json-schema.org/draft-03/vocab/applicator" :applicator "oneOf" check-property-oneOf #{}]
     ["https://json-schema.org/draft-03/vocab/applicator" :applicator "patternProperties" check-property-patternProperties #{}]
     ["https://json-schema.org/draft-03/vocab/applicator" :applicator "properties" check-property-properties #{}]
     ["https://json-schema.org/draft-03/vocab/applicator" :applicator "propertyDependencies" check-property-propertyDependencies #{}]
     ["https://json-schema.org/draft-03/vocab/core" :core "$ref" check-property-$ref #{"id"}]
     ["https://json-schema.org/draft-03/vocab/core" :core "$schema" check-property-$schema #{}]
     ["https://json-schema.org/draft-03/vocab/core" :core "$vocabulary" check-property-$vocabulary #{}] ;; TODO - should not be here
     ["https://json-schema.org/draft-03/vocab/core" :core "definitions" check-property-definitions #{}]
     ["https://json-schema.org/draft-03/vocab/core" :core "id" check-property-id #{}]
     ["https://json-schema.org/draft-03/vocab/meta-data" :meta-data "default" check-property-default #{}]
     ["https://json-schema.org/draft-03/vocab/meta-data" :meta-data "deprecated" check-property-deprecated #{}]
     ["https://json-schema.org/draft-03/vocab/meta-data" :meta-data "description" check-property-description #{}]
     ["https://json-schema.org/draft-03/vocab/meta-data" :meta-data "title" check-property-title #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "disallow" check-property-disallow #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "divisibleBy" check-property-divisibleBy #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "enum" check-property-enum #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "exclusiveMaximum" check-property-exclusiveMaximum-old #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "exclusiveMinimum" check-property-exclusiveMinimum-old #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "format" (make-check-property-format false) #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "maxItems" check-property-maxItems #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "maxLength" check-property-maxLength #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "maxProperties" check-property-maxProperties #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "maximum" check-property-maximum-old #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "minItems" check-property-minItems #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "minLength" check-property-minLength #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "minProperties" check-property-minProperties #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "minimum" check-property-minimum-old #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "multipleOf" check-property-multipleOf #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "pattern" check-property-pattern #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "required" check-property-required #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "type" check-property-type #{}]
     ["https://json-schema.org/draft-03/vocab/validation" :validation "uniqueItems" check-property-uniqueItems #{}]])
   :draft4
   (sort-vocabulary
    [["https://json-schema.org/draft-04/vocab/applicator" :applicator "additionalItems" check-property-additionalItems #{"items"}]
     ["https://json-schema.org/draft-04/vocab/applicator" :applicator "additionalProperties" check-property-additionalProperties #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft-04/vocab/applicator" :applicator "allOf" check-property-allOf #{}]
     ["https://json-schema.org/draft-04/vocab/applicator" :applicator "anyOf" check-property-anyOf #{}]
     ["https://json-schema.org/draft-04/vocab/applicator" :applicator "dependencies" check-property-dependencies #{}]
     ["https://json-schema.org/draft-04/vocab/applicator" :applicator "items" check-property-items #{}]
     ["https://json-schema.org/draft-04/vocab/applicator" :applicator "not" check-property-not #{}]
     ["https://json-schema.org/draft-04/vocab/applicator" :applicator "oneOf" check-property-oneOf #{}]
     ["https://json-schema.org/draft-04/vocab/applicator" :applicator "patternProperties" check-property-patternProperties #{}]
     ["https://json-schema.org/draft-04/vocab/applicator" :applicator "properties" check-property-properties #{}]
     ["https://json-schema.org/draft-04/vocab/applicator" :applicator "propertyDependencies" check-property-propertyDependencies #{}]
     ["https://json-schema.org/draft-04/vocab/core" :core "$ref" check-property-$ref #{"id"}]
     ["https://json-schema.org/draft-04/vocab/core" :core "$schema" check-property-$schema #{}]
     ["https://json-schema.org/draft-04/vocab/core" :core "$vocabulary" check-property-$vocabulary #{}] ; TODO: should not be here
     ["https://json-schema.org/draft-04/vocab/core" :core "definitions" check-property-definitions #{}]
     ["https://json-schema.org/draft-04/vocab/core" :core "id" check-property-id #{}]
     ["https://json-schema.org/draft-04/vocab/meta-data" :meta-data "default" check-property-default #{}]
     ["https://json-schema.org/draft-04/vocab/meta-data" :meta-data "deprecated" check-property-deprecated #{}]
     ["https://json-schema.org/draft-04/vocab/meta-data" :meta-data "description" check-property-description #{}]
     ["https://json-schema.org/draft-04/vocab/meta-data" :meta-data "title" check-property-title #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "enum" check-property-enum #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "exclusiveMaximum" check-property-exclusiveMaximum-old #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "exclusiveMinimum" check-property-exclusiveMinimum-old #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "format" (make-check-property-format true) #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "maxItems" check-property-maxItems #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "maxLength" check-property-maxLength #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "maxProperties" check-property-maxProperties #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "maximum" check-property-maximum-old #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "minItems" check-property-minItems #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "minLength" check-property-minLength #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "minProperties" check-property-minProperties #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "minimum" check-property-minimum-old #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "multipleOf" check-property-multipleOf #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "pattern" check-property-pattern #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "required" check-property-required #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "uniqueItems" check-property-uniqueItems #{}]
     ["https://json-schema.org/draft-04/vocab/validation" :validation "type" check-property-type #{}]])
   :draft6
   (sort-vocabulary
    [["https://json-schema.org/draft-06/vocab/applicator" :applicator "additionalItems" check-property-additionalItems #{"items"}]
     ["https://json-schema.org/draft-06/vocab/applicator" :applicator "additionalProperties" check-property-additionalProperties #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft-06/vocab/applicator" :applicator "allOf" check-property-allOf #{}]
     ["https://json-schema.org/draft-06/vocab/applicator" :applicator "anyOf" check-property-anyOf #{}]
     ["https://json-schema.org/draft-06/vocab/applicator" :applicator "contains" check-property-contains #{}]
     ["https://json-schema.org/draft-06/vocab/applicator" :applicator "dependencies" check-property-dependencies #{}]
     ["https://json-schema.org/draft-06/vocab/applicator" :applicator "items" check-property-items #{}]
     ["https://json-schema.org/draft-06/vocab/applicator" :applicator "not" check-property-not #{}]
     ["https://json-schema.org/draft-06/vocab/applicator" :applicator "oneOf" check-property-oneOf #{}]
     ["https://json-schema.org/draft-06/vocab/applicator" :applicator "patternProperties" check-property-patternProperties #{}]
     ["https://json-schema.org/draft-06/vocab/applicator" :applicator "properties" check-property-properties #{}]
     ["https://json-schema.org/draft-06/vocab/applicator" :applicator "propertyDependencies" check-property-propertyDependencies #{}]
     ["https://json-schema.org/draft-06/vocab/applicator" :applicator "propertyNames" check-property-propertyNames #{}]
     ["https://json-schema.org/draft-06/vocab/core" :core "$id" check-property-$id #{}]
     ["https://json-schema.org/draft-06/vocab/core" :core "$ref" check-property-$ref #{"id" "$id"}]
     ["https://json-schema.org/draft-06/vocab/core" :core "$schema" check-property-$schema #{}]
     ["https://json-schema.org/draft-06/vocab/core" :core "$vocabulary" check-property-$vocabulary #{}] ; TODO: should not be here
     ["https://json-schema.org/draft-06/vocab/core" :core "definitions" check-property-definitions #{}]
     ["https://json-schema.org/draft-06/vocab/core" :core "id" check-property-id #{}]
     ["https://json-schema.org/draft-06/vocab/meta-data" :meta-data "default" check-property-default #{}]
     ["https://json-schema.org/draft-06/vocab/meta-data" :meta-data "deprecated" check-property-deprecated #{}]
     ["https://json-schema.org/draft-06/vocab/meta-data" :meta-data "description" check-property-description #{}]
     ["https://json-schema.org/draft-06/vocab/meta-data" :meta-data "title" check-property-title #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "const" check-property-const #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "enum" check-property-enum #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "exclusiveMaximum" check-property-exclusiveMaximum-new #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "exclusiveMinimum" check-property-exclusiveMinimum-new #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "format" (make-check-property-format true) #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "maxContains" check-property-maxContains #{"contains"}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "maxItems" check-property-maxItems #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "maxLength" check-property-maxLength #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "maxProperties" check-property-maxProperties #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "maximum" check-property-maximum-new #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "minContains" check-property-minContains #{"contains"}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "minItems" check-property-minItems #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "minLength" check-property-minLength #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "minProperties" check-property-minProperties #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "minimum" check-property-minimum-new #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "multipleOf" check-property-multipleOf #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "pattern" check-property-pattern #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "required" check-property-required #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "uniqueItems" check-property-uniqueItems #{}]
     ["https://json-schema.org/draft-06/vocab/validation" :validation "type" check-property-type #{}]])
   :draft7
   (sort-vocabulary
    [["https://json-schema.org/draft-07/vocab/applicator" :applicator "additionalItems" check-property-additionalItems #{"items"}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "additionalProperties" check-property-additionalProperties #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "allOf" check-property-allOf #{}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "anyOf" check-property-anyOf #{}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "contains" check-property-contains #{}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "dependencies" check-property-dependencies #{}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "else" check-property-else #{"if"}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "if" check-property-if #{}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "items" check-property-items #{}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "not" check-property-not #{}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "oneOf" check-property-oneOf #{}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "patternProperties" check-property-patternProperties #{}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "properties" check-property-properties #{}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "propertyDependencies" check-property-propertyDependencies #{}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "propertyNames" check-property-propertyNames #{}]
     ["https://json-schema.org/draft-07/vocab/applicator" :applicator "then" check-property-then #{"if"}]
     ["https://json-schema.org/draft-07/vocab/content" :content "contentEncoding" (make-check-property-contentEncoding true) #{}]
     ["https://json-schema.org/draft-07/vocab/content" :content "contentMediaType" (make-check-property-contentMediaType true) #{"contentEncoding"}]
     ["https://json-schema.org/draft-07/vocab/content" :content "contentSchema" (make-check-property-contentSchema true) #{"contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft-07/vocab/core" :core "$comment" check-property-$comment #{}]
     ["https://json-schema.org/draft-07/vocab/core" :core "$id" check-property-$id #{}]
     ["https://json-schema.org/draft-07/vocab/core" :core "$ref" check-property-$ref #{"id" "$id"}]
     ["https://json-schema.org/draft-07/vocab/core" :core "$schema" check-property-$schema #{}]
     ["https://json-schema.org/draft-07/vocab/core" :core "$vocabulary" check-property-$vocabulary #{}] ; TODO: should not be here
     ["https://json-schema.org/draft-07/vocab/core" :core "definitions" check-property-definitions #{}]
     ["https://json-schema.org/draft-07/vocab/core" :core "id" check-property-id #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data" :meta-data "default" check-property-default #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data" :meta-data "deprecated" check-property-deprecated #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data" :meta-data "description" check-property-description #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data" :meta-data "examples" check-property-examples #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data" :meta-data "readOnly" check-property-readOnly #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data" :meta-data "title" check-property-title #{}]
     ["https://json-schema.org/draft-07/vocab/meta-data" :meta-data "writeOnly" check-property-writeOnly #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "const" check-property-const #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "enum" check-property-enum #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "exclusiveMaximum" check-property-exclusiveMaximum-new #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "exclusiveMinimum" check-property-exclusiveMinimum-new #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "format" (make-check-property-format true) #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "maxContains" check-property-maxContains #{"contains"}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "maxItems" check-property-maxItems #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "maxLength" check-property-maxLength #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "maxProperties" check-property-maxProperties #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "maximum" check-property-maximum-new #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "minContains" check-property-minContains #{"contains"}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "minItems" check-property-minItems #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "minLength" check-property-minLength #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "minProperties" check-property-minProperties #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "minimum" check-property-minimum-new #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "multipleOf" check-property-multipleOf #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "pattern" check-property-pattern #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "required" check-property-required #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "uniqueItems" check-property-uniqueItems #{}]
     ["https://json-schema.org/draft-07/vocab/validation" :validation "type" check-property-type #{}]])
   :draft2019-09
   (sort-vocabulary
    [["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "additionalItems" check-property-additionalItems #{"items"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "additionalProperties" check-property-additionalProperties #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "allOf" check-property-allOf #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "anyOf" check-property-anyOf #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "contains" check-property-contains #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "dependencies" check-property-dependencies #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "dependentSchemas" check-property-dependentSchemas #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "else" check-property-else #{"if"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "if" check-property-if #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "items" check-property-items #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "not" check-property-not #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "oneOf" check-property-oneOf #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "patternProperties" check-property-patternProperties #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "properties" check-property-properties #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "propertyDependencies" check-property-propertyDependencies #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "propertyNames" check-property-propertyNames #{}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "then" check-property-then #{"if"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "unevaluatedItems" check-property-unevaluatedItems #{"additionalItems" "uniqueItems" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2019-09/vocab/applicator" :applicator "unevaluatedProperties" check-property-unevaluatedProperties #{"additionalProperties" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2019-09/vocab/content" :content "contentEncoding" (make-check-property-contentEncoding false) #{}]
     ["https://json-schema.org/draft/2019-09/vocab/content" :content "contentMediaType" (make-check-property-contentMediaType false) #{"contentEncoding"}]
     ["https://json-schema.org/draft/2019-09/vocab/content" :content "contentSchema" (make-check-property-contentSchema false) #{"contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft/2019-09/vocab/core" :core "$anchor" check-property-$anchor #{"id" "$id"}]
     ["https://json-schema.org/draft/2019-09/vocab/core" :core "$comment" check-property-$comment #{}]
     ["https://json-schema.org/draft/2019-09/vocab/core" :core "$defs" check-property-$defs #{}]
     ["https://json-schema.org/draft/2019-09/vocab/core" :core "$id" check-property-$id #{}]
     ["https://json-schema.org/draft/2019-09/vocab/core" :core "$recursiveAnchor" check-property-$recursiveAnchor #{"id" "$id"}]
     ["https://json-schema.org/draft/2019-09/vocab/core" :core "$recursiveRef" check-property-$recursiveRef #{"$recursiveAnchor"}]
     ["https://json-schema.org/draft/2019-09/vocab/core" :core "$ref" check-property-$ref #{"id" "$id" "$anchor"}]
     ["https://json-schema.org/draft/2019-09/vocab/core" :core "$schema" check-property-$schema #{}]
     ["https://json-schema.org/draft/2019-09/vocab/core" :core "$vocabulary" check-property-$vocabulary #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data" :meta-data "default" check-property-default #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data" :meta-data "deprecated" check-property-deprecated #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data" :meta-data "description" check-property-description #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data" :meta-data "examples" check-property-examples #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data" :meta-data "readOnly" check-property-readOnly #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data" :meta-data "title" check-property-title #{}]
     ["https://json-schema.org/draft/2019-09/vocab/meta-data" :meta-data "writeOnly" check-property-writeOnly #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "const" check-property-const #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "dependentRequired" check-property-dependentRequired #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "enum" check-property-enum #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "exclusiveMaximum" check-property-exclusiveMaximum-new #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "exclusiveMinimum" check-property-exclusiveMinimum-new #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "format" (make-check-property-format true) #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "maxContains" check-property-maxContains #{"contains"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "maxItems" check-property-maxItems #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "maxLength" check-property-maxLength #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "maxProperties" check-property-maxProperties #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "maximum" check-property-maximum-new #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "minContains" check-property-minContains #{"contains"}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "minItems" check-property-minItems #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "minLength" check-property-minLength #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "minProperties" check-property-minProperties #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "minimum" check-property-minimum-new #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "multipleOf" check-property-multipleOf #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "pattern" check-property-pattern #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "required" check-property-required #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "uniqueItems" check-property-uniqueItems #{}]
     ["https://json-schema.org/draft/2019-09/vocab/validation" :validation "type" check-property-type #{}]])
   :draft2020-12
   (sort-vocabulary
    [["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "additionalItems" check-property-additionalItems #{"items"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "additionalProperties" check-property-additionalProperties #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "allOf" check-property-allOf #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "anyOf" check-property-anyOf #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "contains" check-property-contains #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "dependencies" check-property-dependencies #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "dependentSchemas" check-property-dependentSchemas #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "else" check-property-else #{"if"}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "if" check-property-if #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "items" check-property-items #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "not" check-property-not #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "oneOf" check-property-oneOf #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "patternProperties" check-property-patternProperties #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "prefixItems" check-property-prefixItems #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "properties" check-property-properties #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "propertyDependencies" check-property-propertyDependencies #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "propertyNames" check-property-propertyNames #{}]
     ["https://json-schema.org/draft/2020-12/vocab/applicator" :applicator "then" check-property-then #{"if"}]
     ["https://json-schema.org/draft/2020-12/vocab/content" :content "contentEncoding" (make-check-property-contentEncoding false) #{}]
     ["https://json-schema.org/draft/2020-12/vocab/content" :content "contentMediaType" (make-check-property-contentMediaType false) #{"contentEncoding"}]
     ["https://json-schema.org/draft/2020-12/vocab/content" :content "contentSchema" (make-check-property-contentSchema false) #{"contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft/2020-12/vocab/core" :core "$anchor" check-property-$anchor #{"id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core" :core "$comment" check-property-$comment #{}]
     ["https://json-schema.org/draft/2020-12/vocab/core" :core "$defs" check-property-$defs #{}]
     ["https://json-schema.org/draft/2020-12/vocab/core" :core "$dynamicAnchor" check-property-$dynamicAnchor #{"id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core" :core "$dynamicRef" check-property-$dynamicRef #{"$dynamicAnchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core" :core "$id" check-property-$id #{}]
     ["https://json-schema.org/draft/2020-12/vocab/core" :core "$recursiveAnchor" check-property-$recursiveAnchor #{"id" "$id"}]
     ["https://json-schema.org/draft/2020-12/vocab/core" :core "$recursiveRef" check-property-$recursiveRef #{"$recursiveAnchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core" :core "$ref" check-property-$ref #{"id" "$id" "$anchor"}]
     ["https://json-schema.org/draft/2020-12/vocab/core" :core "$schema" check-property-$schema #{}]
     ["https://json-schema.org/draft/2020-12/vocab/core" :core "$vocabulary" check-property-$vocabulary #{}]
     ["https://json-schema.org/draft/2020-12/vocab/format-annotation" :format-annotation "format" (make-check-property-format false) #{}]
     ["https://json-schema.org/draft/2020-12/vocab/format-assertion" :format-assertion "format" (make-check-property-format true) #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data" :meta-data "default" check-property-default #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data" :meta-data "deprecated" check-property-deprecated #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data" :meta-data "description" check-property-description #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data" :meta-data "examples" check-property-examples #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data" :meta-data "readOnly" check-property-readOnly #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data" :meta-data "title" check-property-title #{}]
     ["https://json-schema.org/draft/2020-12/vocab/meta-data" :meta-data "writeOnly" check-property-writeOnly #{}]
     ["https://json-schema.org/draft/2020-12/vocab/unevaluated" :unevaluated "unevaluatedItems" check-property-unevaluatedItems #{"additionalItems" "uniqueItems" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2020-12/vocab/unevaluated" :unevaluated "unevaluatedProperties" check-property-unevaluatedProperties #{"additionalProperties" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "const" check-property-const #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "dependentRequired" check-property-dependentRequired #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "enum" check-property-enum #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "exclusiveMaximum" check-property-exclusiveMaximum-new #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "exclusiveMinimum" check-property-exclusiveMinimum-new #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "maxContains" check-property-maxContains #{"contains"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "maxItems" check-property-maxItems #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "maxLength" check-property-maxLength #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "maxProperties" check-property-maxProperties #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "maximum" check-property-maximum-new #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "minContains" check-property-minContains #{"contains"}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "minItems" check-property-minItems #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "minLength" check-property-minLength #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "minProperties" check-property-minProperties #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "minimum" check-property-minimum-new #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "multipleOf" check-property-multipleOf #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "pattern" check-property-pattern #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "required" check-property-required #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "uniqueItems" check-property-uniqueItems #{}]
     ["https://json-schema.org/draft/2020-12/vocab/validation" :validation "type" check-property-type #{}]])
   :draft-next
   (sort-vocabulary
    [["https://json-schema.org/draft/next/vocab/applicator" :applicator "additionalItems" check-property-additionalItems #{"items"}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "additionalProperties" check-property-additionalProperties #{"properties" "patternProperties"}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "allOf" check-property-allOf #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "anyOf" check-property-anyOf #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "contains" check-property-contains #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "dependencies" check-property-dependencies #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "dependentSchemas" check-property-dependentSchemas #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "else" check-property-else #{"if"}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "if" check-property-if #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "items" check-property-items #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "not" check-property-not #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "oneOf" check-property-oneOf #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "patternProperties" check-property-patternProperties #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "prefixItems" check-property-prefixItems #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "properties" check-property-properties #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "propertyDependencies" check-property-propertyDependencies #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "propertyNames" check-property-propertyNames #{}]
     ["https://json-schema.org/draft/next/vocab/applicator" :applicator "then" check-property-then #{"if"}]
     ["https://json-schema.org/draft/next/vocab/content" :content "contentEncoding" (make-check-property-contentEncoding false) #{}]
     ["https://json-schema.org/draft/next/vocab/content" :content "contentMediaType" (make-check-property-contentMediaType false) #{"contentEncoding"}]
     ["https://json-schema.org/draft/next/vocab/content" :content "contentSchema" (make-check-property-contentSchema false) #{"contentEncoding" "contentMediaType"}]
     ["https://json-schema.org/draft/next/vocab/core" :core "$anchor" check-property-$anchor #{"id" "$id"}]
     ["https://json-schema.org/draft/next/vocab/core" :core "$comment" check-property-$comment #{}]
     ["https://json-schema.org/draft/next/vocab/core" :core "$defs" check-property-$defs #{}]
     ["https://json-schema.org/draft/next/vocab/core" :core "$dynamicAnchor" check-property-$dynamicAnchor #{"id" "$id"}]
     ["https://json-schema.org/draft/next/vocab/core" :core "$dynamicRef" check-property-$dynamicRef #{"$dynamicAnchor"}]
     ["https://json-schema.org/draft/next/vocab/core" :core "$id" check-property-$id #{}]
     ["https://json-schema.org/draft/next/vocab/core" :core "$recursiveAnchor" check-property-$recursiveAnchor #{"id" "$id"}]
     ["https://json-schema.org/draft/next/vocab/core" :core "$recursiveRef" check-property-$recursiveRef #{"$recursiveAnchor"}]
     ["https://json-schema.org/draft/next/vocab/core" :core "$ref" check-property-$ref #{"id" "$id" "$anchor"}]
     ["https://json-schema.org/draft/next/vocab/core" :core "$schema" check-property-$schema #{}]
     ["https://json-schema.org/draft/next/vocab/core" :core "$vocabulary" check-property-$vocabulary #{}]
     ["https://json-schema.org/draft/next/vocab/format-annotation" :format-annotation "format" (make-check-property-format false) #{}]
     ["https://json-schema.org/draft/next/vocab/format-assertion" :format-assertion "format" (make-check-property-format true) #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data" :meta-data "default" check-property-default #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data" :meta-data "deprecated" check-property-deprecated #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data" :meta-data "description" check-property-description #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data" :meta-data "examples" check-property-examples #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data" :meta-data "readOnly" check-property-readOnly #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data" :meta-data "title" check-property-title #{}]
     ["https://json-schema.org/draft/next/vocab/meta-data" :meta-data "writeOnly" check-property-writeOnly #{}]
     ["https://json-schema.org/draft/next/vocab/unevaluated" :unevaluated "unevaluatedItems" check-property-unevaluatedItems #{"additionalItems" "uniqueItems" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/next/vocab/unevaluated" :unevaluated "unevaluatedProperties" check-property-unevaluatedProperties #{"additionalProperties" "oneOf" "anyOf" "allOf" "not" "if" "then" "else"}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "const" check-property-const #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "dependentRequired" check-property-dependentRequired #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "enum" check-property-enum #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "exclusiveMaximum" check-property-exclusiveMaximum-new #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "exclusiveMinimum" check-property-exclusiveMinimum-new #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "maxContains" check-property-maxContains #{"contains"}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "maxItems" check-property-maxItems #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "maxLength" check-property-maxLength #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "maxProperties" check-property-maxProperties #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "maximum" check-property-maximum-new #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "minContains" check-property-minContains #{"contains"}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "minItems" check-property-minItems #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "minLength" check-property-minLength #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "minProperties" check-property-minProperties #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "minimum" check-property-minimum-new #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "multipleOf" check-property-multipleOf #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "pattern" check-property-pattern #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "required" check-property-required #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "uniqueItems" check-property-uniqueItems #{}]
     ["https://json-schema.org/draft/next/vocab/validation" :validation "type" check-property-type #{}]])})

(def draft->default-$vocabulary
  (map-values
   (fn [v]
     (into {} (map (fn [v] [v true]) (distinct (map first v)))))
   draft->vocab-and-group-and-property-and-semantics))

(defn make-dialect-2 [d v->b]
  (reduce
   (fn [acc [v g p f]]
     (let [b (v->b v)]
       (if (nil? b)
         acc
         (conj acc [p f]))))
   []
   ;; N.B. important to preserve evaluation order of vocabulary
   ;; properties...
   (draft->vocab-and-group-and-property-and-semantics d)))

;; lets define a dialect as a function that given an m2 will return
;; you a correctly ordered sequence of pairs of m2-kv and
;; property-checker

;; should really convert strings to uris...
(defn new-make-dialect-2 [d v->b]
  ;;(prn "NEW-MAKE-DIALECT:" d v->b)

  (fn [m2]
    ;; Guard: only process maps (not booleans true/false)
    (when (map? m2)
      (let [r ((partial
                (make-stable-sort-by
                 third
                 (filter
                  (comp (into #{} (keys v->b)) first)
                  (draft->vocab-and-group-and-property-and-semantics d)))
                first
                (juxt first (comp fourth second)))

               m2)]

        (when (nil? r)
          (prn "GRRR...:" d v->b m2))

        r))))

(def make-dialect (memoize make-dialect-2))
(def new-make-dialect (memoize new-make-dialect-2))

