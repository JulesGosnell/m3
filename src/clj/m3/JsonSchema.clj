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

(ns m3.JsonSchema
  "Java-friendly facade for M3 JSON Schema Validator.
   Exposes static methods that accept and return java.util.Map/List.

   Java usage:
     import m3.JsonSchema;
     Map result = JsonSchema.validate(\"{\\\"type\\\":\\\"string\\\"}\", \"\\\"hello\\\"\");
     boolean valid = (boolean) result.get(\"valid\");

   Zero-copy from Jackson:
     ObjectMapper mapper = new ObjectMapper();
     Map schema = mapper.readValue(schemaJson, LinkedHashMap.class);
     Object document = mapper.readValue(docJson, Object.class);
     Map result = JsonSchema.validate(schema, document);"
  (:require
   [m3.json-schema :as api]
   [m3.util :refer [convert-output]])
  (:gen-class
   :name m3.JsonSchema
   :methods [^:static [validate [String String] java.util.Map]
             ^:static [validate [String String java.util.Map] java.util.Map]
             ^:static [validate [java.util.Map Object] java.util.Map]
             ^:static [validate [java.util.Map Object java.util.Map] java.util.Map]]))

(def ^:private java-output-fns
  {:make-map (fn [& kvs]
               (let [m (java.util.LinkedHashMap.)]
                 (doseq [[k v] (partition 2 kvs)]
                   (.put m k v))
                 m))
   :make-vec (fn [coll] (java.util.ArrayList. ^java.util.Collection coll))
   :make-kw  identity})

(defn- java-opts->clj-opts [^java.util.Map opts]
  (when opts
    (cond-> {}
      (.get opts "draft")          (assoc :draft (keyword (.get opts "draft")))
      (.get opts "strictFormat")   (assoc :strict-format? true)
      (.get opts "strictInteger")  (assoc :strict-integer? true))))

(defn- result->java [result]
  (convert-output java-output-fns result))

;; gen-class dispatches overloads by Java type signature.
;; All overloads call -validate; we dispatch on argument types.
(defn -validate
  [& args]
  (case (count args)
    2 (let [[schema document] args]
        (if (string? schema)
          ;; (String, String)
          (result->java (api/validate schema document))
          ;; (Map, Object)
          (result->java (api/validate (into {} schema) document))))
    3 (let [[schema document opts] args]
        (if (string? schema)
          ;; (String, String, Map)
          (result->java (api/validate schema document (java-opts->clj-opts opts)))
          ;; (Map, Object, Map)
          (result->java (api/validate (into {} schema) document (java-opts->clj-opts opts)))))))
