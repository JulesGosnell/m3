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

(ns m3.json-schema
  "M3 JSON Schema Validator — public API.
   The most complete JSON Schema validator. All drafts 3 through draft-next.

   Quick start:

     (require '[m3.json-schema :as m3])

     ;; Validate a document against a schema
     (m3/validate {\"type\" \"string\"} \"hello\")
     ;; => {:valid? true, :errors nil}

     (m3/validate {\"type\" \"number\"} \"oops\")
     ;; => {:valid? false, :errors [{:schema-path [...] :message \"...\" ...}]}

     ;; Compile once, validate many
     (let [v (m3/validator {\"type\" \"object\" \"required\" [\"id\"]})]
       (v {\"id\" 1})    ;; => {:valid? true, :errors nil}
       (v {}))          ;; => {:valid? false, :errors [...]}

   Supported drafts: :draft3, :draft4, :draft6, :draft7,
                     :draft2019-09, :draft2020-12, :draft-next, :latest
   Default: :latest (currently :draft2020-12)

   Error shape:
     {:schema-path   [\"type\"]        ;; path into the schema that failed
      :document-path [\"name\"]        ;; path into the document that failed
      :message       \"...\"           ;; human-readable description
      :document      ...              ;; the failing document fragment
      :schema        ...              ;; the relevant schema fragment
      :errors        [...]            ;; nested sub-errors (if applicable)}"
  (:require
   [m3.platform :refer [json-decode]]
   [m3.uri :refer [parse-uri uri-base]]
   [m3.validate :as v]))

(defn- registry->uri->schema
  "Build a :uri->schema function from a registry map {uri-string -> schema}.
   Falls back to the default file-based resolution."
  [registry]
  (let [;; Pre-parse registry URIs for fast lookup
        parsed (reduce-kv
                (fn [m uri-str schema]
                  (let [uri (uri-base (parse-uri uri-str))]
                    (assoc m uri schema)))
                {} registry)
        default (v/uri->continuation v/uri-base->dir)]
    (fn [c p uri]
      (if-let [schema (parsed (uri-base uri))]
        (let [c2 (v/make-context (select-keys c [:uri->schema :draft :id-key :quiet?]) schema)]
          [c2 [] schema])
        (default c p uri)))))

(defn- opts->c2 [{:keys [draft strict-format? strict-integer? quiet? registry]}]
  (cond-> {:quiet? true :draft (or draft :latest)}
    strict-format?  (assoc :strict-format? true)
    strict-integer? (assoc :strict-integer? true)
    (some? quiet?)  (assoc :quiet? quiet?)
    registry        (assoc :uri->schema (registry->uri->schema registry))))

(defn validate
  "Validate a document against a JSON Schema.
   Returns {:valid? bool, :errors [...]}

   Both-strings form: schema and document are both JSON strings.
     (validate \"{\\\"type\\\":\\\"string\\\"}\" \"\\\"hello\\\"\")

   Parsed form: schema is a map, document is any Clojure/Java value.
     (validate {\"type\" \"string\"} \"hello\")

   opts - optional map:
     :draft            - :draft3, :draft4, :draft6, :draft7, :draft2019-09, :draft2020-12, :draft-next, :latest
     :strict-format?   - true to treat format as assertion (default: annotation-only)
     :strict-integer?  - true to require actual integers (not 1.0 for integer type)
     :quiet?           - false to enable logging (default: true)
     :registry         - map of URI string to schema, for $ref resolution"
  ([schema document]
   (validate schema document nil))
  ([schema document opts]
   (let [;; If schema is a string, parse both as JSON (the "JSON strings" form).
         ;; If schema is already a map, document is used as-is (the "parsed" form).
         json-strings? (string? schema)
         schema   (if json-strings? (json-decode schema) schema)
         document (if json-strings? (json-decode document) document)
         c2       (opts->c2 (or opts {}))]
     (try
       (v/validate c2 schema {} document)
       (catch #?(:clj StackOverflowError :cljs :default) _
         {:valid? false
          :errors [{:message "StackOverflow: schema contains infinite $ref cycle"
                    :schema-path []
                    :document-path []
                    :document document
                    :schema schema}]})))))

(defn validator
  "Compile a schema, return a reusable validation function.
   The returned function takes a document (any value) and
   returns {:valid? bool, :errors [...]}.
   More efficient for validating many documents against one schema.

   schema - map (parsed schema) or JSON string
   opts   - same as validate."
  ([schema]
   (validator schema nil))
  ([schema opts]
   (let [schema (if (string? schema) (json-decode schema) schema)
         c2     (opts->c2 (or opts {}))
         f      (v/validate-m2 c2 schema)]
     (fn [document]
       (try
         (v/reformat (f {} document))
         (catch #?(:clj StackOverflowError :cljs :default) _
           {:valid? false
            :errors [{:message "StackOverflow: schema contains infinite $ref cycle"
                      :schema-path []
                      :document-path []
                      :document document
                      :schema schema}]}))))))
