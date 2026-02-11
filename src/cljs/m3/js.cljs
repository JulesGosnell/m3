(ns m3.js
  "JavaScript entry point for M3 JSON Schema Validator.
   Exports validate function for use from Node.js/browser."
  (:require
   [m3.platform :refer [deep-js->clj]]
   [m3.validate :as v]))

(def ^:private path (js/require "path"))

(def ^:private draft-strings
  {"draft3"       :draft3
   "draft4"       :draft4
   "draft6"       :draft6
   "draft7"       :draft7
   "draft2019-09" :draft2019-09
   "draft2020-12" :draft2020-12
   "draft-next"   :draft-next})

;; Resolve schema resource paths relative to the npm package root.
;; __dirname is the npm-dist directory; schemas are bundled in npm-dist/schemas/
(def ^:private schemas-dir
  (.resolve path js/__dirname "schemas"))

;; Override the global uri-base->dir so $schema resolution uses absolute paths.
;; This makes the npm module work regardless of the user's cwd.
(set! v/uri-base->dir
  {"http://json-schema.org" schemas-dir
   "https://json-schema.org" schemas-dir})

;; Also rebuild uri->schema* which is a partial over uri-base->dir
(set! v/uri->schema* (partial v/uri->schema v/uri-base->dir))

(def ^:private npm-uri->schema
  (v/uri->continuation v/uri-base->dir))

(defn- convert-errors
  "Recursively convert Clojure error tree to plain JS objects."
  [v]
  (cond
    (map? v)
    (let [obj #js {}]
      (doseq [[k val] v]
        (let [js-key (case k
                       :schema-path   "schemaPath"
                       :document-path "documentPath"
                       :message       "message"
                       :document      "document"
                       :schema        "schema"
                       :errors        "errors"
                       :valid?        "valid"
                       (if (keyword? k) (name k) (str k)))]
          (aset obj js-key (convert-errors val))))
      obj)

    (vector? v)
    (to-array (mapv convert-errors v))

    (seq? v)
    (to-array (mapv convert-errors v))

    (keyword? v)
    (name v)

    :else v))

(defn- js-opts->clj-opts [opts]
  (when opts
    (let [opts-clj (deep-js->clj opts)]
      (cond-> {:draft (or (draft-strings (get opts-clj "draft")) :draft2020-12)}
        (get opts-clj "strictFormat")  (assoc :strict-format? true)
        (get opts-clj "strictInteger") (assoc :strict-integer? true)))))

(defn ^:export validate
  "Validate a JSON document against a JSON Schema.
   Returns {valid: boolean, errors: Array|null}.

   Usage:
     const {validate} = require('m3-json-schema');
     validate({type: 'string'}, 'hello');        // {valid: true, errors: null}
     validate({type: 'number'}, 'not a number'); // {valid: false, errors: [...]}"
  ([schema document]
   (validate schema document nil))
  ([schema document opts]
   (let [schema-clj  (deep-js->clj schema)
         document-clj (deep-js->clj document)
         clj-opts (or (js-opts->clj-opts opts)
                      {:draft :draft2020-12})
         c2 (cond-> {:quiet? true
                      :draft (:draft clj-opts)
                      :uri->schema npm-uri->schema}
              (:strict-format? clj-opts)  (assoc :strict-format? true)
              (:strict-integer? clj-opts) (assoc :strict-integer? true))
         result (v/validate c2 schema-clj {} document-clj)]
     (convert-errors result))))

(defn ^:export validator
  "Compile a schema and return a reusable validation function.
   More efficient for validating many documents against one schema.

   Usage:
     const {validator} = require('m3-json-schema');
     const v = validator({type: 'string', minLength: 1});
     v('hello');  // {valid: true, errors: null}
     v('');       // {valid: false, errors: [...]}"
  ([schema]
   (validator schema nil))
  ([schema opts]
   (let [schema-clj (deep-js->clj schema)
         clj-opts (or (js-opts->clj-opts opts)
                      {:draft :draft2020-12})
         c2 (cond-> {:quiet? true
                      :draft (:draft clj-opts)
                      :uri->schema npm-uri->schema}
              (:strict-format? clj-opts)  (assoc :strict-format? true)
              (:strict-integer? clj-opts) (assoc :strict-integer? true))
         f (v/validate-m2 c2 schema-clj)]
     (fn [document]
       (let [document-clj (deep-js->clj document)
             result (v/reformat (f {} document-clj))]
         (convert-errors result))))))
