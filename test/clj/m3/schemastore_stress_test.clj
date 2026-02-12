;; Stress-test: compile real-world schemas from SchemaStore and json-schema-corpus
;;
;; Usage:
;;   lein test m3.schemastore-stress-test
;;
;; Prerequisites (auto-cloned if missing):
;;   /tmp/schemastore          — https://github.com/SchemaStore/schemastore
;;   /tmp/json-schema-corpus   — https://github.com/sdbs-uni-p/json-schema-corpus

(ns m3.schemastore-stress-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [m3.json-schema :as m3])
  (:import [java.io File]))

(defn- ensure-cloned
  "Clone a git repo to target-dir if it doesn't exist. Returns the File."
  ^File [repo-url ^String target-dir]
  (let [dir (io/file target-dir)]
    (when-not (.isDirectory dir)
      (println (format "Cloning %s → %s ..." repo-url target-dir))
      (let [p (.start (ProcessBuilder.
                       ["git" "clone" "--depth" "1" repo-url target-dir]))]
        (when-not (zero? (.waitFor p))
          (throw (ex-info "git clone failed" {:repo repo-url :dir target-dir})))))
    dir))

(defn- json-files
  "List all .json files under dir (non-recursive)."
  [^File dir]
  (->> (.listFiles dir)
       (filter #(.endsWith (.getName ^File %) ".json"))
       (sort-by #(.getName ^File %))
       vec))

(defn- try-compile
  "Try to compile and validate a trivial document against a schema.
   Returns {:ok true :file name} or {:error msg :file name}."
  [^File schema-file]
  (let [fname (.getName schema-file)]
    (try
      (let [schema (with-open [r (io/reader schema-file)]
                     (json/read r))
            ;; Compile + validate — exercises memoized compile path
            _ (m3/validate schema {} {:quiet? true})
            ;; Validate again — exercises memo cache hit
            _ (m3/validate schema {"x" 1} {:quiet? true})]
        {:ok true :file fname})
      (catch StackOverflowError _
        {:error "StackOverflow" :file fname})
      (catch Exception e
        {:error (.getMessage e) :file fname}))))

(defn- run-collection
  "Compile all JSON schemas in dir. Returns {:total :ok :errors :error-list}."
  [^File dir label]
  (let [files (json-files dir)
        results (pmap try-compile files)
        errors (filterv :error results)
        ok-count (count (filter :ok results))]
    (println (format "\n%s: %d schemas, %d compiled OK, %d errors"
                     label (count results) ok-count (count errors)))
    (when (seq errors)
      (println "\nError categories:")
      (doseq [[msg cnt] (->> errors (map :error) frequencies (sort-by val) reverse)]
        (println (format "  %4d  %s" cnt msg)))
      (println "\nFirst 20 errors:")
      (doseq [{:keys [file error]} (take 20 errors)]
        (println (format "  %-55s %s" file error)))
      (when (> (count errors) 20)
        (println (format "  ... and %d more" (- (count errors) 20)))))
    (println (format "Error rate: %.1f%%" (* 100.0 (/ (count errors) (max 1 (count results))))))
    {:total (count results) :ok ok-count :errors (count errors) :error-list errors}))

;;------------------------------------------------------------------------------

(deftest schemastore-compile-test
  (let [dir (ensure-cloned "https://github.com/SchemaStore/schemastore" "/tmp/schemastore")
        schema-dir (io/file dir "src/schemas/json")]
    (if-not (.isDirectory schema-dir)
      (println "SKIP: SchemaStore schemas not found")
      (let [{:keys [ok errors]} (run-collection schema-dir "SchemaStore")]
        (is (pos? ok) "At least some SchemaStore schemas should compile")
        (is (zero? errors) "All SchemaStore schemas should compile")))))

(deftest corpus-compile-test
  (let [dir (ensure-cloned "https://github.com/sdbs-uni-p/json-schema-corpus" "/tmp/json-schema-corpus")
        schema-dir (io/file dir "json_schema_corpus")]
    (if-not (.isDirectory schema-dir)
      (println "SKIP: json-schema-corpus not found")
      (let [{:keys [ok errors]} (run-collection schema-dir "json-schema-corpus")]
        (is (pos? ok) "At least some corpus schemas should compile")
        ;; Don't assert zero errors — wild schemas may use non-standard features
        (println (format "\nCorpus: %d/%d successful (%.1f%%)"
                         ok (+ ok errors) (* 100.0 (/ ok (max 1 (+ ok errors))))))))))
