;; Simple test runner that collects failures for parallel testing
(ns m3.test-runner
  (:require
   [clojure.test :as test]
   [clojure.java.io :refer [file]]
   [m3.testsuite-test :as suite]))

(defn run-with-collection
  "Run a test function, collecting failures instead of printing"
  [test-fn & args]
  (let [failures (atom [])
        original-report test/report]
    (with-redefs [test/report (fn [m]
                                (when (= (:type m) :fail)
                                  (swap! failures conj
                                         {:message (:message m)
                                          :expected (:expected m)
                                          :actual (:actual m)}))
                                (when (= (:type m) :error)
                                  (swap! failures conj
                                         {:message (str "ERROR: " (:message m))
                                          :actual (:actual m)})))]
      (apply test-fn args)
      @failures)))

(defn run-draft-collected
  "Run tests for a single draft, return failures"
  [draft dir]
  (let [f (file (str suite/json-schema-test-suite-root dir))]
    {:draft draft
     :dir dir
     :failures (run-with-collection suite/test-directory f draft)}))

(defn run-parallel-quiet
  "Run all tests in parallel, report only failures"
  []
  (println "Running tests in parallel...")
  (let [test-configs [[:draft3 "draft3"]
                      [:draft4 "draft4"]
                      [:draft6 "draft6"]
                      [:draft7 "draft7"]
                      [:draft2019-09 "draft2019-09"]
                      [:draft2020-12 "draft2020-12"]
                      [:draft-next "draft-next"]]
        ;; Run in parallel
        results (doall
                 (pmap (fn [[draft dir]]
                         (run-draft-collected draft dir))
                       test-configs))
        ;; Count failures
        total-failures (reduce + (map (comp count :failures) results))]

    (if (zero? total-failures)
      (do
        (println "✓ All tests passed!")
        true)
      (do
        (println (str "\n✗ " total-failures " failures found:\n"))
        (doseq [{:keys [draft dir failures]} results
                :when (seq failures)]
          (println (str "  " draft " (" dir "): " (count failures) " failures"))
          ;; Just show first 3 failures per draft
          (doseq [f (take 3 failures)]
            (println (str "    - " (or (:message f) (:actual f))))))
        false))))

;; Add main function for command line use
(defn -main
  "Main entry point for lein run"
  []
  (let [success? (run-parallel-quiet)]
    (System/exit (if success? 0 1))))
