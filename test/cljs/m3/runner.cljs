(ns m3.runner
  (:require
   [cljs.test :refer-macros [run-tests]]
   [m3.uri-test]))

(enable-console-print!)

;; Initialize shadow-cljs test environment
(defonce test-data (test-env/get-test-data))

;; Run all discovered test namespaces
(let [test-namespaces (keys (:ns-registrations @test-data))]
  (doseq [ns test-namespaces]
    (println "Running tests in:" ns))
  (apply run-tests test-namespaces))
