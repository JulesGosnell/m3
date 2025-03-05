(ns m3.repl
  (:require [cljs.nodejs :as nodejs]))

;; Enable println to work with Node.js
(nodejs/enable-util-print!)

;; Main function required for Node.js entry point
(defn -main [& args]
  (println "Hello from ClojureScript Node REPL with shadow-cljs!"))

;; Set the main function as the entry point
(set! *main-cli-fn* -main)
