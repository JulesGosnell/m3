(ns m3.mcp-runner
  (:require [clojure-mcp.main :as mcp]))

(defn -main [& args]
  (let [port (if (seq args) (Integer/parseInt (first args)) 7888)]
    (mcp/start-mcp-server {:port port})))
