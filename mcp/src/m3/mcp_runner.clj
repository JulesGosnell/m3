(ns m3.mcp-runner
  (:require
   [nrepl.server :refer [start-server]]
   [clojure-mcp.main :as mcp]))

;; integrate nrepl and clojure-mcp in-vm together so that we:
;; a) only have to start a single process
;; b) don't have to be explicit about a port
;; c) avoid an annoying race condition at startup

(defn -main []
  (let [{p :port} (start-server)]
    (prn "connecting clojure-map to nrepl on port:" p)
    (mcp/start-mcp-server {:port p})))
