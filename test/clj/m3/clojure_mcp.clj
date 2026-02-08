(ns m3.clojure-mcp
  (:require
   [nrepl.server :refer [start-server] :rename {start-server start-nrepl-server}]))

;; integrate nrepl and clojure-mcp in-vm together so that we:
;; a) only have to start a single process
;; b) don't have to be explicit about a port
;; c) avoid an annoying race condition at startup
;;
;; clojure-mcp is resolved at runtime (not a Lein dependency);
;; it's added to the classpath by bin/mcp-clojure-tools.sh

(defn -main []
  (let [{p :port} (start-nrepl-server)]
    (println "connecting clojure-mcp to nrepl on port:" p)
    ((requiring-resolve 'clojure-mcp.main/start-mcp-server) {:port p})))
