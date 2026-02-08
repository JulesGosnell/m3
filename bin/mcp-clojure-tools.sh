#!/bin/sh

cd "$(dirname "$0")/.."

# clojure-mcp is a git dep (not on Maven/Clojars), so we compute its
# classpath via Clojure CLI and merge it with the Lein project classpath.

MCP_CP=$(clojure -Sdeps '{:deps {io.github.bhauman/clojure-mcp {:git/tag "v0.2.3" :git/sha "bbefc7a"}}}' -Spath)
M3_CP=$(lein classpath)

java -cp "$M3_CP:$MCP_CP" clojure.main -m m3.clojure-mcp
