#!/bin/sh

cd "$(dirname "$0")/.."

lein with-profile +mcp run -m m3.clojure-mcp

