#!/bin/sh

cd "$(dirname "$0")/.."

# export PORT=7888

# while ! nc -z localhost $PORT
# do
#     echo -n "."
#     sleep 1
# done

# echo

# sleep 10

lein with-profile +mcp run -m m3.mcp-runner 7888

