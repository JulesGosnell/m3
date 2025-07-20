#!/bin/sh

cd "$(dirname "$0")/.."

npx @srbhptl39/mcp-superassistant-proxy@latest --config ./mcp/config/grok.json
