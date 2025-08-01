# m3

[![CI](https://github.com/JulesGosnell/m3/actions/workflows/ci.yml/badge.svg)](https://github.com/JulesGosnell/m3/actions/workflows/ci.yml)
[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.jules_gosnell/m3.svg)](https://clojars.org/org.clojars.jules_gosnell/m3)

This is the m3 project - a pure Clojure (CLJ/CLJS) JSON validator.

It is based upon a validator for a subset of JSON which I wrote for [Agora Digital Capital Markets](https://agoradcm.com/), and which they have kindly donated to the project - my thanks to them for allowing me to share this code.

I aspire to it becoming a complete and fully featured JSON validator which can be run and produce identical[^1] results in both the backend and frontend of any Clojure[Script] application including errors expressed as native types for easy integration.

It is tested against [JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite). There are still a few holes in fn-ality (disallow and extends in draft3 and a couple of the more esoteric reference types have failing tests) - I hope to plug these soon.

Here is a table of M3's current [features](https://julesgosnell.github.io/m3/features.html) which should allow you to see whether we support the functionality that your project requires.

I'll cut an initial release once I have a few more tests nailed down.

If you find this useful or interesting and would like to get involved, please give me a shout.


To checkout:

```
git clone  --recursive git@github.com:JulesGosnell/m3.git
cd m3
```

To test on clj:
```
lein test
```

To test on cljs:
```
npm install source-map-support --save-dev
npm install big.js
lein test-cljs
```

Starting a ClojureScript REPL:


start shadow server (note server port number)

```
jules@fedora:~/src/m3$ lein with-profile dev shadow watch :dev
lein-shadow - running: npm --version
lein-shadow - found existing package.json file at /home/jules/src/m3/package.json
lein-shadow - reading node dependencies from project.clj
lein-shadow - running: npm ci
lein-shadow - node packages not managed, skipping node package manager installs
lein-shadow - running shadow-cljs...
shadow-cljs - server version: 2.28.21 running at http://localhost:9630
shadow-cljs - nREPL server started on port 36997
shadow-cljs - watching build :dev
[:dev] Configuring build.
[:dev] Compiling ...
[:dev] Build completed. (87 files, 86 compiled, 0 warnings, 4.40s)
```

install websocket library (1st time)

```
jules@fedora:~/src/m3$ npm install ws

added 1 package, and audited 7 packages in 1s

1 package is looking for funding
  run `npm fund` for details

found 0 vulnerabilities
jules@fedora:~/src/m3$ 
```

run up node repl backend

```
jules@fedora:~/src/m3$ node ./target/node/repl.js 
Hello from ClojureScript Node REPL with shadow-cljs!
shadow-cljs - #3 ready!
```

in emacs (port number is taken from shadow server output above)

```
M-x cider-connect-cljs
Host: localhost
Port for localhost: 36997
Select ClojureScript REPL type: shadow
Select shadow-cljs build: dev
```
you then should see this in the repl buffer:

```
;; ClojureScript REPL type: shadow
;; ClojureScript REPL init form: (do (require '[shadow.cljs.devtools.api :as shadow]) (shadow/watch :dev) (shadow/nrepl-select :dev))
;;
To quit, type: :cljs/quit
[:selected :dev]shadow.user> 
cljs.user>
```

congratulations, you are ready to repl...

(it must be easier than this, surely ?)



Here are some simple usage examples:

Usage

```
m3.validate> (ns m3.validate)
nil
m3.validate> ;; (validate schema-context schema document-context document)
```

A successful validation - string

```
m3.validate> (validate {} {"type" "string"} {} "hello")
{:valid? true, :errors nil}
m3.validate> 
```

A failed validation

```
m3.validate> (validate {} {"type" "string"} {} 0)
{:valid? false,
 :errors
 [{:schema-path [],
   :message "schema: document did not conform - 0",
   :document-path [],
   :document 0,
   :schema {"type" "string"},
   :errors
   [{:schema-path ["type"],
     :message "type: not a string - 0",
     :document-path [],
     :document 0,
     :schema {"type" "string"}}]}]}
m3.validate> 
```

A successful validation - array

```
m3.validate> (validate {} {"type" "array" "items" {"type" "string"}} {} ["hello" "goodbye"])
{:valid? true, :errors nil}
m3.validate> 
```

A failed validation - array

```
m3.validate> (validate {} {"type" "array" "items" {"type" "string"}} {} ["hello" 0])
{:valid? false,
 :errors
 [{:schema-path [],
   :message "schema: document did not conform - [\"hello\" 0]",
   :document-path [],
   :document ["hello" 0],
   :schema {"type" "array", "items" {"type" "string"}},
   :errors
   [{:schema-path ["items"],
     :message
     "items: at least one item did not conform to schema - [\"hello\" 0]",
     :document-path [],
     :document ["hello" 0],
     :schema {"type" "array", "items" {"type" "string"}},
     :errors
     [{:schema-path ["items"],
       :message "schema: document did not conform - 0",
       :document-path [1],
       :document 0,
       :schema {"type" "string"},
       :errors
       [{:schema-path ["items" "type"],
         :message "type: not a string - 0",
         :document-path [1],
         :document 0,
         :schema {"type" "string"}}]}]}]}]}
m3.validate> 
```

A failed validation - format

```
m3.validate> (validate {} {"type" "string" "format" "date"} {} "2025/01/01")
{:valid? false,
 :errors
 [{:schema-path [],
   :message "schema: document did not conform - \"2025/01/01\"",
   :document-path [],
   :document "2025/01/01",
   :schema {"type" "string", "format" "date"},
   :errors
   [{:schema-path ["format"],
     :message
     "format: not a valid date: \"2025/01/01\" - Text '2025/01/01' could not be parsed at index 4 - \"2025/01/01\"",
     :document-path [],
     :document "2025/01/01",
     :schema {"type" "string", "format" "date"}}]}]}
m3.validate> 
```

A successful validation - suppression of format checking

```
m3.validate> (validate {:strict-format? false} {"type" "string" "format" "date"} {} "2025/01/01")
[nREPL-session-2ae0106f-f0da-4741-ac30-6dc3a1fe61e1] WARN m3.validate - format: not a valid date: "2025/01/01" - Text '2025/01/01' could not be parsed at index 4 - "2025/01/01"
{:valid? true, :errors nil}
m3.validate> 
```

A successful validation - format

```
m3.validate> (validate {} {"type" "string" "format" "date"} {} "2025-01-01")
{:valid? true, :errors nil}
m3.validate> 
```

A successful validation - oneOf - with tracing enabled

```
m3.validate> (validate {:trace? true} {"oneOf" [{"type" "string" "format" "date"} {"type" "integer"} {"type" "array"} {"type" "boolean"}]} {} false)
["oneOf" 0 "type"] [] [❌ type: not a string - false]
["oneOf" 1 "type"] [] [❌ type: not an integer - false]
["oneOf" 2 "type"] [] [❌ type: not an array - false]
["oneOf" 3 "type"] [] ✅
["oneOf"] [] ✅
{:valid? true, :errors nil}
m3.validate> 
```

[^1]Limitations:

Java and JavaScript treat numbers very differently and this behaviour is inherited by other languages built upon those platforms.

```
clj.user> (= 1 1.0)
false
clj.user> 
```

vs

```
cljs.user> (= 1 1.0)
true
cljs.user> 
```

This means that there will be edge cases which will validate in CLJS but not CLJ and vice-versa.
I could add a layer of my own number types on top of CLJS to try to get it to behave like CLJ but I think this would intrude too heavily on the user's JSON docs and violate the principle of least surprise. So, unless anyone can suggest a better solution, I am going to live with the JS number stack and the inconsistency between platforms for the present.


To develop with Claude

- run `./bin/nrepl.sh` in a terminal
- run `./mcp-claude.sh` in another terminal
- run claude-desktop (https://claude.ai/download, https://github.com/bsneed/claude-desktop-fedora) in a third terminal

I found that Claude seemed to understand the code and was fun to talk to but soon got lost in refactoring and didn't create anything useful


To develop with Grok [4] (WIP)

- run `./bin/nrepl.sh` in a terminal
- run `./bin/mcp-grok.sh` in another terminal
- install MCP SuperAssistant plugin in Chrome
- login in to x.com or grok.com

Currently, MCP button fails to show on Grok page although extension seems to be talking to local proxy very happily. I've opened an issue on github (https://github.com/srbhptl39/MCP-SuperAssistant)


