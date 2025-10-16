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

- put something like this (fixup the path to mcp-claude.sh) in your ~/.config/Claude/claude_desktop_config.json:
```
{
  "mcpServers": {
    "clojure-mcp": {
        "command": "~/src/m3/bin/mcp-claude.sh"
    }
  }
}
```
- run `./bin/nrepl.sh` in a terminal
- run claude-desktop (https://claude.ai/download, https://github.com/bsneed/claude-desktop-fedora) in a third terminal


To develop with Grok [4] (WIP)

- run `./bin/nrepl.sh` in a terminal
- run `./bin/mcp-grok.sh` in another terminal
- install MCP SuperAssistant plugin in Chrome
- login in to x.com or grok.com

Currently, MCP button fails to show on Grok page although extension seems to be talking to local proxy very happily. I've opened an issue on github (https://github.com/srbhptl39/MCP-SuperAssistant)


---

## AI Project Summary

This section provides architectural context for AI assistants working on m3.

### Philosophy

**m3** aims to be a small, fast, completely extensible JSON Schema validator that produces identical results across Clojure and ClojureScript platforms. The design philosophy emphasizes:

- **Pure functional programming** - No mutation, all validation state threaded through contexts
- **Data-driven architecture** - Vocabularies, dialects, and property checkers are all data structures
- **Complete extensibility** - Every aspect (types, formats, properties) can be extended or replaced
- **Cross-platform parity** - Maximum compatibility between JVM and JavaScript runtimes
- **Separation of concerns** - Schema compilation (L2) separate from runtime validation (L1)

### Design

#### Core Architecture

**L2/L1 Curried Pattern**: The foundation of m3's design. Each property checker is a curried function:
- **Level 2 (L2)**: Compilation time - processes the schema (m2), returns a validation function
- **Level 1 (L1)**: Runtime - the returned function validates documents (m1)
- **Signature**: `(fn [property c2 p2 m2 v2] [c2 m2 (fn [c1 p1 m1] [c1 m1 errors])])`
- **Benefits**: Schema compilation can be cached, validation is fast, context flows cleanly

**Context Threading**: Two contexts flow through validation:
- **c2**: Compilation context - contains dialect, draft, uri->schema, uri->draft, etc.
- **c1**: Runtime context - accumulates validation state, errors, markers
- Contexts are immutable and threaded through all validation steps

**Vocabulary System**: JSON Schema validation is defined by vocabularies (`src/cljc/m3/vocabulary.cljc`):
- Historical accuracy: Each draft (03, 04, 06, 07, 2019-09, 2020-12, next) has its exact vocabulary
- 71 property checkers organized by vocabulary membership
- Properties have dependencies (e.g., `then` depends on `if`)
- Topological sorting ensures properties validate in correct order
- Dialects compose vocabularies into validation functions

**Dialect Switching**: Solved using loop/recur in `compile-m2` (`src/cljc/m3/validate.cljc`):
- `compile-m2` iterates through schema properties using dialect
- When `check-property-$schema` runs, it updates `:dialect` in c2
- Loop detects dialect change: `(identical? old-dialect new-dialect)`
- On change, re-evaluates `(new-dialect m2)` to get fresh property list
- Tracks `processed-keys` set to avoid re-processing properties
- Enables schemas to switch JSON Schema drafts mid-validation

#### Key Files

- **validate.cljc** (1250+ lines): Core validation engine, `compile-m2`, context management
- **property.cljc** (700+ lines): All 71 property checkers following L2/L1 pattern
- **vocabulary.cljc** (400+ lines): Vocabulary tables, dialect composition, topological sorting
- **ref.cljc**: Reference resolution ($ref, $recursiveRef, $dynamicRef)
- **type.cljc**: JSON type checking
- **format.cljc**: Format validators with platform-specific implementations
- **uri.cljc**: URI parsing and resolution

#### Current Implementation Status

**Working** (as of October 2025):
- ✅ Dialect switching via loop/recur with processed-keys tracking
- ✅ `check-property-$schema`: Parses URI, loads metaschema, extracts $vocabulary, builds dialect, updates c2
- ✅ `check-property-$vocabulary`: Builds custom dialect from vocabulary map, updates c2
- ✅ All property checkers migrated to new L2/L1 protocol
- ✅ Cross-platform testing: 24,021 assertions passing (CLJ and CLJS)
- ✅ Official JSON Schema Test Suite integration

**Test Baseline**: 
- Clojure: 24,021 assertions, 0 failures, 0 errors
- ClojureScript: 24,021 assertions, 0 failures, 0 errors

### Issues

Current known issues that need attention:

1. **Dialect Propagation**: Dialect has to be repeatedly defaulted instead of flowing naturally down validation chain. It should only change when encountering `$schema` in m2. (HIGH PRIORITY)

2. **Context Threading After Migration**: Recent migration of interceptors to property checkers may have introduced c2/c1 threading bugs. Systematic review needed.

3. **$ref m2 Mutation**: `check-property-$ref` (and other ref types) can return a different m2 than received (schema replacement). After each property checker call, must check `(identical? old-m2 new-m2)` and re-run `(dialect new-m2)` if stale. Requires changes to `compile-m2`. (CRITICAL)

4. **Missing :draft in Errors**: Error objects don't contain `:draft` field, which would help debugging.

5. **Draft-03 Required Property**: In draft-03, "required" lives within property definitions. Need to pre-process properties to gather required info, then pass through validation levels (similar to marker stash strategy).

6. **Circular Dependency**: Property checkers need to recursively call `check-schema` from validate.cljc, but validate.cljc depends on property.cljc. Currently using `(deref (resolve 'm3.validate/check-schema))` pattern. Should pass check-schema (or dialect) through c2 context instead.

### Future Directions

#### Near Term (Next Steps)

1. **Fix Dialect Propagation** - Make dialect flow naturally, only changing on `$schema`
2. **Review Context Threading** - Audit all property checkers for correct c2/c1 threading
3. **Handle $ref m2 Mutation** - Implement `(identical? old-m2 new-m2)` check and dialect re-evaluation
4. **Add :draft to Errors** - Include draft information in all error objects
5. **Implement Draft-03 Required** - Pre-pass strategy for property-level required

#### Medium Term

1. **Complete Circular Dependency Cleanup** - Pass check-schema/dialect through c2
2. **Marker Collection Strategy** - Recursive schema hierarchy traversal with marker stashes (ids, anchors) passed via c1 → c2 transfer
3. **Reference Resolution** - Fix $ref, $recursiveRef, $dynamicRef with proper marker stashes

#### Long Term

1. **Performance Optimization**:
   - Leverage topological sorting for better performance
   - Implement memoization strategies for schema compilation
   - Optimize hot paths in validation

2. **Feature Completion**:
   - Complete draft-03 support (disallow, extends)
   - Handle esoteric reference types
   - Full $vocabulary composition system

3. **Documentation**:
   - Update examples (likely stale)
   - Document extension points
   - Add architectural diagrams

#### Development Notes for AI Assistants

**Testing**: Always run tests after changes:
```bash
lein test              # Clojure tests (~10 seconds, 24k+ assertions)
lein test-cljs         # ClojureScript tests  
```

**MCP Integration**: Project uses clojure-mcp for MCP access via Claude Desktop. Available tools: file operations, REPL evaluation, bash commands, grep/glob, agents.

**Code Review Required**: Never commit without user review. Present changes with summary, rationale, and trade-offs. Wait for approval.

**Platform Differences**: Be careful with number handling (1 != 1.0 in CLJ but == in CLJS), regex engines, date/time, and base64 encoding differences.

**Key Functions to Know**:
- `compile-m2`: Core schema compilation (loop/recur with dialect switching)
- `check-schema`: Main validation entry point
- `make-dialect`: Composes vocabularies into validation function
- `draft->vocab-and-group-and-property-and-semantics`: Vocabulary definition table

### Session Startup Protocol

When starting a new session, AI assistants should perform these verification steps:

1. **Verify MCP Connections**:
   - Test clojure-tools, clojure-language-server, and emacs MCP connections
   - If any fail, notify user immediately

2. **Check Tool Versions**:
   - Clojure CLI: `clojure --version` (ensure latest)
   - Clojure LSP: `clojure-lsp --version` (ensure latest)
   - Leiningen: `lein version`
   - Java: `java -version`

3. **Run Project Tests**:
   - `lein test` (CLJ) - should complete in ~10 seconds
   - `lein test-cljs` (CLJS) - may take longer
   - Verify baseline: 24,021 assertions, 0 failures, 0 errors

4. **Check Project Status**:
   - Recent commits: `git log --oneline -10`
   - Uncommitted changes: `git status`
   - Dependency updates: `lein ancient`
   - GitHub Actions: https://github.com/JulesGosnell/m3/actions

### Code Changes Process

**CRITICAL**: Follow this process for all code modifications:

**Before Making Changes**:
- Understand existing code structure and patterns
- Review related test files for expected behavior
- Consider impact on both CLJ and CLJS platforms

**After Making Changes**:
- Run tests: `lein test` and `lein test-cljs`
- Verify tests still pass with same assertion count
- Check for any new warnings in test output

**CODE REVIEW REQUIRED**:
- **DO NOT COMMIT** without user review
- Present changes to user with:
  * Summary of what was changed
  * Rationale for the approach taken
  * Any trade-offs or alternatives considered
- Wait for user approval or feedback
- Be prepared to revise based on user input

**After Review Approval**:
- Only then use `git add` and `git commit`
- Write clear, detailed commit messages explaining the why, not just the what
- Reference any relevant issue numbers or prior commits

**New Files**:
- When creating a new code file, copy copyright notice from an existing file

Remember: Code review is a learning opportunity. The user may have better approaches or additional context to improve solutions.

---

**Status**: Active development, no official release yet. Contributions welcome - contact maintainer if interested.
