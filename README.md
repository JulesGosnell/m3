# M3 — The most complete JSON Schema validator

[![CI](https://github.com/JulesGosnell/m3/actions/workflows/ci.yml/badge.svg)](https://github.com/JulesGosnell/m3/actions/workflows/ci.yml)
[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.jules_gosnell/m3.svg)](https://clojars.org/org.clojars.jules_gosnell/m3)

**Every draft. Every keyword. Every language.**

M3 passes all 9,710 assertions in the official [JSON Schema Test Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite) across every draft from draft-03 through draft-next — including `$dynamicRef`, `unevaluatedProperties`, `$vocabulary`, and all format validators.

Use it from **Clojure**, **Java**, **Kotlin**, **Scala**, **JavaScript**, or **Node.js**.

---

## Quick Start

### Clojure

```clojure
;; Leiningen
[org.clojars.jules_gosnell/m3 "1.0.0-beta1"]

;; deps.edn
org.clojars.jules_gosnell/m3 {:mvn/version "1.0.0-beta1"}
```

```clojure
(require '[m3.json-schema :as m3])

(m3/validate {"type" "string"} "hello")
;; => {:valid? true, :errors nil}

(m3/validate {"type" "number"} "oops")
;; => {:valid? false, :errors [{:schema-path ["type"], :message "type: not a[n] number - \"oops\"", ...}]}

;; Compile once, validate many
(let [v (m3/validator {"type" "object" "required" ["id"]})]
  (v {"id" 1})   ;; => {:valid? true, :errors nil}
  (v {}))         ;; => {:valid? false, :errors [...]}

;; Choose a draft
(m3/validate {"type" "string"} "hello" {:draft :draft7})
```

### Java / Kotlin / Scala

```xml
<!-- Maven -->
<dependency>
  <groupId>org.clojars.jules_gosnell</groupId>
  <artifactId>m3</artifactId>
  <version>1.0.0-beta1</version>
</dependency>
```

```java
import m3.JsonSchema;
import java.util.Map;
import java.util.List;

// From JSON strings
Map result = JsonSchema.validate("{\"type\":\"string\"}", "\"hello\"");
boolean valid = (boolean) result.get("valid");   // true
List errors = (List) result.get("errors");        // null

// Zero-copy from Jackson — no conversion needed
ObjectMapper mapper = new ObjectMapper();
Map schema = mapper.readValue(schemaJson, LinkedHashMap.class);
Object document = mapper.readValue(docJson, Object.class);
Map result = JsonSchema.validate(schema, document);

// With options
Map result = JsonSchema.validate(schema, document,
    Map.of("draft", "draft2020-12", "strictFormat", true));
```

```kotlin
val result = JsonSchema.validate(mapOf("type" to "string"), "hello")
val valid = result["valid"] as Boolean
```

M3 accepts `java.util.Map` and `java.util.List` directly — documents from Jackson, Gson, or any JSON library work with zero conversion.

### JavaScript / Node.js

```bash
npm install m3-json-schema
```

```javascript
const { validate, validator } = require('m3-json-schema');

validate({ type: 'string' }, 'hello');
// { valid: true, errors: null }

validate({ type: 'number' }, 'not a number');
// { valid: false, errors: [{ schemaPath: ['type'], message: '...', ... }] }

// Compile once, validate many
const v = validator({ type: 'object', required: ['id'] });
v({ id: 1 });   // { valid: true, errors: null }
v({});            // { valid: false, errors: [...] }

// Choose a draft
validate({ type: 'string' }, 'hello', { draft: 'draft7' });
```

---

## Draft Support

| Draft | Status |
|-------|--------|
| draft-03 | All tests passing |
| draft-04 | All tests passing |
| draft-06 | All tests passing |
| draft-07 | All tests passing |
| draft 2019-09 | All tests passing |
| draft 2020-12 | All tests passing |
| draft-next | All tests passing |

Default: `draft2020-12`

---

## Options

| Option | Clojure | Java/JS | Description |
|--------|---------|---------|-------------|
| Draft | `:draft :draft7` | `"draft": "draft7"` | JSON Schema draft version |
| Strict format | `:strict-format? true` | `"strictFormat": true` | Treat `format` as assertion (default: annotation-only) |
| Strict integer | `:strict-integer? true` | `"strictInteger": true` | Require actual integers (reject `1.0` for `"type": "integer"`) |

---

## Error Shape

Errors are nested trees mirroring the schema structure:

```
{:schema-path   ["properties" "age" "type"]   ;; path into the schema
 :document-path ["age"]                        ;; path into the document
 :message       "type: not a[n] integer - \"old\""
 :document      "old"                          ;; the failing value
 :schema        {"type" "integer"}             ;; the relevant schema
 :errors        [...]                          ;; nested sub-errors
```

Java/JS output uses camelCase string keys: `schemaPath`, `documentPath`, `message`, `document`, `schema`, `errors`.

---

## Architecture

M3 uses a two-level curried design:

- **Level 2 (compile time)**: Each schema keyword compiles into a validation function
- **Level 1 (runtime)**: The compiled function validates documents

This means schema compilation is done once and the compiled validator can be reused across many documents — use `validator` / `JsonSchema.validate(Map, Object)` for best performance.

Internally, two context maps thread through validation:
- **c2** (compile-time): draft, dialect, URI resolution, schema stash
- **c1** (runtime): evaluation tracking, dynamic anchor scope, conditional state

---

## Building from Source

```bash
git clone --recursive git@github.com:JulesGosnell/m3.git
cd m3

# Run Clojure tests (9,710 assertions)
lein test

# Run ClojureScript tests
npm install
lein test-cljs

# Build npm module
lein shadow compile npm

# Clean everything
lein clean-all
```

---

## Platform Notes

Java and JavaScript handle numbers differently — `(= 1 1.0)` is `false` in Clojure but `true` in ClojureScript. This can cause edge-case differences between JVM and JS validation for numeric types. Use `:strict-integer? true` if you need strict integer checking.

---

## License

Copyright 2025 Julian Gosnell. [Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0).
