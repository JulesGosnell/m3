# M3 — The most complete JSON Schema validator

[![CI](https://github.com/JulesGosnell/m3/actions/workflows/ci.yml/badge.svg)](https://github.com/JulesGosnell/m3/actions/workflows/ci.yml)
[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.jules_gosnell/m3.svg)](https://clojars.org/org.clojars.jules_gosnell/m3)
[![npm](https://img.shields.io/npm/v/m3-json-schema.svg)](https://www.npmjs.com/package/m3-json-schema)

**Every draft. Every keyword. Every language.**

M3 passes **every test**[^1] in the official [JSON Schema Test Suite][test-suite] across **every draft** from draft-03 through draft-next — **9,622 assertions** with zero failures. No other validator in any language covers all seven drafts completely, and M3 will use the same code to give you the same answer in both front and backend.

This includes full support for every keyword: `$ref`, `$dynamicRef`, `$recursiveRef`, `unevaluatedProperties`, `unevaluatedItems`, `$vocabulary`, `$anchor`, `$dynamicAnchor`, `if`/`then`/`else`, `dependentSchemas`, `prefixItems`, `contentMediaType`, `contentEncoding`, ...and all format validators.

Use it from **Clojure**, **Java**, **Kotlin**, **Scala**, **JavaScript**, or **Node.js**.

**Requires:** Java 17+ (JVM) | Node.js 18+ (JavaScript)

### Installation

<table>
<tr>
<td><b>Leiningen</b></td>
<td><b>deps.edn</b></td>
<td><b>Maven</b></td>
<td><b>Gradle</b></td>
<td><b>npm</b></td>
</tr>
<tr>
<td>

```clojure
[org.clojars.jules_gosnell/m3
 "1.0.0-beta1"]
```

</td>
<td>

```clojure
org.clojars.jules_gosnell/m3
{:mvn/version "1.0.0-beta1"}
```

</td>
<td>

```xml
<dependency>
  <groupId>org.clojars.jules_gosnell</groupId>
  <artifactId>m3</artifactId>
  <version>1.0.0-beta1</version>
</dependency>
```

</td>
<td>

```groovy
implementation
  'org.clojars.jules_gosnell:m3:1.0.0-beta1'
```

</td>
<td>

```bash
npm install m3-json-schema
```

</td>
</tr>
</table>

> **Note for Maven/Gradle users:** M3 is hosted on [Clojars](https://clojars.org). Add the Clojars repository to your build configuration:
>
> **Maven** — add to `<repositories>` in `pom.xml`:
> ```xml
> <repository>
>   <id>clojars</id>
>   <url>https://repo.clojars.org</url>
> </repository>
> ```
>
> **Gradle** — add to `repositories` block:
> ```groovy
> maven { url 'https://repo.clojars.org' }
> ```

---

## [Test Suite][test-suite] Compliance

| Draft | JVM | JavaScript |
|-------|-----|------------|
| draft-03 | All tests passing | All tests passing |
| draft-04 | All tests passing | All tests passing |
| draft-06 | All tests passing | All tests passing |
| draft-07 | All tests passing | All tests passing |
| draft 2019-09 | All tests passing | All tests passing |
| draft 2020-12 | All tests passing | All tests passing |
| draft-next | All tests passing | All tests passing[^1] |

No other JSON Schema validator supports all seven drafts. Most support only one or two. M3 is the only implementation listed for draft-next.

Default draft: `draft2020-12`

---

## Language Examples

### Clojure

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

### Java

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

### Kotlin

```kotlin
import m3.JsonSchema

val result = JsonSchema.validate("""{"type":"string"}""", "\"hello\"")
val valid = result["valid"] as Boolean   // true

// From parsed maps
val schema = mapOf("type" to "object", "required" to listOf("name", "age"))
val doc = mapOf("name" to "Alice", "age" to 30)
val result = JsonSchema.validate(schema, doc)

// With options
val result = JsonSchema.validate(schema, doc,
    mapOf("draft" to "draft2020-12", "strictFormat" to true))
```

### Scala

```scala
import m3.JsonSchema
import java.util.{Map => JMap, LinkedHashMap}

// From JSON strings
val result = JsonSchema.validate("""{"type":"string"}""", "\"hello\"")
val valid = result.get("valid").asInstanceOf[Boolean]   // true

// From parsed maps (e.g. via Jackson or Gson)
val schema = new LinkedHashMap[String, Any]()
schema.put("type", "integer")
schema.put("minimum", 0)
val result = JsonSchema.validate(schema, 42)
```

### JavaScript / Node.js

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

All JVM languages accept `java.util.Map` and `java.util.List` directly — documents from Jackson, Gson, or any JSON library work with zero conversion.

---

## Options

| Option | Clojure | Java/JS | Description |
|--------|---------|---------|-------------|
| Draft | `:draft :draft7` | `"draft": "draft7"` | JSON Schema draft version |
| Strict format | `:strict-format? true` | `"strictFormat": true` | Treat `format` as assertion (default: annotation-only) |
| Strict integer | `:strict-integer? true` | `"strictInteger": true` | Require actual integers (reject `1.0` for `"type": "integer"`) |

Supported draft values: `draft3`, `draft4`, `draft6`, `draft7`, `draft2019-09`, `draft2020-12`, `draft-next`, `latest`.

Use `latest` (`:latest` in Clojure) as an alias for the most recent stable draft (currently `draft2020-12`).

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

Dialects are composable: each draft is defined as an ordered set of vocabularies, and each vocabulary maps keywords to checker functions. This makes M3 extensible — custom dialects can be assembled from existing or new vocabularies.

Internally, two context maps thread through validation:
- **c2** (compile-time): draft, dialect, URI resolution, schema stash
- **c1** (runtime): evaluation tracking, dynamic anchor scope, conditional state

---

## Building from Source

```bash
git clone --recursive git@github.com:JulesGosnell/m3.git
cd m3

# Run Clojure tests (9,622 test-suite assertions)
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

## License

Copyright 2025 Julian Gosnell. [Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0).

---

[^1]: One test is excluded on JavaScript: `zeroTerminatedFloats.json` — "a float is not an integer even without fractional part". JavaScript has no integer/float distinction (`JSON.parse("1.0") === JSON.parse("1")`), making this test impossible to pass at the language level. On the JVM, all 9,622 test-suite assertions pass without exception.

[test-suite]: https://github.com/json-schema-org/JSON-Schema-Test-Suite
