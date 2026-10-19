# M3 — JSON Schema validator for JVM and JavaScript

[![CI](https://github.com/JulesGosnell/m3/actions/workflows/ci.yml/badge.svg)](https://github.com/JulesGosnell/m3/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/JulesGosnell/m3/branch/main/graph/badge.svg)](https://codecov.io/gh/JulesGosnell/m3)
[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.jules_gosnell/m3.svg)](https://clojars.org/org.clojars.jules_gosnell/m3)
[![npm](https://img.shields.io/npm/v/m3-json-schema.svg)](https://www.npmjs.com/package/m3-json-schema)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

**One codebase. JVM and JavaScript. Draft-03 through draft-v1.**

M3 passes the applicable tests in the official [JSON Schema Test Suite][test-suite] across drafts from draft-03 through draft-v1 (formerly draft-next). The JVM and JavaScript implementations share the same Clojure/ClojureScript codebase.

Written in Clojure/ClojureScript, M3 compiles to JVM bytecode and JavaScript. Use it from **Clojure**, **Java**, **Kotlin**, **Scala**, **JavaScript**, or **Node.js**.

Full support for every keyword: `$ref`, `$dynamicRef`, `$recursiveRef`, `unevaluatedProperties`, `unevaluatedItems`, `$vocabulary`, `$anchor`, `$dynamicAnchor`, `if`/`then`/`else`, `dependentSchemas`, `prefixItems`, `contentMediaType`, `contentEncoding`, and all format validators.

**Requires:** Java 21+ (JVM) | Node.js 18+ (JavaScript)

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
 "1.0.0-beta3"]
```

</td>
<td>

```clojure
org.clojars.jules_gosnell/m3
{:mvn/version "1.0.0-beta3"}
```

</td>
<td>

```xml
<dependency>
  <groupId>org.clojars.jules_gosnell</groupId>
  <artifactId>m3</artifactId>
  <version>1.0.0-beta3</version>
</dependency>
```

</td>
<td>

```groovy
implementation
  'org.clojars.jules_gosnell:m3:1.0.0-beta3'
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

The examples use the latest published Clojars release, `1.0.0-beta3`. The source checkout is currently `1.0.0-beta4-SNAPSHOT` and has not been published.

---

## [Test Suite][test-suite] Compliance

| Draft | JVM | JavaScript |
|-------|-----|------------|
| draft-03 | All applicable tests passing | All applicable tests passing |
| draft-04 | All applicable tests passing | All applicable tests passing |
| draft-06 | All applicable tests passing | All applicable tests passing |
| draft-07 | All applicable tests passing | All applicable tests passing |
| draft 2019-09 | All applicable tests passing | All applicable tests passing |
| draft 2020-12 | All applicable tests passing | All applicable tests passing |
| draft-v1 (formerly draft-next) | All applicable tests passing | All applicable tests passing |

The JavaScript run excludes the `zeroTerminatedFloats.json` case "a float is not an integer even without fractional part." JavaScript parses `1.0` and `1` to the same Number value, so the validator cannot distinguish their source notation. The JVM run preserves that distinction and passes the case.

Default draft: `latest` (currently `draft2020-12`)

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

// Pass Jackson values directly — no caller-side conversion needed
ObjectMapper mapper = new ObjectMapper();
Map schema = mapper.readValue(schemaJson, LinkedHashMap.class);
Object document = mapper.readValue(docJson, Object.class);
Map result = JsonSchema.validate(schema, document);

// With options
Map result = JsonSchema.validate(schema, document,
    Map.of("draft", "draft2020-12"));
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
    mapOf("draft" to "draft2020-12"))
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

The Java facade accepts `java.util.Map` and `java.util.List` directly, so Java, Kotlin, and Scala callers can pass values from Jackson, Gson, or other JSON libraries without converting them first. The Clojure API uses Clojure maps and vectors.

---

## Options

| Option | Clojure | Java/JS | Description |
|--------|---------|---------|-------------|
| Draft | `:draft :draft7` | `"draft": "draft7"` | JSON Schema draft version |
| Registry | `:registry {uri schema}` | `"registry": {uri: schema}` | Map of URI to schema for `$ref` resolution |

Supported draft values: `draft3`, `draft4`, `draft6`, `draft7`, `draft2019-09`, `draft2020-12`, `draft-v1`, `latest`.

Use `latest` (`:latest` in Clojure) as an alias for the most recent stable draft (currently `draft2020-12`).

### References

`$ref` supports registered schemas and references within the current schema resource, including URI fragments, anchors, and JSON Pointers. An empty reference (`"$ref": ""`) refers to the current resource. JSON Pointer tokens are decoded as strings, so a token such as `"0"` can address an object property; when traversal reaches an array, numeric tokens are interpreted as array indexes. A reference that cannot be resolved produces a validation error.

---

## Validation Results

M3 returns three levels of feedback: **errors**, **warnings**, and **infos**.

```clojure
(m3/validate {"type" "string"} 42)
;; => {:valid? false, :errors [{:schema-path ["type"], :message "...", ...}]}

(m3/validate {"type" "string" "format" "email"} "not-an-email")
;; => {:valid? true, :errors nil, :warnings [{:schema-path ["format"], :message "...", ...}]}

(m3/validate {"type" "string" "$comment" "a note"} "hello")
;; => {:valid? true, :errors nil, :infos [{:schema-path ["$comment"], :message "...", ...}]}
```

| Level | When | Effect on `:valid?` |
|-------|------|---------------------|
| **Errors** | Validation failures — `type`, `required`, `pattern`, etc. | `false` |
| **Warnings** | Schema is valid but something noteworthy — `format` annotation failures, `contentEncoding` decode failures, `deprecated` properties, unrecognised `format` values | `true` (does not affect validity) |
| **Infos** | Informational annotations — `$comment` values | `true` (does not affect validity) |

`:warnings` and `:infos` are only present in the result when non-empty. All three levels share the same shape:

```
{:schema-path   ["properties" "age" "type"]   ;; path into the schema
 :document-path ["age"]                        ;; path into the document
 :message       "type: not a[n] integer - \"old\""
 :document      "old"                          ;; the failing value
 :schema        {"type" "integer"}             ;; the relevant schema
```

Errors may additionally contain an `:errors` key with nested sub-errors for compound keywords like `allOf`, `oneOf`, etc.

Java/JS output uses camelCase string keys: `schemaPath`, `documentPath`, `message`, `document`, `schema`, `errors`, `warnings`, `infos`.

This makes M3 well-suited for building UIs that display not just pass/fail, but rich diagnostic feedback — warnings for deprecated fields, info annotations from `$comment`, and detailed error trees for complex schemas.

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

# Run Clojure unit and JSON Schema Test Suite tests
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

[test-suite]: https://github.com/json-schema-org/JSON-Schema-Test-Suite
