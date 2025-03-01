# m3

[![CI](https://github.com/JulesGosnell/m3/actions/workflows/ci.yml/badge.svg)](https://github.com/JulesGosnell/m3/actions/workflows/ci.yml)

This is the m3 project - a pure Clojure JSON validator.

It is based upon a validator for a subset of JSON which I wrote for [Agora Digital Capital Markets](https://agoradcm.com/), and which they have kindly donated to the project - my thanks to them for allowing me to share this code.

I aspire to it becoming a complete and fully featured JSON validator which can be run and produce identical results in both the backend and frontend of any Clojure[Script] application.

It is tested (lein test) against [JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite). There are still holes in fn-ality - I hope to plug these soon.

Here is a table of M3's current [features](https://julesgosnell.github.io/m3/features.html) which should allow you to see whether we support the functionality that your project requires.

I hope to get a release up to [Clojars](https://clojars.org/) soon.

If you find this useful or interesting and would like to get involved, please give me a shout.


Jules

Here are some simple usage examples:

```
;; (validate schema-context schema document-context document)
```

```
m3.validate> (validate {} {"type" "string"} {} "hello")
{:valid? true, :errors nil}
```

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
```

```
m3.validate> (validate {} {"type" "array" "items" {"type" "string"}} {} ["hello" "goodbye"])
{:valid? true, :errors nil}
```

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
```

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
```

```
m3.validate> (validate {:strict-format? false} {"type" "string" "format" "date"} {} "2025/01/01")
[nREPL-session-2ae0106f-f0da-4741-ac30-6dc3a1fe61e1] WARN m3.validate - format: not a valid date: "2025/01/01" - Text '2025/01/01' could not be parsed at index 4 - "2025/01/01"
{:valid? true, :errors nil}
```

```
m3.validate> (validate {} {"type" "string" "format" "date"} {} "2025-01-01")
{:valid? true, :errors nil}
```
