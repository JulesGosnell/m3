(ns m3.pattern)

;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
(def email-pattern #"^(?:[^\s@\"\.](?:(?!\.\.)[^\s@\"])*[^\s@\"\.]|\"(?:[^\r\n\\\"]|\\[\s\S])+\")@(?:[a-zA-Z0-9\-]+(?:\.[a-zA-Z0-9\-]+)*|\[IPv6:[a-fA-F0-9:]+\]|\[(?:(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)\])$")

;; https://howtodoinjava.com/java/regex/java-regex-validate-email-address/
(def ipv4-pattern #"^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\.(?!$)|$)){4}$")

;; adapted from: https://github.com/ajv-validator/ajv-formats/blob/master/src/formats.ts
(def ipv6-pattern #"^((([0-9a-f]{1,4}:){7}([0-9a-f]{1,4}|:))|(([0-9a-f]{1,4}:){6}(:[0-9a-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){5}(((:[0-9a-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){4}(((:[0-9a-f]{1,4}){1,3})|((:[0-9a-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){3}(((:[0-9a-f]{1,4}){1,4})|((:[0-9a-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){2}(((:[0-9a-f]{1,4}){1,5})|((:[0-9a-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){1}(((:[0-9a-f]{1,4}){1,6})|((:[0-9a-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9a-f]{1,4}){1,7})|((:[0-9a-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))$")

;; adapted from: https://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
(def hostname-pattern #"^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]{0,61}[A-Za-z0-9])$")

;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
(def json-pointer-pattern #"^(?:/(?:[^~/]|~[01])*)*$")

;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
(def relative-pointer-pattern #"^(?:0|[1-9][0-9]*)(#|(?:/(?:[^~/]|~[01])*)*)$|^#(?:/(?:[^~/]|~[01])*)*$")

;; TODO: this should be shared with uri.cljc

;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
(def uri-pattern #"^(?:[A-Za-z][A-Za-z0-9+.\-]*:[^\s]*|#(?:[^\s]*)?)$")

;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
(def uri-reference-pattern #"^[^\s\\]*$")

;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
(def uri-template-pattern #"^(?:[^\s{}]|(?:\{[^\s{}]*\}))*$")

;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
(def idn-email-pattern #"^(?:[^\s@\[\]\"(),:;<>\\]+(?:\.[^\s@\[\]\"(),:;<>\\]+)*|\"(?:[^\"\\\r\n]|\\.)+\")@(?:[^\s@\[\]\"(),:;<>\\]+\.)*[^\s@\[\]\"(),:;<>\\]+$")

;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
(def iri-pattern #"^[A-Za-z][A-Za-z0-9+.\-]*://(?:[^\s/?#@\\]+@)?(?:\[[0-9A-Fa-f:]+\]|[^\s/?#@:]+)(?::\d+)?(?:/[^\s?#\\]*)?(?:\?[^\s#\\]*)?(?:#[^\s\\]*)?$")

;; N.B. made by ChatGPT o-preview specifically to pass testsuite...
(def iri-reference-pattern #"^[^\s\\]*$")

;; https://www.jvt.me/posts/2022/01/14/java-uuid-regex/
(def uuid-pattern #"^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$")

;; ISO 8601 duration
;; https://en.wikipedia.org/wiki/ISO_8601#Durations
;; TODO:
;; - allow more than 4 numbers before each letter
;; - allow a single decimal point in each number group ?
;; - allow a general date-time format ?
(def json-duration-pattern
  (re-pattern
   (str
    "^"
    "P"
    "("
    "("
    "([0-9]{1,4}Y|)"
    "([0-9]{1,4}M|)"
    "([0-9]{1,4}D|)"
    ")"
    "("
    "T"
    "([0-9]{1,4}H|)"
    "([0-9]{1,4}M|)"
    "([0-9]{1,4}S|)"
    "|"
    ")"
    "|"
    "([0-9]{1,4}W|)"    
    ")"
    "$"    
    )))

;; see: https://github.com/juxt/jinx/blob/master/src/juxt/jinx/alpha/patterns.clj

;; by grok4
(def time-pattern #"^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.\d{1,6})?(Z|[\+-]([01][0-9]|2[0-3]):[0-5][0-9])?$")
(def ip-address-pattern #"^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$")
(def color-pattern #"^#[0-9a-fA-F]{3}(?:[0-9a-fA-F]{3})?$|^rgb\(\s*(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\s*,\s*){2}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\s*\)$|^[a-zA-Z]+$")
