;; Copyright 2025 Julian Gosnell
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns m3.pattern)

(def email-pattern #"^(?:[^\s@\"\.](?:(?:(?!\.\.)[^\s@\"])*[^\s@\"\.])?|\"(?:[^\r\n\\\"]|\\[\s\S])+\")@(?:[a-zA-Z0-9\-]+(?:\.[a-zA-Z0-9\-]+)*|\[IPv6:[a-fA-F0-9:]+\]|\[(?:(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)\.){3}(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)\])$")

;; https://howtodoinjava.com/java/regex/java-regex-validate-email-address/
;; \z (end-of-input, not end-of-line) so a trailing \n is rejected.
(def ipv4-pattern #"^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\.(?!\z)|\z)){4}\z")

;; adapted from: https://github.com/ajv-validator/ajv-formats/blob/master/src/formats.ts
(def ipv6-pattern #"(?i)^((([0-9a-f]{1,4}:){7}([0-9a-f]{1,4}|:))|(([0-9a-f]{1,4}:){6}(:[0-9a-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){5}(((:[0-9a-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){4}(((:[0-9a-f]{1,4}){1,3})|((:[0-9a-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){3}(((:[0-9a-f]{1,4}){1,4})|((:[0-9a-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){2}(((:[0-9a-f]{1,4}){1,5})|((:[0-9a-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){1}(((:[0-9a-f]{1,4}){1,6})|((:[0-9a-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9a-f]{1,4}){1,7})|((:[0-9a-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))$")

;; adapted from: https://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
(def hostname-pattern #"^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]{0,61}[A-Za-z0-9])$")

(def json-pointer-pattern #"^(?:/(?:[^~/]|~[01])*)*$")

(def relative-pointer-pattern #"^(?:0|[1-9][0-9]*)(#|(?:/(?:[^~/]|~[01])*)*)$|^#(?:/(?:[^~/]|~[01])*)*$")

;; TODO: this should be shared with uri.cljc

;; RFC 3986 URI: scheme then only allowed ASCII chars, with valid percent-encoding.
;; Allowed chars: unreserved (A-Z a-z 0-9 - . _ ~), sub-delims (!$&'()*+,;=),
;; gen-delims (:/?#[]@), and %HH percent-encoded triplets.  Disallows: spaces,
;; backslash, " < > { } ^ ` |, raw % without two hex digits, raw non-ASCII.
;; Also accepts a bare fragment-only form (e.g. "#" or "#foo") so that
;; meta-schema $refs that self-reference don't fail self-validation.
(def uri-pattern
  #"^(?:[A-Za-z][A-Za-z0-9+.\-]*:(?:[A-Za-z0-9\-._~!$&'()*+,;=:/?\#\[\]@]|%[0-9A-Fa-f]{2})*|\#(?:[A-Za-z0-9\-._~!$&'()*+,;=:/?\#\[\]@]|%[0-9A-Fa-f]{2})*)$")

;; URI-reference: like uri-pattern but scheme is optional.  Same allowed chars:
;; unreserved | sub-delims | gen-delims | %HH percent-encoded triplets.
(def uri-reference-pattern
  #"^(?:[A-Za-z][A-Za-z0-9+.\-]*:)?(?:[A-Za-z0-9\-._~!$&'()*+,;=:/?\#\[\]@]|%[0-9A-Fa-f]{2})*$")

(def uri-template-pattern #"^(?:[^\s{}]|(?:\{[^\s{}]*\}))*$")

(def idn-email-pattern #"^(?:[^\s@\[\]\"(),:;<>\\]+(?:\.[^\s@\[\]\"(),:;<>\\]+)*|\"(?:[^\"\\\r\n]|\\.)+\")@(?:[^\s@\[\]\"(),:;<>\\]+\.)*[^\s@\[\]\"(),:;<>\\]+$")

(def iri-pattern #"^[A-Za-z][A-Za-z0-9+.\-]*://(?:[^\s/?#@\\]+@)?(?:\[[0-9A-Fa-f:]+\]|[^\s/?#@:]+)(?::\d+)?(?:/[^\s?#\\]*)?(?:\?[^\s#\\]*)?(?:#[^\s\\]*)?$")

(def iri-reference-pattern #"^[^\s\\]*$")

;; https://www.jvt.me/posts/2022/01/14/java-uuid-regex/
(def uuid-pattern #"^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$")

;; RFC 3339 Appendix A (ABNF for duration):
;;   duration   = "P" (dur-date [dur-time] / dur-time / dur-week)
;;   dur-date   = dur-day / dur-month dur-day? / dur-year dur-month? dur-day?
;;   dur-time   = "T" (dur-hour / dur-minute / dur-second)
;;   dur-hour   = 1*DIGIT "H" [dur-minute]
;;   dur-minute = 1*DIGIT "M" [dur-second]
;;   dur-second = 1*DIGIT "S"
;;   dur-day    = 1*DIGIT "D"
;;   dur-month  = 1*DIGIT "M" [dur-day]
;;   dur-year   = 1*DIGIT "Y" [dur-month [dur-day]]
;;   dur-week   = 1*DIGIT "W"
;; Note that components must appear in order and intermediate components
;; cannot be omitted, e.g. "P1Y2D" (year+day, no month) is invalid.
(def json-duration-pattern
  (let [date "(?:\\d+Y(?:\\d+M(?:\\d+D)?)?|\\d+M(?:\\d+D)?|\\d+D)"
        time "T(?:\\d+H(?:\\d+M(?:\\d+S)?)?|\\d+M(?:\\d+S)?|\\d+S)"]
    (re-pattern (str "^P(?:" date "(?:" time ")?|" time "|\\d+W)$"))))

;; see: https://github.com/juxt/jinx/blob/master/src/juxt/jinx/alpha/patterns.clj

(def time-pattern #"^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.\d{1,6})?(Z|[\+-]([01][0-9]|2[0-3]):[0-5][0-9])?$")
(def ip-address-pattern #"^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$")
(def color-pattern #"^#[0-9a-fA-F]{3}(?:[0-9a-fA-F]{3})?$|^rgb\(\s*(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\s*,\s*){2}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\s*\)$")
