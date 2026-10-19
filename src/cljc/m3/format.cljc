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

(ns m3.format
  (:require
   [cljc.java-time.local-date :refer [parse] :rename {parse local-date-parse}]
   [clojure.string :as str]
   [m3.util :refer [make-error]]
   [m3.ecma :refer [ecma-pattern]]
   [m3.pattern :refer [email-pattern ipv4-pattern ipv6-pattern hostname-pattern
                       json-pointer-pattern relative-pointer-pattern uri-pattern
                       uri-reference-pattern uri-template-pattern idn-email-pattern
                       iri-pattern iri-reference-pattern uuid-pattern json-duration-pattern
                       time-pattern ip-address-pattern color-pattern]]
   [m3.idn-hostname :refer [json-idn-hostname?]]))

;;------------------------------------------------------------------------------
;; Helper functions

(defn match [format pattern _c2 p2 m2 p1 m1]
  ;; re-matches requires the WHOLE string to match the pattern — re-find
  ;; would accept "valid hostname\n" because Java's $ matches before a
  ;; trailing newline.  re-matches also avoids the \z portability problem
  ;; (JS regex doesn't recognise \z, treating it as literal 'z').
  (when-not (re-matches pattern m1)
    [(make-error (str "format: not a valid " format) p2 m2 p1 m1)]))

(defn check-pattern [pattern format c2 p2 m2]
  (fn [_c1 p1 m1]
    (match format pattern c2 p2 m2 p1 m1)))

(defn check-parse [p f _c2 p2 m2]
  (fn [_c1 p1 m1]
    (try
      (p m1)
      nil
      (catch #?(:cljs js/Error :clj Exception) e
        [(make-error (str "format: not a valid " f ": " (ex-message e)) p2 m2 p1 m1)]))))

(defn json-duration? [s]
  ;; The RFC 3339 grammar baked into json-duration-pattern already enforces
  ;; ordering, intermediate-component requirements, and that "P" / "PT" alone
  ;; do not match — so a successful regex match is sufficient.
  (boolean (re-matches json-duration-pattern s)))

(defn ^:private parse-int [s]
  #?(:clj (Integer/parseInt s) :cljs (js/parseInt s 10)))

(def ^:private leap-second-time-re
  #"(?i)^(\d{2}):(\d{2}):(60(?:\.\d+)?)(z|([+-])(\d{2}):(\d{2}))$")

(defn ^:private valid-leap-second-time?
  "Returns true if s is a valid leap-second time, false if it contains :60 but
   is invalid, nil if it does not contain :60 (not a leap second)."
  [s]
  (when-let [[_ hh mm _ss _tz tz-sign tz-hh tz-mm] (re-find leap-second-time-re s)]
    (let [h (parse-int hh)
          m (parse-int mm)]
      (and (<= 0 h 23)
           (<= 0 m 59)
           (if (nil? tz-sign)
             (and (= h 23) (= m 59))
             (let [oh (parse-int tz-hh)
                   om (parse-int tz-mm)]
               (and (<= 0 oh 23)
                    (<= 0 om 59)
                    (let [local-min (+ (* h 60) m)
                          off-min (* (if (= tz-sign "+") 1 -1) (+ (* oh 60) om))
                          utc-min (mod (- local-min off-min) 1440)]
                      (= utc-min 1439)))))))))

;;------------------------------------------------------------------------------
;; Individual format checker functions

(defn check-format-host-name [_c2 p2 m2]
  (check-pattern hostname-pattern "host-name" _c2 p2 m2))

(defn check-format-ip-address [_c2 p2 m2]
  (check-pattern ip-address-pattern "ip-address" _c2 p2 m2))

(def ^:private css-color-names
  #{"aqua" "black" "blue" "fuchsia" "gray" "green" "lime" "maroon"
    "navy" "olive" "orange" "purple" "red" "silver" "teal" "white" "yellow"})

(defn check-format-color [_c2 p2 m2]
  (fn [_c1 p1 m1]
    (when-not (or (re-find color-pattern m1)
                  (css-color-names (str/lower-case m1)))
      [(make-error "format: not a valid color" p2 m2 p1 m1)])))

(defn check-format-style [_c2 _p2 _m2]
  ;; Draft-03 specific, not validated
  (constantly nil))

(defn check-format-phone [_c2 _p2 _m2]
  ;; Draft-03 specific, not validated
  (constantly nil))

(defn check-format-utc-millisec [_c2 _p2 _m2]
  ;; Draft-03 specific, not validated
  (constantly nil))

(defn check-format-email [_c2 p2 m2]
  (check-pattern email-pattern "email" _c2 p2 m2))

(defn check-format-ipv4 [_c2 p2 m2]
  (check-pattern ipv4-pattern "ipv4" _c2 p2 m2))

(defn check-format-ipv6 [_c2 p2 m2]
  (check-pattern ipv6-pattern "ipv6" _c2 p2 m2))

(def ^:private ace-label-prefix-re #"(?i)^xn--")

(defn check-format-hostname [_c2 p2 m2]
  ;; Plain hostname must be ASCII (RFC 1123).  Validate:
  ;;   - structural regex (label characters, hyphen position, label length ≤ 63)
  ;;   - total length ≤ 253 (RFC 1035 §2.3.4 / §3.1)
  ;;   - any A-label (xn-- prefix) is Punycode-decodable per RFC 5891
  ;; Non-ACE labels with consecutive hyphens (e.g. "ab--cd") are valid per
  ;; RFC 1123 — only ACE labels get full IDNA validation.
  (fn [_c1 p1 m1]
    (when (string? m1)
      (or (when-not (re-matches hostname-pattern m1)
            [(make-error "format: hostname has invalid structure" p2 m2 p1 m1)])
          (when (> (count m1) 253)
            [(make-error "format: hostname exceeds 253 characters" p2 m2 p1 m1)])
          (let [labels (str/split m1 #"\.")]
            (when (and (some #(re-find ace-label-prefix-re %) labels)
                       (not (json-idn-hostname? m1)))
              [(make-error "format: hostname A-label fails IDNA Punycode validation" p2 m2 p1 m1)]))))))

(def ^:private date-time-format-re
  #"^(\d{4}-\d{2}-\d{2})[Tt](\d{2}):(\d{2}):(\d{2})(?:\.(\d+))?([Zz]|[+-]\d{2}:\d{2})$")

(defn- valid-clock-fields? [hh mm ss]
  (let [h (parse-int hh) m (parse-int mm) s (parse-int ss)]
    (and (<= 0 h 23) (<= 0 m 59) (<= 0 s 60))))

(defn- valid-time-offset? [tz]
  (or (re-matches #"[Zz]" tz)
      (when-let [[_ hh mm] (re-matches #"[+-](\d{2}):(\d{2})" tz)]
        (and (<= 0 (parse-int hh) 23) (<= 0 (parse-int mm) 59)))))

(defn check-format-date-time [_c2 p2 m2]
  (fn [_c1 p1 m1]
    (when (string? m1)
      (let [[_ date hh mm ss _fraction tz] (re-matches date-time-format-re m1)]
        (when-not
         (and date
              (valid-clock-fields? hh mm ss)
              (valid-time-offset? tz)
              (or (not= ss "60") (valid-leap-second-time? (str hh ":" mm ":" ss tz)))
              (try (local-date-parse date) true
                   (catch #?(:cljs js/Error :clj Exception) _ false)))
          [(make-error "format: not a valid date-time" p2 m2 p1 m1)])))))

(defn check-format-date [_c2 p2 m2]
  (let [parse-check (check-parse local-date-parse "date" _c2 p2 m2)]
    (fn [_c1 p1 m1]
      (when (string? m1)
        (if (re-matches #"\d{4}-\d{2}-\d{2}" m1)
          (parse-check _c2 p1 m1)
          [(make-error "format: not a valid date (year must be four digits)" p2 m2 p1 m1)])))))

(defn check-format-time-pattern [c2 p2 m2]
  (check-pattern time-pattern "time" c2 p2 m2))

(defn check-format-time-parse [c2 p2 m2]
  (fn [_c1 p1 m1]
    (when (string? m1)
      (when-not
       (if-let [[_ hh mm ss _fraction tz]
                (re-matches #"^(\d{2}):(\d{2}):(\d{2})(?:\.(\d+))?([Zz]|[+-]\d{2}:\d{2})$" m1)]
         (and (valid-clock-fields? hh mm ss)
              (if (= ss "60")
                (and (not= :draft-v1 (:draft c2))
                     (valid-leap-second-time? m1))
                true)
              (valid-time-offset? tz))
         false)
        [(make-error "format: not a valid time" p2 m2 p1 m1)]))))

(defn check-format-json-pointer [_c2 p2 m2]
  (check-pattern json-pointer-pattern "json-pointer" _c2 p2 m2))

(defn check-format-relative-json-pointer [_c2 p2 m2]
  (check-pattern relative-pointer-pattern "relative-json-pointer" _c2 p2 m2))

;; If a URI has an authority (scheme://...), reject "[" / "]" outside an IPv6
;; literal and require any port to be all digits.  These are the structural
;; rules the character-set regex can't catch.
(def ^:private authority-prefix-re #"^(?:[A-Za-z][A-Za-z0-9+.\-]*:)?//")
(def ^:private scheme-prefix-re #"^[A-Za-z][A-Za-z0-9+.\-]*:")
(def ^:private ipvfuture-pattern #"(?i)^v[0-9a-f]+\.[a-z0-9\-._~!$&'()*+,;=:]+$")

(defn- valid-port? [port]
  (or (nil? port) (re-matches #"\d*" port)))

(defn- valid-ip-literal? [host]
  (or (re-matches ipv6-pattern host)
      (re-matches ipvfuture-pattern host)))

(defn- valid-authority? [authority]
  (let [at-count (count (re-seq #"@" authority))]
    (when (<= at-count 1)
      (let [[userinfo host-port] (if (= at-count 1)
                                   (str/split authority #"@" 2)
                                   [nil authority])]
        (and (not (and userinfo (re-find #"[\[\]]" userinfo)))
             (if (str/starts-with? host-port "[")
               (when-let [[_ host suffix] (re-matches #"\[([^\]]+)\](:.*)?" host-port)]
                 (and (valid-ip-literal? host)
                      (or (nil? suffix) (valid-port? (subs suffix 1)))
                      (or (not (str/includes? host "."))
                          (and (re-matches ipv6-pattern host)
                               (let [embedded-ipv4 (last (str/split host #":"))]
                                 (or (not (str/includes? embedded-ipv4 "."))
                                     (re-matches ipv4-pattern embedded-ipv4)))))))
               (let [parts (str/split host-port #":" -1)]
                 (and (<= (count parts) 2)
                      (not (str/includes? (first parts) "["))
                      (not (str/includes? (first parts) "]"))
                      (or (= 1 (count parts)) (valid-port? (second parts)))))))))))

(defn- authority-end [s start]
  (or (some (fn [i]
              (when (#{\/ \? \#} (nth s i)) i))
            (range start (count s)))
      (count s)))

(defn- valid-uri-structure? [s]
  (let [authority-start (when-let [m (re-find authority-prefix-re s)] (count m))
        has-scheme (boolean (re-find scheme-prefix-re s))]
    (if authority-start
      (let [end (authority-end s authority-start)
            authority (subs s authority-start end)
            remainder (subs s end)]
        (and (valid-authority? authority)
             (not (re-find #"[\[\]]" remainder))))
      (and (not (re-find #"[\[\]]" s))
           (or has-scheme
               (let [path (first (str/split s #"[?#]" 2))
                     first-segment (first (str/split path #"/" 2))]
                 (not (str/includes? first-segment ":"))))))))

(defn- valid-percent-encoding? [s]
  (not (re-find #"%(?![0-9A-Fa-f]{2})" s)))

(defn check-format-uri [_c2 p2 m2]
  (let [base-check (check-pattern uri-pattern "uri" _c2 p2 m2)]
    (fn [_c1 p1 m1]
      (or (base-check _c1 p1 m1)
          ;; Authority structure: only applies when scheme://... is present.
          (when (and (string? m1) (not (valid-uri-structure? m1)))
            [(make-error "format: uri authority or path structure is malformed" p2 m2 p1 m1)])))))

(defn check-format-uri-reference [_c2 p2 m2]
  (let [base-check (check-pattern uri-reference-pattern "uri-reference" _c2 p2 m2)]
    (fn [_c1 p1 m1]
      (or (base-check _c1 p1 m1)
          (when (and (string? m1) (not (valid-uri-structure? m1)))
            [(make-error "format: uri-reference authority or path structure is malformed" p2 m2 p1 m1)])))))

(defn check-format-uri-template [_c2 p2 m2]
  (check-pattern uri-template-pattern "uri-template" _c2 p2 m2))

(defn check-format-idn-email [_c2 p2 m2]
  (fn [_c1 p1 m1]
    (when (string? m1)
      (let [[_ _local domain] (re-matches idn-email-pattern m1)]
        (when-not (and domain (json-idn-hostname? domain))
          [(make-error "format: not a valid idn-email" p2 m2 p1 m1)])))))

(defn check-format-idn-hostname [_c2 p2 m2]
  (fn [_c1 p1 m1]
    (when-not (json-idn-hostname? m1)
      [(make-error "format: not a valid idn-hostname:" p2 m2 p1 m1)])))

(defn check-format-iri [_c2 p2 m2]
  (let [base-check (check-pattern iri-pattern "iri" _c2 p2 m2)]
    (fn [_c1 p1 m1]
      (or (base-check _c1 p1 m1)
          (when (and (string? m1)
                     (or (not (valid-percent-encoding? m1))
                         (not (valid-uri-structure? m1))))
            [(make-error "format: not a valid iri" p2 m2 p1 m1)])))))

(defn check-format-iri-reference [_c2 p2 m2]
  (let [base-check (check-pattern iri-reference-pattern "iri-reference" _c2 p2 m2)]
    (fn [_c1 p1 m1]
      (or (base-check _c1 p1 m1)
          (when (and (string? m1)
                     (or (not (valid-percent-encoding? m1))
                         (not (valid-uri-structure? m1))))
            [(make-error "format: not a valid iri-reference" p2 m2 p1 m1)])))))

(defn check-format-uuid [_c2 p2 m2]
  (check-pattern uuid-pattern "uuid" _c2 p2 m2))

(defn check-format-duration [_c2 p2 m2]
  (fn [_c1 p1 m1]
    (when-not (json-duration? m1)
      [(make-error "format: not a valid duration:" p2 m2 p1 m1)])))

(defn check-format-regex [_c2 p2 m2]
  (check-parse ecma-pattern "regex" _c2 p2 m2))

(defn check-format-unknown [_c2 _p2 _m2]
  (constantly nil))

;;------------------------------------------------------------------------------

(def draft3-format->checker
  {"color"        check-format-color
   "date"         check-format-date
   "date-time"    check-format-date-time
   "email"        check-format-email
   "host-name"    check-format-host-name    ; renamed to "hostname" in draft-04
   "ip-address"   check-format-ip-address   ; renamed to "ipv4" in draft-04
   "ipv6"         check-format-ipv6
   "phone"        check-format-phone        ; dropped in draft-04
   "regex"        check-format-regex
   "style"        check-format-style        ; dropped in draft-04
   "time"         check-format-time-pattern ; uses pattern check in draft-03
   "uri"          check-format-uri
   "utc-millisec" check-format-utc-millisec ; dropped in draft-04
   })

(def draft4-format->checker
  (-> draft3-format->checker
      (dissoc
       "color"
       "date"
       "phone"
       "style"
       "utc-millisec"
       "host-name"
       "ip-address"
       "time")
      (assoc
       "hostname" check-format-hostname
       "ipv4"     check-format-ipv4
       "regex"   check-format-regex
       "unknown"  check-format-unknown)))

(def draft6-format->checker
  (assoc
   draft4-format->checker
   "json-pointer"  check-format-json-pointer
   "uri-reference" check-format-uri-reference
   "uri-template"  check-format-uri-template))

(def draft7-format->checker
  (assoc
   draft6-format->checker
   "date"                  check-format-date
   "idn-email"             check-format-idn-email
   "idn-hostname"          check-format-idn-hostname
   "iri"                   check-format-iri
   "iri-reference"         check-format-iri-reference
   "relative-json-pointer" check-format-relative-json-pointer
   "time"                  check-format-time-parse))

(def draft2019-09-format->checker
  (assoc
   draft7-format->checker
   "duration" check-format-duration
   "uuid"     check-format-uuid))

(def draft2020-12-format->checker draft2019-09-format->checker)

(def draft-next-format->checker draft2019-09-format->checker)
