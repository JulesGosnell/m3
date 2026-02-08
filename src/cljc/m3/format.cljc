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
   [cljc.java-time.offset-date-time :refer [parse] :rename {parse offset-date-time-parse}]
   [cljc.java-time.offset-time :refer [parse] :rename {parse offset-time-parse}]
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
  (when-not (re-find pattern m1)
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
  (boolean
   (when-let [[_p-t-or-w _ymdthms-or-w ymd _y _m _d thms _h _m _s w] (re-find json-duration-pattern s)]
     (not
      (or
       (and (empty? ymd) (empty? thms) (empty? w))
       (= "T" thms))))))

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

(def ^:private date-time-split-re #"(?i)^(\d{4}-\d{2}-\d{2})t(.+)$")

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

(defn check-format-hostname [_c2 p2 m2]
  (check-pattern hostname-pattern "hostname" _c2 p2 m2))

(defn check-format-date-time [_c2 p2 m2]
  (let [normal-check (check-parse offset-date-time-parse "date-time" _c2 p2 m2)]
    (fn [_c1 p1 m1]
      (if-let [[_ date-part time-part] (re-find date-time-split-re m1)]
        (case (valid-leap-second-time? time-part)
          true (try
                 (local-date-parse date-part)
                 nil
                 (catch #?(:cljs js/Error :clj Exception) e
                   [(make-error (str "format: not a valid date-time: " (ex-message e)) p2 m2 p1 m1)]))
          false [(make-error "format: not a valid date-time" p2 m2 p1 m1)]
          (normal-check _c1 p1 m1))
        (normal-check _c1 p1 m1)))))

(defn check-format-date [_c2 p2 m2]
  (check-parse local-date-parse "date" _c2 p2 m2))

(defn check-format-time-pattern [c2 p2 m2]
  (check-pattern time-pattern "time" c2 p2 m2))

(defn check-format-time-parse [_c2 p2 m2]
  (let [normal-check (check-parse offset-time-parse "time" _c2 p2 m2)]
    (fn [_c1 p1 m1]
      (case (valid-leap-second-time? m1)
        true nil
        false [(make-error "format: not a valid time" p2 m2 p1 m1)]
        (normal-check _c1 p1 m1)))))

(defn check-format-json-pointer [_c2 p2 m2]
  (check-pattern json-pointer-pattern "json-pointer" _c2 p2 m2))

(defn check-format-relative-json-pointer [_c2 p2 m2]
  (check-pattern relative-pointer-pattern "relative-json-pointer" _c2 p2 m2))

(defn check-format-uri [_c2 p2 m2]
  (check-pattern uri-pattern "uri" _c2 p2 m2))

(defn check-format-uri-reference [_c2 p2 m2]
  (check-pattern uri-reference-pattern "uri-reference" _c2 p2 m2))

(defn check-format-uri-template [_c2 p2 m2]
  (check-pattern uri-template-pattern "uri-template" _c2 p2 m2))

(defn check-format-idn-email [_c2 p2 m2]
  (check-pattern idn-email-pattern "idn-email" _c2 p2 m2))

(defn check-format-idn-hostname [_c2 p2 m2]
  (fn [_c1 p1 m1]
    (when-not (json-idn-hostname? m1)
      [(make-error "format: not a valid idn-hostname:" p2 m2 p1 m1)])))

(defn check-format-iri [_c2 p2 m2]
  (check-pattern iri-pattern "iri" _c2 p2 m2))

(defn check-format-iri-reference [_c2 p2 m2]
  (check-pattern iri-reference-pattern "iri-reference" _c2 p2 m2))

(defn check-format-uuid [_c2 p2 m2]
  (check-pattern uuid-pattern "uuid" _c2 p2 m2))

(defn check-format-duration [_c2 p2 m2]
  (fn [_c1 p1 m1]
    (when-not (json-duration? m1)
      [(make-error "format: not a valid duration:" p2 m2 p1 m1)])))

(defn check-format-regex [_c2 p2 m2]
  (check-parse ecma-pattern "regex" _c2 p2 m2))

(def ^:private lookbehind-re #"\(\?<[=!]")

(defn check-format-regex-draft3 [_c2 p2 m2]
  (let [base-check (check-format-regex _c2 p2 m2)]
    (fn [_c1 p1 m1]
      (if (re-find lookbehind-re m1)
        [(make-error "format: not a valid regex (lookbehind not supported)" p2 m2 p1 m1)]
        (base-check _c1 p1 m1)))))

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
   "regex"        check-format-regex-draft3
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

(def draft->format->checker
  {:draft3        draft3-format->checker
   :draft4        draft4-format->checker
   :draft6        draft6-format->checker
   :draft7        draft7-format->checker
   :draft2019-09  draft2019-09-format->checker
   :draft2020-12  draft2020-12-format->checker
   :draft-next    draft-next-format->checker})
