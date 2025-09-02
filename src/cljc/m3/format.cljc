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
  "Format validation for JSON Schema - extracted from validate.cljc"
  (:require
   [cljc.java-time.local-date :refer [parse] :rename {parse local-date-parse}]
   [cljc.java-time.offset-date-time :refer [parse] :rename {parse offset-date-time-parse}]
   [cljc.java-time.offset-time :refer [parse] :rename {parse offset-time-parse}]
   [#?(:clj clojure.tools.logging :cljs m3.log) :as log]
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

;;------------------------------------------------------------------------------
;; Individual format checker functions

(defn check-format-host-name [_c2 p2 m2]
  (check-pattern hostname-pattern "host-name" _c2 p2 m2))

(defn check-format-ip-address [_c2 p2 m2]
  (check-pattern ip-address-pattern "ip-address" _c2 p2 m2))

(defn check-format-color [_c2 p2 m2]
  ;; TODO: need to or this with a complete set of CSS2/CSS3 color names
  (check-pattern color-pattern "color" _c2 p2 m2))

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
  (check-parse offset-date-time-parse "date-time" _c2 p2 m2))

(defn check-format-date [_c2 p2 m2]
  (check-parse local-date-parse "date" _c2 p2 m2))

(defn check-format-time-draft3 [c2 p2 m2]
  (check-pattern time-pattern "time" c2 p2 m2))

(defn check-format-time-standard [_c2 p2 m2]
  (check-parse offset-time-parse "time" _c2 p2 m2))

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

(defn check-format-unknown [_c2 _p2 _m2]
  (constantly nil))

;;------------------------------------------------------------------------------
;; Table mapping drafts to format names to checker functions

(def draft->format->checker
  {:draft3 {"date-time" check-format-date-time
            "date" check-format-date
            "time" check-format-time-draft3
            "utc-millisec" check-format-utc-millisec
            "regex" check-format-regex
            "color" check-format-color
            "style" check-format-style
            "phone" check-format-phone
            "uri" check-format-uri
            "email" check-format-email
            "ip-address" check-format-ip-address
            "ipv6" check-format-ipv6
            "host-name" check-format-host-name}

   :draft4 {"date-time" check-format-date-time
            "email" check-format-email
            "hostname" check-format-hostname
            "ipv4" check-format-ipv4
            "ipv6" check-format-ipv6
            "uri" check-format-uri}

   :draft6 {"date-time" check-format-date-time
            "email" check-format-email
            "hostname" check-format-hostname
            "ipv4" check-format-ipv4
            "ipv6" check-format-ipv6
            "uri" check-format-uri
            "uri-reference" check-format-uri-reference
            "uri-template" check-format-uri-template
            "json-pointer" check-format-json-pointer}

   :draft7 {"date-time" check-format-date-time
            "date" check-format-date
            "time" check-format-time-standard
            "email" check-format-email
            "idn-email" check-format-idn-email
            "hostname" check-format-hostname
            "idn-hostname" check-format-idn-hostname
            "ipv4" check-format-ipv4
            "ipv6" check-format-ipv6
            "uri" check-format-uri
            "uri-reference" check-format-uri-reference
            "iri" check-format-iri
            "iri-reference" check-format-iri-reference
            "uri-template" check-format-uri-template
            "json-pointer" check-format-json-pointer
            "relative-json-pointer" check-format-relative-json-pointer
            "regex" check-format-regex}

   :draft2019-09 {"date-time" check-format-date-time
                  "date" check-format-date
                  "time" check-format-time-standard
                  "duration" check-format-duration
                  "email" check-format-email
                  "idn-email" check-format-idn-email
                  "hostname" check-format-hostname
                  "idn-hostname" check-format-idn-hostname
                  "ipv4" check-format-ipv4
                  "ipv6" check-format-ipv6
                  "uri" check-format-uri
                  "uri-reference" check-format-uri-reference
                  "iri" check-format-iri
                  "iri-reference" check-format-iri-reference
                  "uri-template" check-format-uri-template
                  "json-pointer" check-format-json-pointer
                  "relative-json-pointer" check-format-relative-json-pointer
                  "regex" check-format-regex
                  "uuid" check-format-uuid}

   :draft2020-12 {"date-time" check-format-date-time
                  "date" check-format-date
                  "time" check-format-time-standard
                  "duration" check-format-duration
                  "email" check-format-email
                  "idn-email" check-format-idn-email
                  "hostname" check-format-hostname
                  "idn-hostname" check-format-idn-hostname
                  "ipv4" check-format-ipv4
                  "ipv6" check-format-ipv6
                  "uri" check-format-uri
                  "uri-reference" check-format-uri-reference
                  "iri" check-format-iri
                  "iri-reference" check-format-iri-reference
                  "uri-template" check-format-uri-template
                  "json-pointer" check-format-json-pointer
                  "relative-json-pointer" check-format-relative-json-pointer
                  "regex" check-format-regex
                  "uuid" check-format-uuid}

   :draft-next {"date-time" check-format-date-time
                "date" check-format-date
                "time" check-format-time-standard
                "duration" check-format-duration
                "email" check-format-email
                "idn-email" check-format-idn-email
                "hostname" check-format-hostname
                "idn-hostname" check-format-idn-hostname
                "ipv4" check-format-ipv4
                "ipv6" check-format-ipv6
                "uri" check-format-uri
                "uri-reference" check-format-uri-reference
                "iri" check-format-iri
                "iri-reference" check-format-iri-reference
                "uri-template" check-format-uri-template
                "json-pointer" check-format-json-pointer
                "relative-json-pointer" check-format-relative-json-pointer
                "regex" check-format-regex
                "uuid" check-format-uuid}})

;;------------------------------------------------------------------------------
;; Legacy multimethod support (to be removed)

(defmulti check-format (fn [format _c2 _p2 _m2] format))

(defmethod check-format :default [f _c2 _p2 _m2]
  (fn [_c1 _p1 _m1]
    (log/warn "format: not recognised via multimethod:" (pr-str f))
    nil))
