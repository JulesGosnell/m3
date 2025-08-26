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

(defmulti check-format (fn [format _c2 _p2 _m2] format))

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
      (catch Exception e
        [(make-error (str "format: not a valid " f ": " (ex-message e)) p2 m2 p1 m1)]))))

;; standard formats

(defmethod check-format "host-name" [f c2 p2 m2]
  (check-pattern hostname-pattern f c2 p2 m2))

(defmethod check-format "ip-address" [f c2 p2 m2]
  (check-pattern ip-address-pattern f c2 p2 m2))

(defmethod check-format "color" [f c2 p2 m2]
  ;; TODO:
  ;; need to or this with a complete set of CSS2/CSS3 color names
  (check-pattern color-pattern f c2 p2 m2))

(defmethod check-format "email" [f c2 p2 m2]
  (check-pattern email-pattern f c2 p2 m2))

(defmethod check-format "ipv4" [f c2 p2 m2]
  (check-pattern ipv4-pattern f c2 p2 m2))

(defmethod check-format "ipv6" [f c2 p2 m2]
  (check-pattern ipv6-pattern f c2 p2 m2))

(defmethod check-format "hostname" [f c2 p2 m2]
  (check-pattern hostname-pattern f c2 p2 m2))

(defmethod check-format "date-time" [f c2 p2 m2]
  (check-parse offset-date-time-parse f c2 p2 m2))

(defmethod check-format "date" [f c2 p2 m2]
  (check-parse local-date-parse f c2 p2 m2))

(defmethod check-format "time" [f {d :draft :as c2} p2 m2]
  (if (= :draft3 d)
    (check-pattern time-pattern f c2 p2 m2)
    (check-parse offset-time-parse f c2 p2 m2)))

(defmethod check-format "json-pointer" [f c2 p2 m2]
  (check-pattern json-pointer-pattern f c2 p2 m2))

(defmethod check-format "relative-json-pointer" [f c2 p2 m2]
  (check-pattern relative-pointer-pattern f c2 p2 m2))

(defmethod check-format "uri" [f c2 p2 m2]
  (check-pattern uri-pattern f c2 p2 m2))

(defmethod check-format "uri-reference" [f c2 p2 m2]
  (check-pattern uri-reference-pattern f c2 p2 m2))

(defmethod check-format "uri-template" [f c2 p2 m2]
  (check-pattern uri-template-pattern f c2 p2 m2))

(defmethod check-format "idn-email" [f c2 p2 m2]
  (check-pattern idn-email-pattern f c2 p2 m2))

(defmethod check-format "idn-hostname" [_format _c2 p2 m2]
  (fn [_c1 p1 m1]
    (when-not (json-idn-hostname? m1)
      [(make-error "format: not a valid idn-hostname:" p2 m2 p1 m1)])))

(defmethod check-format "iri" [f c2 p2 m2]
  (check-pattern iri-pattern f c2 p2 m2))

(defmethod check-format "iri-reference" [f c2 p2 m2]
  (check-pattern iri-reference-pattern f c2 p2 m2))

(defmethod check-format "uuid" [f c2 p2 m2]
  (check-pattern uuid-pattern f c2 p2 m2))

(defn json-duration? [s]
  (boolean
   (when-let [[_p-t-or-w _ymdthms-or-w ymd _y _m _d thms _h _m _s w] (re-find json-duration-pattern s)]
     (not
      (or
       (and (empty? ymd) (empty? thms) (empty? w))
       (= "T" thms))))))

(defmethod check-format "duration" [_format _c2 p2 m2]
  (fn [_c1 p1 m1]
    (when-not (json-duration? m1)
      [(make-error "format: not a valid duration:" p2 m2 p1 m1)])))

(defmethod check-format "regex" [f c2 p2 m2]
  (check-parse ecma-pattern f c2 p2 m2))

(defmethod check-format "unknown" [_format _c2 _p2 _m2]
  (constantly nil))

(defmethod check-format :default [f _c2 _p2 _m2]
  (fn [_c1 _p1 _m1]
    (log/warn "format: not recognised:" (pr-str f))
    nil))
