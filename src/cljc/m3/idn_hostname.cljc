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

;; Validates internationalized domain names (IDN hostnames) per IDNA2008/UTS#46.
;; Delegates base IDNA processing to ICU4J (JVM) / tr46 (CLJS), then applies
;; IDNA2008 exception checks that the libraries don't enforce.

(ns m3.idn-hostname
  (:require
   [clojure.string :as str]
   #?(:cljs ["tr46" :as tr46]))
  #?(:clj
     (:import
      [com.ibm.icu.text IDNA]
      [java.lang StringBuilder])))

;;------------------------------------------------------------------------------
;; Platform IDNA library

#?(:clj
   (def ^:private ^IDNA idna
     (IDNA/getUTS46Instance
      (bit-or IDNA/CHECK_BIDI
              IDNA/CHECK_CONTEXTJ
              IDNA/CHECK_CONTEXTO
              IDNA/NONTRANSITIONAL_TO_ASCII
              IDNA/NONTRANSITIONAL_TO_UNICODE))))

(defn- idna-valid?
  "Base IDNA validation via platform library. Checks Punycode, mapping,
   normalization, CONTEXTJ, CONTEXTO, BiDi, label length, and hyphen rules."
  [^String s]
  #?(:clj
     (let [info (com.ibm.icu.text.IDNA$Info.)
           _ (.nameToASCII idna s (StringBuilder.) info)]
       (not (.hasErrors info)))
     :cljs
     (some? (tr46/toASCII s #js {:checkBidi true
                                  :checkHyphens true
                                  :checkJoiners true
                                  :processingOption "nontransitional"
                                  :verifyDNSLength true}))))

;;------------------------------------------------------------------------------
;; IDNA2008 DISALLOWED exceptions (RFC 5892 Section 2.6)
;; These are characters that UTS#46 may accept but IDNA2008 disallows.

(def ^:private disallowed-exceptions
  #{0x0640  ;; ARABIC TATWEEL
    0x07FA  ;; NKO LAJANYALAN
    0x302E  ;; HANGUL SINGLE DOT TONE MARK
    0x302F  ;; HANGUL DOUBLE DOT TONE MARK
    0x3031  ;; VERTICAL KANA REPEAT MARK
    0x3032  ;; VERTICAL KANA REPEAT MARK WITH VOICED SOUND MARK
    0x3033  ;; VERTICAL KANA REPEAT MARK UPPER HALF
    0x3034  ;; VERTICAL KANA REPEAT MARK LOWER HALF
    0x3035  ;; VERTICAL KANA REPEAT DOUBLE-STRUCK
    0x303B  ;; VERTICAL IDEOGRAPHIC ITERATION MARK
    })

(defn- has-disallowed-exception?
  "Check if any code point in the string is a DISALLOWED exception."
  [^String s]
  #?(:clj (let [stream (.codePoints s)]
            (.. stream (anyMatch (reify java.util.function.IntPredicate
                                   (test [_ cp] (contains? disallowed-exceptions cp))))))
     :cljs (loop [i 0]
             (if (< i (count s))
               (let [cp (.codePointAt s i)]
                 (if (contains? disallowed-exceptions cp)
                   true
                   (recur (+ i (if (> cp 0xffff) 2 1)))))
               false))))

;;------------------------------------------------------------------------------
;; Per-label validation

(defn- to-unicode-label
  "Decode an ACE (xn--) label to Unicode, or return the label as-is."
  [^String label]
  #?(:clj (java.net.IDN/toUnicode label)
     :cljs (if (str/starts-with? (str/lower-case label) "xn--")
             (when-let [result (tr46/toUnicode label #js {})]
               (.-domain result))
             label)))

;;------------------------------------------------------------------------------
;; CONTEXTO rules (RFC 5892 Appendix A) — CLJS only
;; ICU4J handles these via CHECK_CONTEXTO; tr46 does not.

#?(:cljs
   (defn- string->codepoints
     "Convert a string to a vector of Unicode code points."
     [^String s]
     (loop [i 0, out (transient [])]
       (if (< i (count s))
         (let [cp (.codePointAt s i)]
           (recur (+ i (if (> cp 0xffff) 2 1)) (conj! out cp)))
         (persistent! out)))))

#?(:cljs
   (defn- script-of
     "Return the Unicode script of a code point as a keyword."
     [cp]
     (let [s (js/String.fromCodePoint cp)]
       (cond
         (.match s (js/RegExp "\\p{Script=Greek}" "u"))    :GREEK
         (.match s (js/RegExp "\\p{Script=Hebrew}" "u"))   :HEBREW
         (.match s (js/RegExp "\\p{Script=Hiragana}" "u")) :HIRAGANA
         (.match s (js/RegExp "\\p{Script=Katakana}" "u")) :KATAKANA
         (.match s (js/RegExp "\\p{Script=Han}" "u"))      :HAN
         :else nil))))

#?(:cljs
   (defn- check-contexto-rule
     "Check a single CONTEXTO code point in context. Returns true if valid."
     [cp codepoints idx]
     (case cp
       ;; MIDDLE DOT: must be between two 'l' (U+006C)
       0x00b7 (and (> idx 0) (< idx (dec (count codepoints)))
                   (= (nth codepoints (dec idx)) 0x006C)
                   (= (nth codepoints (inc idx)) 0x006C))
       ;; GREEK KERAIA: must be followed by a Greek character
       0x0375 (and (< idx (dec (count codepoints)))
                   (= :GREEK (script-of (nth codepoints (inc idx)))))
       ;; HEBREW GERESH / GERSHAYIM: must be preceded by a Hebrew character
       0x05f3 (and (> idx 0) (= :HEBREW (script-of (nth codepoints (dec idx)))))
       0x05f4 (and (> idx 0) (= :HEBREW (script-of (nth codepoints (dec idx)))))
       ;; KATAKANA MIDDLE DOT: label must contain Hiragana, Katakana, or Han
       0x30fb (some (fn [c]
                      (#{:HIRAGANA :KATAKANA :HAN} (script-of c)))
                    codepoints)
       ;; Not a CONTEXTO code point
       true)))

#?(:cljs
   (def ^:private contexto-codepoints #{0x00b7 0x0375 0x05f3 0x05f4 0x30fb}))

#?(:cljs
   (defn- check-contexto-label
     "Check all CONTEXTO rules in a Unicode label. Returns true if all pass."
     [^String label]
     (let [cps (string->codepoints label)]
       (loop [i 0]
         (if (>= i (count cps))
           true
           (let [cp (nth cps i)]
             (if (and (contains? contexto-codepoints cp)
                      (not (check-contexto-rule cp cps i)))
               false
               (recur (inc i)))))))))

;;------------------------------------------------------------------------------
;; Public API

(defn json-idn-hostname? [data]
  (if-not (string? data)
    true
    (try
      (let [s (str/replace data #"[\u3002\uff0e\uff61]" ".")]
        (and (not (str/blank? s))
             (not (str/starts-with? s "."))
             (not (str/ends-with? s "."))
             (not (str/includes? s ".."))
             (idna-valid? s)
             ;; Check IDNA2008 exceptions the library doesn't enforce:
             (not (some (fn [label]
                          (let [u-label (or (to-unicode-label label) label)]
                            (or (has-disallowed-exception? u-label)
                                ;; tr46 doesn't check CONTEXTO rules; ICU4J does
                                #?(:clj  false
                                   :cljs (not (check-contexto-label u-label))))))
                        (str/split s #"\.")))))
      (catch #?(:cljs js/Error :clj Exception) _
        false))))
