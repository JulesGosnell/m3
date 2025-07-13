(ns m3.idn-hostname
  (:require [clojure.string :as str])
  (:import [java.net IDN]
           [java.text Normalizer Normalizer$Form]
           [java.lang Character StringBuilder Character$UnicodeScript]))

(def exceptions {
  0x00df :pvalid
  0x03c2 :pvalid
  0x06fd :pvalid
  0x06fe :pvalid
  0x0f0b :pvalid
  0x3007 :pvalid
  0x0640 :disallowed
  0x07fa :disallowed
  0x302e :disallowed
  0x302f :disallowed
  0x3031 :disallowed
  0x3032 :disallowed
  0x3033 :disallowed
  0x3034 :disallowed
  0x3035 :disallowed
  0x303b :disallowed
  })

(def context-map {
  0x200c :contextj
  0x200d :contextj
  0x00b7 :contexto
  0x0375 :contexto
  0x05f3 :contexto
  0x05f4 :contexto
  0x30fb :contexto
  })

(def case-fold-map {
  ;; the full case-fold-map as before
  })

(def joining-type-map {
  0x0620 "U"
  0x0621 "U"
  0x0622 "R"
  0x0623 "R"
  0x0624 "R"
  0x0625 "R"
  0x0626 "D"
  0x0627 "R"
  0x0628 "D"
  0x0629 "R"
  0x062A "D"
  0x062B "D"
  0x062C "D"
  0x062D "D"
  0x062E "D"
  0x062F "R"
  0x0630 "R"
  0x0631 "R"
  0x0632 "R"
  0x0633 "D"
  0x0634 "D"
  0x0635 "D"
  0x0636 "D"
  0x0637 "D"
  0x0638 "D"
  0x0639 "D"
  0x063A "D"
  0x063B "D"
  0x063C "D"
  0x063D "D"
  0x063E "D"
  0x063F "D"
  0x0641 "D"
  0x0642 "D"
  0x0643 "D"
  0x0644 "D"
  0x0645 "D"
  0x0646 "D"
  0x0647 "D"
  0x0648 "R"
  0x0649 "D"
  0x064A "D"
  })

(def ccc-map {
  0x094d 9
  0x09CD 9
  0x0A4D 9
  0x0ACD 9
  0x0B4D 9
  0x0BCD 9
  0x0C4D 9
  0x0CCD 9
  0x0D4D 9
  0x0DCD 9
  0x0F84 9
  })

(defn joining-type [cp]
  (get joining-type-map cp "U"))

(defn string-to-codepoints [s]
  (let [result (transient [])]
    (loop [i 0]
      (if (< i (.length s))
        (let [cp (.codePointAt s i)]
          (conj! result cp)
          (recur (+ i (Character/charCount cp))))
        (persistent! result)))))

(defn codepoints-to-string [cps]
  (let [sb (StringBuilder.)]
    (doseq [cp cps]
      (.appendCodePoint sb cp))
    (.toString sb)))

(defn noncharacter? [cp]
  (or (and (<= 0xfdd0 cp 0xfdef))
      (let [low (bit-and cp 0xffff)]
        (and (<= 0 (bit-shift-right cp 16) 0x10) (or (= low 0xfffe) (= low 0xffff))))))

(defn ignorable-properties? [cp]
  (or (Character/isWhitespace cp)
      (= (Character/getType cp) Character/FORMAT)
      (noncharacter? cp)))

(defn ignorable-blocks? [cp]
  (or (and (<= 0x20d0 cp 0x20ff))
      (and (<= 0x1d100 cp 0x1d1ff))
      (and (<= 0x1d200 cp 0x1d24f))))

(defn letter-digit? [cp]
  (let [type (Character/getType cp)]
    (or (= type Character/UPPERCASE_LETTER)
        (= type Character/LOWERCASE_LETTER)
        (= type Character/OTHER_LETTER)
        (= type Character/DECIMAL_DIGIT_NUMBER)
        (= type Character/MODIFIER_LETTER)
        (= type Character/NON_SPACING_MARK)
        (= type Character/COMBINING_SPACING_MARK))))

(defn case-fold [s]
  (let [sb (StringBuilder.)
        len (.length s)]
    (loop [i 0]
      (if (< i len)
        (let [cp (.codePointAt s i)
              folded (or (get case-fold-map cp) [(Character/toLowerCase cp)])]
          (doseq [f folded]
            (.appendCodePoint sb f))
          (recur (+ i (Character/charCount cp))))
        (.toString sb)))))

(defn unstable? [cp]
  (let [s (codepoints-to-string [cp])
        nf1 (Normalizer/normalize s Normalizer$Form/NFKC)
        cf (case-fold nf1)
        nf2 (Normalizer/normalize cf Normalizer$Form/NFKC)]
    (not (= nf2 s))))

(defn get-derived-property [cp]
  (or (get context-map cp)
      (get exceptions cp)
      (if-not (Character/isDefined cp) :unassigned
              (if (unstable? cp) :disallowed
                  (if (ignorable-properties? cp) :disallowed
                      (if (ignorable-blocks? cp) :disallowed
                          (if (or (letter-digit? cp) (= cp 0x002D)) :pvalid
                              :disallowed)))))))

(defn check-context-rule [cp codepoints idx]
  (case cp
    0x200d (and (> idx 0) (= 9 (get ccc-map (nth codepoints (dec idx)) 0)))
    0x200c (or (and (> idx 0) (= 9 (get ccc-map (nth codepoints (dec idx)) 0)))
                (let [left-idx (loop [j (dec idx)]
                                 (cond
                                   (< j 0) nil
                                   (= "T" (joining-type (nth codepoints j))) (recur (dec j))
                                   :else j))
                      right-idx (loop [j (inc idx)]
                                  (cond
                                    (>= j (count codepoints)) nil
                                    (= "T" (joining-type (nth codepoints j))) (recur (inc j))
                                    :else j))]
                  (when (and left-idx right-idx)
                    (let [left-jt (joining-type (nth codepoints left-idx))
                          right-jt (joining-type (nth codepoints right-idx))]
                      (and (or (= "R" left-jt) (= "D" left-jt))
                           (or (= "L" right-jt) (= "D" right-jt)))))))
    0x00b7 (and (> idx 0) (< idx (dec (count codepoints)))
                 (= (nth codepoints (dec idx)) 0x006C)
                 (= (nth codepoints (inc idx)) 0x006C))
    0x0375 (and (< idx (dec (count codepoints)))
                 (= Character$UnicodeScript/GREEK (Character$UnicodeScript/of (nth codepoints (inc idx)))))
    0x05f3 (and (> idx 0)
                 (= Character$UnicodeScript/HEBREW (Character$UnicodeScript/of (nth codepoints (dec idx)))))
    0x05f4 (and (> idx 0)
                 (= Character$UnicodeScript/HEBREW (Character$UnicodeScript/of (nth codepoints (dec idx)))))
    0x30fb (some (fn [c] (and (not= c 0x30fb)
                              (let [script (Character$UnicodeScript/of c)]
                                (or (= script Character$UnicodeScript/HIRAGANA)
                                    (= script Character$UnicodeScript/KATAKANA)
                                    (= script Character$UnicodeScript/HAN))))) codepoints)
    false))

(defn validate-u-label [label]
  (let [codepoints (string-to-codepoints label)]
    (when-not (empty? codepoints)
      (let [first-type (Character/getType (first codepoints))]
        (when-not (or (= first-type Character/NON_SPACING_MARK)
                      (= first-type Character/ENCLOSING_MARK)
                      (= first-type Character/COMBINING_SPACING_MARK))
          (let [arabic? (some #(<= 0x0660 % 0x0669) codepoints)
                ext? (some #(<= 0x06f0 % 0x06f9) codepoints)]
            (when-not (and arabic? ext?)
              (and (not (and (>= (count label) 4) (= (subs label 2 4) "--")))
                   (every? (fn [[idx prop]]
                             (or (= :pvalid prop)
                                 (and (#{:contextj :contexto} prop)
                                      (check-context-rule (nth codepoints idx) codepoints idx))))
                           (map-indexed vector (map get-derived-property codepoints)))))))))))

(defn validate-label [label]
  (and (not (str/blank? label))
       (<= (count label) 63)
       (not (str/starts-with? label "-"))
       (not (str/ends-with? label "-"))
       (if (str/starts-with? (str/lower-case label) "xn--")
         (try
           (let [u-label (IDN/toUnicode label)
                 encoded (IDN/toASCII u-label)]
             (and (= label (str/lower-case encoded))
                  (validate-u-label u-label)))
           (catch Exception _ false))
         (try
           (let [a-label (IDN/toASCII label)]
             (and (<= (count a-label) 63)
                  (validate-u-label label)))
           (catch Exception _ false)))))

(defn json-idn-hostname? [data]
  (if-not (string? data)
    true
    (try
      (let [s (str/replace data #"[\u3002\uff0e\uff61]" ".")]
        (and (not (str/blank? s))
             (not (str/starts-with? s "."))
             (not (str/ends-with? s "."))
             (not (str/includes? s ".."))
             (every? validate-label (str/split s #"\."))))
      (catch Exception _
        false))))
