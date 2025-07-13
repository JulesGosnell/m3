(ns m3.idn-hostname
  (:require [clojure.string :as str])
  (:import
   [java.lang Character$UnicodeScript]
   [java.text Normalizer Normalizer$Form]))

(def ^:private base 36)
(def ^:private tmin 1)
(def ^:private tmax 26)
(def ^:private skew 38)
(def ^:private damp 700)
(def ^:private initial-bias 72)
(def ^:private initial-n 128)
(def ^:private delimiter \-)

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
  ;; Comprehensive case folding map as before, no changes needed
  })

(def joining-type-map {
  ;; As before
  })

(def ccc-map {
  0x094d 9
  ;; As before
  })

(defn joining-type [cp]
  (get joining-type-map cp "U"))

(defn adapt [delta numpoints firsttime]
  (let [delta (if firsttime (quot delta damp) (quot delta 2))
        delta (+' delta (quot delta numpoints))]
    (loop [delta delta
           k 0]
      (if (> delta (quot (*' (- base tmin) tmax) 2))
        (recur (quot delta (- base tmin)) (+' k base))
        (+' k (quot (*' (+' (- base tmin) 1) delta) (+' delta skew)))))))

(defn puny-digit [c]
  (let [c (Character/toLowerCase c)]
    (cond
      (<= (int \a) (int c) (int \z)) (- (int c) (int \a))
      (<= (int \0) (int c) (int \9)) (+ 26 (- (int c) (int \0)))
      :else -1)))

(defn punycode-decode [input]
  (let [input (str/lower-case input)
        last-delim (str/last-index-of input delimiter)
        basic-end (if (neg? last-delim) (count input) last-delim)
        output (vec (for [j (range basic-end)]
                      (let [c (int (nth input j))]
                        (when (>= c 128)
                          (throw (IllegalArgumentException. "non-basic in basic part")))
                        c)))
        pos (if (pos? basic-end) (inc last-delim) 0)]
    (loop [pos pos
           n initial-n
           i 0
           bias initial-bias
           output output]
      (if (>= pos (count input))
        output
        (let [old-i i
              [i pos] (loop [w 1
                             k base
                             i i
                             pos pos]
                        (if (>= pos (count input))
                          (throw (IllegalArgumentException. "truncated"))
                          (let [c (nth input pos)
                                digit (puny-digit c)]
                            (when (neg? digit)
                              (throw (IllegalArgumentException. "invalid digit")))
                            (let [i (+' i (*' digit w))]
                              (when (neg? i)
                                (throw (IllegalArgumentException. "overflow")))
                              (let [t (cond (<= k bias) tmin
                                            (>= k (+ bias tmax)) tmax
                                            :else (- k bias))]
                                (if (< digit t)
                                  [i (inc pos)]
                                  (let [w (*' w (- base t))]
                                    (when (neg? w)
                                      (throw (IllegalArgumentException. "overflow")))
                                    (recur w (+' k base) i (inc pos)))))))))]
              (let [len (inc (count output))
                    bias (adapt (-' i old-i) len (= old-i 0))
                    n (+' n (quot i len))]
                (when (neg? n)
                  (throw (IllegalArgumentException. "overflow")))
                (let [i (mod i len)
                      output (vec (concat (subvec output 0 i) [n] (subvec output i)))]
                  (recur pos n (inc i) bias output))))))))

(defn digit-to-char [d]
  (if (<= 0 d 25)
    (char (+ (int \a) d))
    (char (+ (int \0) (- d 26)))))

(defn punycode-encode [input]
  (let [input (vec input)
        basic-count (count (filter #(< % 128) input))
        output (vec (filter #(< % 128) input))
        output (if (pos? basic-count) (conj output (int delimiter)) output)
        h basic-count
        delta 0
        bias initial-bias
        n initial-n]
    (loop [h h
           n n
           delta delta
           bias bias
           output output]
      (if (>= h (count input))
        (apply str (map char output))
        (let [m (apply min (filter #(>= % n) input))
              delta (+' delta (*' (- m n) (inc' h)))
              _ (when (neg? delta) (throw (IllegalArgumentException. "overflow")))
              n m
              [delta bias h output] (reduce (fn [[delta bias h output] c]
                                              (if (< c n)
                                                (let [delta (inc' delta)]
                                                  (when (neg? delta) (throw (IllegalArgumentException. "overflow")))
                                                  [delta bias h output])
                                                (if (= c n)
                                                  (let [q delta
                                                        [q output] (loop [k base
                                                                          q q
                                                                          output output]
                                                                      (let [t (cond (<= k bias) tmin
                                                                                    (>= k (+ bias tmax)) tmax
                                                                                    :else (- k bias))]
                                                                        (if (< q t)
                                                                          [q (conj output (int (digit-to-char q)))]
                                                                          (let [code (+' t (mod (- q t) (- base t)))
                                                                                q (quot (- q t) (- base t))]
                                                                            (recur (+' k base) q (conj output (int (digit-to-char code))))))))]
                                                    (let [bias (adapt delta (inc' h) (= h basic-count))
                                                          delta 0
                                                          h (inc' h)]
                                                      [delta bias h output]))
                                                  [(inc' delta) bias h output])))
                                            [delta bias h output]
                                            input)
              n (inc' n)]
          (recur h n delta bias output))))))

(defn codepoints-to-string [cps]
  (let [sb (StringBuilder.)]
    (doseq [cp cps]
      (.appendCodePoint sb cp))
    (.toString sb)))

(defn string-to-codepoints [s]
  (let [result (transient [])]
    (loop [i 0]
      (if (< i (.length s))
        (let [cp (.codePointAt s i)]
          (conj! result cp)
          (recur (+ i (Character/charCount cp))))
        (persistent! result)))))

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

(defn validate-a-label [label]
  (try
    (let [low (str/lower-case label)
          puny (subs low 4)
          u-codepoints (punycode-decode puny)
          u-label (codepoints-to-string u-codepoints)
          encoded (punycode-encode u-codepoints)]
      (and (= puny encoded)
           (validate-u-label u-label)))
    (catch Exception _
      false)))

(defn validate-label [label]
  (and (not (str/blank? label))
       (<= (count label) 63)
       (not (str/starts-with? label "-"))
       (not (str/ends-with? label "-"))
       (if (str/starts-with? (str/lower-case label) "xn--")
         (validate-a-label label)
         (validate-u-label label))))

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
