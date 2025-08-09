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

(ns m3.ref
  (:require
   [clojure.string :refer [split replace] :rename {replace string-replace}] 
   [#?(:clj clojure.tools.logging :cljs m3.log) :as log]
   [m3.util :refer [present? absent]]
   [m3.uri :refer [parse-uri inherit-uri uri-base uri-fragment]]))


(defn parse-int [s]
  #?(:clj  (Integer/parseInt s)
     :cljs (js/parseInt s)))

(defn ->int-or-string [s]
  (if (re-matches #"[0-9].*" s)
    (parse-int s)
    s))

(defn uri-decode
  "URI-decodes a percent-encoded string."
  [s]
  #?(:clj  (java.net.URLDecoder/decode s "UTF-8")
     :cljs (try
             (js/decodeURIComponent s)
             (catch :default e
               (throw (ex-info "Invalid percent-encoding in URI" {:cause e}))))))

(defn unescape [s]
  (-> s
      (string-replace "~0" "~")
      (string-replace "~1" "/")))

;; or should we just walk the data structure...
(defn canonicalise [path $ref]
  (reduce
   (fn [acc r]
     (let [r (unescape (uri-decode r))]
       (case r
         "" (if (= acc path) [] (conj acc ""))
         "." acc
         ".." (vec (take (dec (count acc)) acc))
         (conj acc (->int-or-string r)))))
   path
   (or (seq (if (= $ref "/") [""] (split $ref #"/" -1))) [""]))) ;; N.B.: -1 prevents trailing ""s being removed

(defn try-path [{t? :trace? path->uri :path->uri :as ctx} path p root]
  (when p
    (let [m (get-in root p absent)]
      (when (present? m)
        (when t? (prn "resolved:" path "->" p))
        [;; use static (original) base uri rather than dynamic (inherited from ref)
         (assoc ctx :id-uri (path->uri p))
         path m]))))

(declare resolve-$ref)

(defn resolve-uri [{m2 :root uri->path :uri->path uri->schema :uri->schema :as ctx} path {t :type f :fragment :as uri} $ref]

  ;;(prn "resolve-uri:" uri $ref)

  (or

   ;; did it match a stashed [$]id or $.*anchor ?

   ;; exactly
   (try-path ctx path (uri->path uri) m2)

   ;; in all but fragment
   (when-let [path2 (uri->path (uri-base uri))]
     (or
      (try-path ctx path (canonicalise path2 f) m2)
      ;; why ?
      (try-path ctx path (concat path2 (canonicalise [] f)) m2)))

   ;; it's just a fragment
   (and (= t :fragment)
        (try-path ctx path (canonicalise path (or f "")) m2)) ;TODO

   ;; did it match a remote schema
   (when-let [[c p _m] (and uri->schema (uri->schema ctx path uri))]

     ;; this is how I would like it to work:
     
     ;; TODO: risk of a stack overflow here if uri does not resolve in context of remote schema...
     ;; (try
     ;;   (resolve-uri c p uri (str "#" (:fragment uri)))
     ;;   (catch StackOverflowError _
     ;;     (log/error "OVERFLOW:" (pr-str [uri (:uri->path c) c]))
     ;;     ))

     ;; but this is safer
     (resolve-uri c p (uri-fragment uri) (str "#" (:fragment uri)))
     )

   (log/warn "$ref: could not resolve:" (pr-str $ref) (pr-str uri)))

  )

;;------------------------------------------------------------------------------
;; expanding $refs

(defmulti meld (fn [{d :draft} parent reffed] d))

;; adapted from merge-with to pass k as well as vals to f
(defn meld-with
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if (contains? m k)
			    (assoc m k (f k (get m k) v))
			    (assoc m k v))))
          merge2 (fn [m1 m2]
		   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defn deep-meld [{id-key :id-key} & maps]
  (reduce
   (fn [acc m]
     (meld-with
      (fn choose-val [k val-in-result val-in-latter]
        (if (and (map? val-in-result) (map? val-in-latter))
          (meld-with choose-val val-in-result val-in-latter)
          (if (= id-key k) val-in-result val-in-latter)))
      acc m))
   maps))

(defn meld-replace    [ctx parent reffed] (if reffed reffed false))
(defn meld-deep-over  [ctx parent reffed] (if (and (map? reffed)(map? parent)) (deep-meld ctx parent reffed) reffed))

(defmethod meld :draft3         [ctx parent reffed] (meld-replace   ctx parent reffed))
(defmethod meld :draft4         [ctx parent reffed] (meld-replace   ctx parent reffed))
(defmethod meld :draft6         [ctx parent reffed] (meld-replace   ctx parent reffed))
(defmethod meld :draft7         [ctx parent reffed] (meld-replace   ctx parent reffed))
(defmethod meld :draft2019-09   [ctx parent reffed] (meld-deep-over ctx parent reffed))
(defmethod meld :draft2020-12   [ctx parent reffed] (meld-deep-over ctx parent reffed))
(defmethod meld :draft-next     [ctx parent reffed] (meld-deep-over ctx parent reffed))
(defmethod meld :latest         [ctx parent reffed] (meld-deep-over ctx parent reffed))

;;------------------------------------------------------------------------------



