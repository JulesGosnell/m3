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

(ns m3.platform
  (:require
   #?(:clj [clojure.data.json :as json]
      :cljs [cljs.core :as cljs])
   #?(:cljs [goog.string :as gstring])
   #?(:cljs [goog.string.format])
   ))

;;------------------------------------------------------------------------------

#?(:cljs (defn pformat [fmt & args] (apply gstring/format fmt args))
   :clj  (def pformat clojure.core/format))


;;------------------------------------------------------------------------------

#?(:cljs
   (defn deep-js->clj [x]
     (cond
       (.isArray js/Array x) (into [] (map deep-js->clj) x)
       (= (goog.typeOf x) "object") (into {} (map #(vector (aget % 0) (deep-js->clj (aget % 1))))
                                          (.entries js/Object x))
       :else x)))

(defn json-decode [s]
  #?(:clj (json/read-str s)
     :cljs (deep-js->clj (js/JSON.parse s))))

(def big-zero?
  #?(:cljs (fn [^js/Big b] (.eq b 0))
     :clj zero?))

#?(:cljs
   (def Big (js/require "big.js")))

#?(:cljs
   (defn pbigdec [x]
     (Big. (str x)))
   :clj (def pbigdec bigdec)
   ) ;; Big constructor takes a string or number

(def big-mod
  #?(:cljs (fn [^js/Big l ^js/Big r] (.mod l r))
     :clj mod))

