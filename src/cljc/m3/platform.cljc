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
   [m3.log :refer []]
   #?(:cljs [goog.string :as gstring])
   #?(:cljs [goog.string.format])
   ))

;;------------------------------------------------------------------------------

#?(:cljs (defn pformat [fmt & args] (apply gstring/format fmt args))
   :clj  (def pformat clojure.core/format))


