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

(ns m3.util
  (:require
   [m3.log :as log]
   ))

;;------------------------------------------------------------------------------

;(def absent (Object.))
(def absent :absent)

(defn absent? [v]
  (= absent v))

(defn present? [v]
  (not (absent? v)))

(def into-set (fnil into #{}))
(def conj-set (fnil conj #{}))

(defn concatv [& args]
  (vec (apply concat args)))

(defn seq-contains? [s p? v]
  (boolean (some (partial p? v) s)))

(defn when-assoc [c k v]
  (if v
    (assoc c k v)
    c))
