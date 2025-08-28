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

(ns m3.util)

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

;;------------------------------------------------------------------------------

(defn- topo-sort-by-2 [f x acc ks]
  (reduce
   (fn [[in out :as acc] k]
     (if-let [v (in k)]
       (let [e [k v]]
         (update (topo-sort-by-2 f x [(dissoc in k) out] (f e)) 1 conj (x e)))
       acc))
   acc
   ks))

(defn topo-sort-by [f x m]
  (second (topo-sort-by-2 f x [m []] (keys m))))

;;------------------------------------------------------------------------------

(defn make-error [message schema-path schema document-path document]
  {:schema-path schema-path :message (str message " - " (pr-str document)) :document-path document-path :document document :schema schema})

(defn make-error-on [message schema-path schema document-path document failed? errors]
  (when (failed? errors)
    [(assoc (make-error message schema-path schema document-path document) :errors errors)]))

(defn make-error-on-failure [message schema-path schema document-path document errors]
  (make-error-on message schema-path schema document-path document seq errors))
