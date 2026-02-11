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

(ns m3.log)

;; Level-filtered logging for CLJS (CLJ uses clojure.tools.logging).
;; Levels: :trace < :info < :warn < :error < :off

(def ^:private level->int
  {:trace 0 :info 1 :warn 2 :error 3 :off 4})

(defonce ^:private log-level (atom :warn))

(defn set-log-level!
  "Set the minimum log level. One of :trace :info :warn :error :off"
  [level]
  (reset! log-level level))

(defn- log? [level]
  (>= (level->int level) (level->int @log-level)))

(defn trace [& args]
  (when (log? :trace)
    (apply println "TRACE:" (mapv pr-str args))))

(defn info [& args]
  (when (log? :info)
    (apply println "INFO:" (mapv pr-str args))))

(defn warn [& args]
  (when (log? :warn)
    (apply println "WARN:" (mapv pr-str args))))

(defn error [& args]
  (when (log? :error)
    (apply println "ERROR:" (mapv pr-str args))))
