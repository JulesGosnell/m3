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

(defn info [& args]
  (apply println "INFO:" (mapv pr-str args)))

(defn warn [& args]
  (apply println "WARN:" (mapv pr-str args)))

(defn trace [& args]
  (apply println "TRACE:" (mapv pr-str args)))

(defn error [& args]
  (apply println "ERROR:" (mapv pr-str args)))
