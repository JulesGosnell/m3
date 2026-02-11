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

(ns m3.log-test
  (:require
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   [m3.log :as log]))

(deftest test-log-returns-nil
  (testing "all log functions return nil"
    (is (nil? (log/trace "t")))
    (is (nil? (log/info "i")))
    (is (nil? (log/warn "w")))
    (is (nil? (log/error "e")))))

(deftest test-log-level-filtering
  (testing "set-log-level! controls output"
    (let [output (atom [])]
      (with-redefs [println (fn [& args] (swap! output conj (first args)))]
        (log/set-log-level! :warn)
        (log/trace "t")
        (log/info "i")
        (log/warn "w")
        (log/error "e")
        (is (= ["WARN:" "ERROR:"] @output)
            "only warn and error should print at :warn level")))
    ;; restore default
    (log/set-log-level! :warn)))

(deftest test-log-level-off
  (testing ":off suppresses all output"
    (let [output (atom [])]
      (with-redefs [println (fn [& args] (swap! output conj (first args)))]
        (log/set-log-level! :off)
        (log/trace "t")
        (log/info "i")
        (log/warn "w")
        (log/error "e")
        (is (empty? @output) "nothing should print at :off level")))
    (log/set-log-level! :warn)))

(deftest test-log-level-trace
  (testing ":trace enables all output"
    (let [output (atom [])]
      (with-redefs [println (fn [& args] (swap! output conj (first args)))]
        (log/set-log-level! :trace)
        (log/trace "t")
        (log/info "i")
        (log/warn "w")
        (log/error "e")
        (is (= ["TRACE:" "INFO:" "WARN:" "ERROR:"] @output)
            "all levels should print at :trace level")))
    (log/set-log-level! :warn)))
