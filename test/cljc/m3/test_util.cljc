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

(ns m3.test-util
  [:require
   [clojure.test :refer [is]]
   [clojure.pprint :refer [pprint]]])

;;------------------------------------------------------------------------------

(defmacro is= [& args]
  `(is (= ~@args)))

;;------------------------------------------------------------------------------

(defn is-valid [{v? :valid? e :errors :as report}]
  (is v? (with-out-str (pprint e)))
  report)

(defn is-invalid [{v? :valid? e :errors :as report}]
  (is (not v?) (with-out-str (pprint e)))
  report)

;;------------------------------------------------------------------------------

