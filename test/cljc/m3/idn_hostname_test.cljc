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

(ns m3.idn-hostname-test
  (:require
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   [m3.idn-hostname :refer [json-idn-hostname?]]))


(deftest test-json-idn-hostname?
  (is (= (json-idn-hostname? "〱〲〳〴〵〮〯〻") false))
  (is (= (json-idn-hostname? "-> $1.00 <--") false))
  (is (= (json-idn-hostname? "۰0") true))
  (is (= (json-idn-hostname? "1host") true))
  (is (= (json-idn-hostname? "a·l") false))
  (is (= (json-idn-hostname? "A׳ב") false))
  (is (= (json-idn-hostname? "A״ב") false))
  (is (= (json-idn-hostname? "def・abc") false))
  (is (= (json-idn-hostname? "실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실실례례테스트례례례례례례례례례례례례례례례례례테스트례례례례례례례례례례례례례례례례례례례테스트례례례례례례례례례례례례테스트례례실례.테스트") false))
  (is (= (json-idn-hostname? "") false))
  (is (= (json-idn-hostname? ".") false))
  (is (= (json-idn-hostname? "ـߺ") false))
  (is (= (json-idn-hostname? "・") false))
  (is (= (json-idn-hostname? "실〮례.테스트") false))
  (is (= (json-idn-hostname? "〮실례.테스트") false))
  (is (= (json-idn-hostname? "h0stn4me") true))
  (is (= (json-idn-hostname? "-hello") false))
  (is (= (json-idn-hostname? "-hello-") false))
  (is (= (json-idn-hostname? "҈hello") false))
  (is (= (json-idn-hostname? "hello-") false))
  (is (= (json-idn-hostname? "̀hello") false))
  (is (= (json-idn-hostname? "ःhello") false))
  (is (= (json-idn-hostname? "hostnam3") true))
  (is (= (json-idn-hostname? "host-name") true))
  (is (= (json-idn-hostname? "hostname") true))
  (is (= (json-idn-hostname? "l·a") false))
  (is (= (json-idn-hostname? "·l") false))
  (is (= (json-idn-hostname? "l·") false))
  (is (= (json-idn-hostname? "l·l") true))
  (is (= (json-idn-hostname? "ßς་〇") true))
  (is (= (json-idn-hostname? "실례.테스트") true))
  (is (= (json-idn-hostname? "XN--aa---o47jg78q") false))
  (is (= (json-idn-hostname? "xn--ihqwcrb4cv8a8dqg056pqjye") true))
  (is (= (json-idn-hostname? "xn--X") false))
  (is (= (json-idn-hostname? "α͵") false))
  (is (= (json-idn-hostname? "α͵S") false))
  (is (= (json-idn-hostname? "α͵β") true))
  (is (= (json-idn-hostname? "א׳ב") true))
  (is (= (json-idn-hostname? "א״ב") true))
  (is (= (json-idn-hostname? "׳ב") false))
  (is (= (json-idn-hostname? "״ב") false))
  (is (= (json-idn-hostname? "۽۾") true))
  (is (= (json-idn-hostname? "ب٠۰") false))
  (is (= (json-idn-hostname? "ب٠ب") true))
  (is (= (json-idn-hostname? "بي‌بي") true))
  (is (= (json-idn-hostname? "क‍ष") false))
  (is (= (json-idn-hostname? "क्‌ष") true))
  (is (= (json-idn-hostname? "क्‍ष") true))
  (is (= (json-idn-hostname? "‍ष") false))
  (is (= (json-idn-hostname? "・ぁ") true))
  (is (= (json-idn-hostname? "・ァ") true))
  (is (= (json-idn-hostname? "・丈") true)))
