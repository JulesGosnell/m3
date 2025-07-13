(ns m3.idn-hostname-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.string :as str]
   [m3.idn-hostname :as idn]))

(deftest validate-label-test
  (testing "Valid ASCII with hyphen"
    (is (idn/validate-label "host-name")))
  (testing "Invalid starting with hyphen"
    (is (not (idn/validate-label "-host"))))
  (testing "Invalid ending with hyphen"
    (is (not (idn/validate-label "host-"))))
  (testing "Zero width joiner preceded by virama"
    (is (idn/validate-label (str "\u0915\u094d\u200d\u0937"))))
  (testing "Zero width non-joiner preceded by virama"
    (is (idn/validate-label (str "\u0915\u094d\u200c\u0937"))))
  (testing "Zero width non-joiner not preceded by virama but matches regexp"
    (is (idn/validate-label (str "\u0628\u064a\u200c\u0628\u064a"))))
  (testing "Chinese U-label"
    (is (idn/validate-label (idn/codepoints-to-string [20182 20204 20026 20160 20040 19981 35828 20013 25991]))))
  (testing "Valid Chinese Punycode"
    (is (idn/validate-label "xn--ihqwcrb4cv8a8dqg056pqjye"))))

(deftest json-idn-hostname-test
  (testing "Valid ASCII hostname"
    (is (idn/json-idn-hostname? "example.com")))
  (testing "Valid with hyphen"
    (is (idn/json-idn-hostname? "host-name.com")))
  (testing "Invalid double dot"
    (is (not (idn/json-idn-hostname? "example..com"))))
  (testing "Valid Chinese Punycode"
    (is (idn/json-idn-hostname? "xn--ihqwcrb4cv8a8dqg056pqjye")))
  (testing "Zero width joiner preceded by virama"
    (is (idn/json-idn-hostname? (str "\u0915\u094d\u200d\u0937"))))
  (testing "Zero width non-joiner preceded by virama"
    (is (idn/json-idn-hostname? (str "\u0915\u094d\u200c\u0937"))))
  (testing "Zero width non-joiner not preceded by virama but matches regexp"
    (is (idn/json-idn-hostname? (str "\u0628\u064a\u200c\u0628\u064a"))))
  (testing "Invalid length"
    (is (not (idn/json-idn-hostname? (str (str/join (repeat 64 "a")) ".com")))))
  (testing "Non-string input"
    (is (idn/json-idn-hostname? 123))))
