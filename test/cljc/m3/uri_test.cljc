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

(ns m3.uri-test
  (:require
   [m3.uri :refer [parse-uri inherit-uri parse-uri]] 
   #?(:clj [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :as t :include-macros true])))

;;------------------------------------------------------------------------------

(deftest test-parse-uri
  (testing "urn"
    (testing "origin"
      (is
       (= {:type :urn, :origin "urn:uuid"}
          (parse-uri "urn:uuid:"))))
    (testing "origin and nss"
      (is
       (=
        {:type :urn, :origin "urn:uuid" :nss "deadbeef-1234-0000-0000-4321feebdaed"}
        (parse-uri "urn:uuid:deadbeef-1234-0000-0000-4321feebdaed"))))
    (testing "origin and fragment"
      (is
       (=
        {:type :urn, :origin "urn:uuid", :fragment "/bar"}
        (parse-uri "urn:uuid:#/bar"))))
    (testing "origin and fragment"
      (is
       (=
        {:type :urn, :origin "urn:uuid", :fragment "/bar"}
        (parse-uri "urn:uuid:#/bar"))))
    (testing "origin, nss and fragment"
      (is
       (=
        {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-0000-0000-4321feebdaed" :fragment "/bar"}
        (parse-uri "urn:uuid:deadbeef-1234-0000-0000-4321feebdaed#/bar")))))
  (testing "url"
    (testing "origin"
      (is
       (= {:type :url, :origin "http://localhost:1234"}
          (parse-uri "http://localhost:1234"))))
    (testing "origin and path"
      (is
       (= {:type :url, :origin "http://localhost:1234" :path "/foo"}
          (parse-uri "http://localhost:1234/foo"))))
    (testing "origin and path and fragment"
      (is
       (= {:type :url, :origin "http://localhost:1234" :path "/foo" :fragment "/bar"}
          (parse-uri "http://localhost:1234/foo#/bar"))))
    (testing "origin and fragment"
      (is
       (= {:type :url, :origin "http://localhost:1234" :fragment "/bar"}
          (parse-uri "http://localhost:1234#/bar"))))
    (testing "path"
      (is
       (= {:type :path :path "/foo"}
          (parse-uri "/foo"))))
    (testing "path and fragment"
      (is
       (= {:type :path :path "/foo" :fragment "/bar"}
          (parse-uri "/foo#/bar"))))
    (testing "fragment"
      (is
       (= {:type :fragment :fragment "/bar"}
          (parse-uri "#/bar"))))))

(deftest test-inherit-uri
  (is
   (=
    [{:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment4"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed"}
     {:type :path, :path "uuid"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment4"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed"}
     {:type :path, :path "uuid"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment4"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed"}
     {:type :path, :path "/uuid"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2"}
     {:type :url, :origin "http://authority.com", :path "/path1"}
     {:type :url, :origin "http://authority.com"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2"}
     {:type :url, :origin "http://authority.com", :path "/path1"}
     {:type :url, :origin "http://authority.com"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2"}
     {:type :url, :origin "http://authority.com", :path "/path1"}
     {:type :url, :origin "http://authority.com"}
     {:type :path, :path "/path3/path4", :fragment "fragment2"}
     {:type :path, :path "/path3/path4"}
     {:type :path, :path "/path3"}
     {:type :path, :path "path5"}
     {:type :path, :path "/path3/path4", :fragment "fragment2"}
     {:type :path, :path "/path3/path4"}
     {:type :path, :path "/path3"}
     {:type :path, :path "path5"}
     {:type :path, :path "/path3/path4", :fragment "fragment2"}
     {:type :path, :path "/path3/path4"}
     {:type :path, :path "/path3"}
     {:type :path, :path "/path5"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment3"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment3"}
     {:type :path, :path "uuid", :fragment "fragment3"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment4"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed"}
     {:type :url, :origin "http://authority.com", :path "/path1/uuid", :fragment "/fragment1"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment4"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed"}
     {:type :url, :origin "http://authority.com", :path "/path1/uuid"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment4"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed"}
     {:type :url, :origin "http://authority.com", :path "/uuid"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment4"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed"}
     {:type :url, :origin "http://authority.com", :path "/uuid"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2"}
     {:type :url, :origin "http://authority.com", :path "/path1"}
     {:type :url, :origin "http://authority.com"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2"}
     {:type :url, :origin "http://authority.com", :path "/path1"}
     {:type :url, :origin "http://authority.com"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2"}
     {:type :url, :origin "http://authority.com", :path "/path1"}
     {:type :url, :origin "http://authority.com"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2"}
     {:type :url, :origin "http://authority.com", :path "/path1"}
     {:type :url, :origin "http://authority.com"}
     {:type :url, :origin "http://authority.com", :path "/path3/path4", :fragment "fragment2"}
     {:type :url, :origin "http://authority.com", :path "/path3/path4", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path3", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path5", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path3/path4", :fragment "fragment2"}
     {:type :url, :origin "http://authority.com", :path "/path3/path4"}
     {:type :url, :origin "http://authority.com", :path "/path3"}
     {:type :url, :origin "http://authority.com", :path "/path1/path5"}
     {:type :url, :origin "http://authority.com", :path "/path3/path4", :fragment "fragment2"}
     {:type :url, :origin "http://authority.com", :path "/path3/path4"}
     {:type :url, :origin "http://authority.com", :path "/path3"}
     {:type :url, :origin "http://authority.com", :path "/path5"}
     {:type :url, :origin "http://authority.com", :path "/path3/path4", :fragment "fragment2"}
     {:type :url, :origin "http://authority.com", :path "/path3/path4"}
     {:type :url, :origin "http://authority.com", :path "/path3"}
     {:type :url, :origin "http://authority.com", :path "/path5"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "fragment3"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "fragment3"}
     {:type :url, :origin "http://authority.com", :path "/path1", :fragment "fragment3"}
     {:type :url, :origin "http://authority.com", :fragment "fragment3"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment4"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed"} {:type :path, :path "/path3/uuid", :fragment "fragment2"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment4"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed"}
     {:type :path, :path "/path3/uuid"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment4"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed"}
     {:type :path, :path "/uuid"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment4"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed"}
     {:type :path, :path "/uuid"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2"}
     {:type :url, :origin "http://authority.com", :path "/path1"}
     {:type :url, :origin "http://authority.com"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2"}
     {:type :url, :origin "http://authority.com", :path "/path1"}
     {:type :url, :origin "http://authority.com"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2"}
     {:type :url, :origin "http://authority.com", :path "/path1"}
     {:type :url, :origin "http://authority.com"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2"}
     {:type :url, :origin "http://authority.com", :path "/path1"}
     {:type :url, :origin "http://authority.com"}
     {:type :path, :path "/path3/path4", :fragment "fragment2"}
     {:type :path, :path "/path3/path4", :fragment "fragment2"}
     {:type :path, :path "/path3", :fragment "fragment2"}
     {:type :path, :path "/path3/path5", :fragment "fragment2"}
     {:type :path, :path "/path3/path4", :fragment "fragment2"}
     {:type :path, :path "/path3/path4"}
     {:type :path, :path "/path3"}
     {:type :path, :path "/path3/path5"}
     {:type :path, :path "/path3/path4", :fragment "fragment2"}
     {:type :path, :path "/path3/path4"}
     {:type :path, :path "/path3"}
     {:type :path, :path "/path5"}
     {:type :path, :path "/path3/path4", :fragment "fragment2"}
     {:type :path, :path "/path3/path4"}
     {:type :path, :path "/path3"}
     {:type :path, :path "/path5"}
     {:type :path, :path "/path3/path4", :fragment "fragment3"}
     {:type :path, :path "/path3/path4", :fragment "fragment3"}
     {:type :path, :path "/path3", :fragment "fragment3"}
     {:type :path, :path "path5", :fragment "fragment3"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed", :fragment "fragment4"}
     {:type :urn, :origin "urn:uuid", :nss "deadbeef-1234-ff00-00ff-4321feebdaed"}
     {:type :path, :path "uuid"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2", :fragment "/fragment1"}
     {:type :url, :origin "http://authority.com", :path "/path1/path2"}
     {:type :url, :origin "http://authority.com", :path "/path1"}
     {:type :url, :origin "http://authority.com"}
     {:type :path, :path "/path3/path4", :fragment "fragment2"}
     {:type :path, :path "/path3/path4"}
     {:type :path, :path "/path3"}
     {:type :path, :path "path5"}
     {:type :fragment, :fragment "fragment3"}]
    (mapv
     (partial apply inherit-uri)
     (let [type->uris
           {:urn      [(parse-uri "urn:uuid:deadbeef-1234-ff00-00ff-4321feebdaed#fragment4")
                       (parse-uri "urn:uuid:deadbeef-1234-ff00-00ff-4321feebdaed")
                       (parse-uri "urn:uuid")]
            :url      [(parse-uri "http://authority.com/path1/path2#/fragment1")
                       (parse-uri "http://authority.com/path1/path2")
                       (parse-uri "http://authority.com/path1")
                       (parse-uri "http://authority.com")]
            :path     [(parse-uri "/path3/path4#fragment2")
                       (parse-uri "/path3/path4")
                       (parse-uri "/path3")
                       (parse-uri "path5")
                       ]
            :fragment [(parse-uri "#fragment3")]}]
       (mapcat
        (fn [[p c :as testcases]]
          (mapcat
           (fn [parent]
             (map
              (fn [child]
                [parent child])
              (type->uris c)))
           (type->uris p)))
        [[:urn      :urn]
         [:urn      :url]
         [:urn      :path]
         [:urn      :fragment]
         [:url      :urn]
         [:url      :url]
         [:url      :path]
         [:url      :fragment]
         [:path     :urn]
         [:path     :url]
         [:path     :path]
         [:path     :fragment]
         [:fragment :urn]
         [:fragment :url]
         [:fragment :path]
         [:fragment :fragment]
         ]))))))
