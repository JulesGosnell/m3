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

(ns m3.ref-test
  (:require 
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   [m3.ref :refer [resolve-uri deep-meld]]))

;;------------------------------------------------------------------------------

(deftest test-resolve-uri

  (testing "matches stashed $id"
    (let [uri {:type :path :path "/a/b"}
          c {"$id" "c"}
          m2 {"a" {"b" c}}
          path ["a"]
          ;; path ["a" "b"] has its own $id (it IS in path->uri), so try-path
          ;; walks to the parent ["a"]. No parent scope → falls back to ctx's id-uri.
          ;; Provide a parent scope so the test exercises the full resolution path.
          parent-uri {:type :path :path "/a"}
          ctx {:root m2 :id-uri parent-uri
               :uri->path {uri ["a" "b"]}
               :path->uri {["a" "b"] uri ["a"] parent-uri}}]
    (is (=
         [(merge ctx {:id-uri parent-uri}) path c]
         (resolve-uri ctx path uri :ref)))))
  )

;;------------------------------------------------------------------------------

(deftest test-meld-deep
  (testing "take first $id, last everything else"
    (is (=
         {"$id" 1, :a 1, :b 2, :c 3}
         (deep-meld {:id-key "$id"} {"$id" 1 :a 1 :b 1 :c 1} {"$id" 2 :b 2 :c 2} {"$id" 3 :c 3})))
    )
  )

