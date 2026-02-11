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

(ns m3.util-test
  (:require
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   [m3.util :refer [map-values topo-sort-by make-stable-sort-by
                     convert-output make-error make-error-on make-error-on-failure
                     absent absent? present? concatv seq-contains? assoc-when
                     third fourth into-set conj-set]]))

(deftest test-map-values
  (is (=
       {:a 2, :b 3, :c 4}
       (map-values (fn [_k v] (inc v)) {:a 1 :b 2 :c 3}))))


(deftest test-topo-sort-by
  (testing "without xform"
    (is (=
         [[:d []] [:b [:d]] [:e []] [:c [:e]] [:a [:b :c]]]
         (topo-sort-by second identity {:a [:b :c], :b [:d], :c [:e], :e [], :d []}))))
  (testing "with-xform"
    (is (=
         [:d :b :e :c :a]
         (topo-sort-by second first {:a [:b :c], :b [:d], :c [:e], :e [], :d []}))))
  )

(deftest test-make-stable-sort-by
  (testing "vanilla"
    (is (=
         [[:a :a] [:b :b] [:c :c] [:d :d]]
         ((make-stable-sort-by identity [:a :b :c :d :e])
          identity identity [:d :b :c :a]))))
  (testing "unknown key"
    (is (=
         [[:a :a] [:b :b] [:c :c] [:d :d]]
         ((make-stable-sort-by identity [:a :b :c :d :e])
          identity identity [:d :b :c :a :f]))))
  (testing "with k1 fn"
    (is (=
         [[:a [:a :A]] [:b [:b :B]] [:c [:c :C]] [:d [:d :D]]]
         ((make-stable-sort-by first [[:a :A] [:b :B] [:c :C] [:d :D] [:e :E]])
          identity identity [:d :b :c :a]))))
  (testing "with k2 fn"
    (is (=
         [[[:a :A] :a] [[:b :B] :b] [[:c :C] :c] [[:d :D] :d]]
         ((make-stable-sort-by identity [:a :b :c :d :e])
          first identity [[:d :D] [:b :B] [:c :C] [:a :A]]))))
  (testing "with xform fn"
    (is (=
         [:a :b :c :d]
         ((make-stable-sort-by identity [:a :b :c :d :e])
          identity first [:d :b :c :a])))))

;;------------------------------------------------------------------------------
;; third / fourth

(deftest test-third
  (is (= 3 (third [1 2 3 4 5])))
  (is (= :c (third [:a :b :c]))))

(deftest test-fourth
  (is (= 4 (fourth [1 2 3 4 5])))
  (is (= :d (fourth [:a :b :c :d]))))

;;------------------------------------------------------------------------------
;; absent / absent? / present?

(deftest test-absent-sentinel
  (is (= :absent absent))
  (is (absent? absent))
  (is (absent? :absent))
  (is (not (absent? nil)))
  (is (not (absent? "hello")))
  (is (present? nil))
  (is (present? "hello"))
  (is (not (present? absent))))

;;------------------------------------------------------------------------------
;; concatv

(deftest test-concatv
  (testing "both non-nil"
    (is (= [1 2 3 4] (concatv [1 2] [3 4]))))
  (testing "first nil defaults to []"
    (is (= [3 4] (concatv nil [3 4]))))
  (testing "empty inputs"
    (is (= [] (concatv [] [])))
    (is (= [1] (concatv [] [1])))))

;;------------------------------------------------------------------------------
;; into-set / conj-set

(deftest test-into-set
  (is (= #{1 2 3} (into-set nil [1 2 3])))
  (is (= #{1 2 3 4} (into-set #{1} [2 3 4]))))

(deftest test-conj-set
  (is (= #{1} (conj-set nil 1)))
  (is (= #{1 2} (conj-set #{1} 2))))

;;------------------------------------------------------------------------------
;; seq-contains?

(deftest test-seq-contains?
  (is (seq-contains? [1 2 3] = 2))
  (is (not (seq-contains? [1 2 3] = 4)))
  (is (seq-contains? ["a" "ab" "abc"] #(>= (count %2) (count %1)) "ab")))

;;------------------------------------------------------------------------------
;; assoc-when

(deftest test-assoc-when
  (testing "assocs non-nil values"
    (is (= {:a 1} (assoc-when {} :a 1))))
  (testing "skips nil values"
    (is (= {} (assoc-when {} :a nil))))
  (testing "adds to existing map"
    (is (= {:a 1 :b 2} (assoc-when {:a 1} :b 2)))))

;;------------------------------------------------------------------------------
;; make-error

(deftest test-make-error
  (let [e (make-error "type mismatch" ["type"] {"type" "string"} ["name"] 42)]
    (is (= ["type"] (:schema-path e)))
    (is (= ["name"] (:document-path e)))
    (is (= 42 (:document e)))
    (is (= {"type" "string"} (:schema e)))
    (is (string? (:message e)))
    (is (.contains ^String (:message e) "42"))))

;;------------------------------------------------------------------------------
;; make-error-on / make-error-on-failure

(deftest test-make-error-on
  (testing "returns nil when not failed"
    (is (nil? (make-error-on "msg" ["x"] {} [] nil seq nil))))
  (testing "returns error when failed"
    (let [sub-errors [{:message "sub"}]
          result (make-error-on "wrapper" ["x"] {} [] "doc" seq sub-errors)]
      (is (= 1 (count result)))
      (is (= sub-errors (:errors (first result))))))
  (testing "message can be a function"
    (let [result (make-error-on (fn [_] "dynamic") ["x"] {} [] "doc" seq [{:m "e"}])]
      (is (.contains ^String (:message (first result)) "dynamic")))))

(deftest test-make-error-on-failure
  (testing "delegates to make-error-on with seq as failed?"
    (is (nil? (make-error-on-failure "msg" ["x"] {} [] nil nil)))
    (let [result (make-error-on-failure "msg" ["x"] {} [] "doc" [{:m "e"}])]
      (is (= 1 (count result))))))

;;------------------------------------------------------------------------------
;; convert-output

(deftest test-convert-output
  (let [output-fns {:make-map (fn [& kvs] (into {} (map vec (partition 2 kvs))))
                    :make-vec (fn [coll] (vec coll))
                    :make-kw  str}]
    (testing "nil output-fns returns value unchanged"
      (is (= {:a 1} (convert-output nil {:a 1})))
      (is (= [1 2] (convert-output nil [1 2])))
      (is (= "hello" (convert-output nil "hello"))))

    (testing "converts map with known error keys"
      (let [result (convert-output output-fns {:valid? true :errors nil})]
        (is (map? result))
        (is (= true (get result "valid")))
        (is (nil? (get result "errors")))))

    (testing "converts map with keyword keys using error-key-map"
      (let [result (convert-output output-fns {:schema-path ["type"]
                                               :message "fail"})]
        (is (= ["type"] (get result "schemaPath")))
        (is (= "fail" (get result "message")))))

    (testing "converts vector"
      (let [result (convert-output output-fns [{:valid? true}])]
        (is (vector? result))
        (is (= true (get (first result) "valid")))))

    (testing "converts seq"
      (let [result (convert-output output-fns (list {:valid? false}))]
        (is (vector? result))
        (is (= false (get (first result) "valid")))))

    (testing "passes through scalars"
      (is (= 42 (convert-output output-fns 42)))
      (is (= "hello" (convert-output output-fns "hello")))
      (is (= true (convert-output output-fns true))))

    (testing "nested conversion"
      (let [result (convert-output output-fns
                     {:valid? false
                      :errors [{:schema-path ["type"]
                                :document-path ["age"]
                                :message "type error"
                                :document "old"
                                :schema {"type" "integer"}}]})]
        (is (= false (get result "valid")))
        (let [errors (get result "errors")]
          (is (= 1 (count errors)))
          (let [e (first errors)]
            (is (= ["type"] (get e "schemaPath")))
            (is (= ["age"] (get e "documentPath")))
            (is (= "old" (get e "document")))))))))
