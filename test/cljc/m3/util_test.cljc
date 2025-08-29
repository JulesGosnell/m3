(ns m3.util-test
  (:require
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   [m3.util :refer [topo-sort-by make-stable-sort-by]]))

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
