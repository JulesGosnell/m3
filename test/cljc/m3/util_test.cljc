(ns m3.util-test
  (:require
   #?(:clj  [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer-macros [deftest testing is]])
   [m3.util :refer [topo-sort-by]]))

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
