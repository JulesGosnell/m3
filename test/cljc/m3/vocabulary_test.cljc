(ns m3.vocabulary-test
  (:require
   [m3.util :refer [fourth]]
   [m3.property :refer [check-property-const check-property-type]]
   [m3.vocabulary :refer [new-make-dialect-2]]
   #?(:clj [clojure.test :refer [deftest testing is]]
      :cljs [cljs.test :refer [deftest testing is] :include-macros true])))

(def v
  {"https://json-schema.org/draft/2020-12/vocab/core"              true
   "https://json-schema.org/draft/2020-12/vocab/applicator"        true
   "https://json-schema.org/draft/2020-12/vocab/unevaluated"       true
   "https://json-schema.org/draft/2020-12/vocab/validation"        true
   "https://json-schema.org/draft/2020-12/vocab/meta-data"         true
   "https://json-schema.org/draft/2020-12/vocab/format-annotation" true
   "https://json-schema.org/draft/2020-12/vocab/content"           true})

(deftest test-new-make-dialect-2
  (let [dialect (new-make-dialect-2 :draft2020-12 v)]
    (testing "simple"
      (is (=
           [[["const" "hello"] check-property-const]
            [["type" "string"] check-property-type]]
           (dialect {"type" "string" "const" "hello"}))))))
    
