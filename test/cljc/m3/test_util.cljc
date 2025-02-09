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

