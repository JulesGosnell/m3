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

(ns m3.ecma
  #?(:clj
     (:import
      [org.graalvm.polyglot Context Value])))

;;------------------------------------------------------------------------------

;; use Graal/JavaScript to acquire an ECMA-262 compliant RegeExp engine
;; now we can use the same code at both back and front end...

#?(:clj (defonce ^Context js-context (Context/create (into-array ["js"]))))

#?(:clj (defonce ^Value RegExp (.getMember (.getBindings js-context "js") "RegExp")))

#?(:clj (defn ecma-pattern [^String s] (.newInstance RegExp (into-array Object [s "u"])))
   :cljs (defn ecma-pattern [s] (js/RegExp. s "u")))

#?(:clj (defn ecma-match [^Value r ^String s] (.asBoolean (.invokeMember r "test" (into-array Object [s]))))
   :cljs (defn ecma-match [r s] (.test r s)))

