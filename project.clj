(def polyglot-version "24.2.2")
(def jackson-version "2.20.0")

(defproject org.clojars.jules_gosnell/m3 "1.0.0-beta2-SNAPSHOT"
  :description "A pure Clojure/ClojureScript JSON validator"
  :url "https://github.com/JulesGosnell/m3"
  :license {:name "Apache License, Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}

  :dependencies
  [;; java
   [org.slf4j/slf4j-simple "2.1.0-alpha1"] ;; https://github.com/qos-ch/slf4j
   ;; https://www.graalvm.org/jdk24/reference-manual/js/RunOnJDK/
   ;; TODO: it would be nice to get JIT compilation going - although do we need it ?
   [org.graalvm.polyglot/polyglot ~polyglot-version] ;; https://github.com/oracle/graal
   [org.graalvm.polyglot/js ~polyglot-version :extension "pom"] ;; https://github.com/oracle/graal
   ;; [org.graalvm.sdk/graal-sdk ~polyglot-version]
   ;; [org.graalvm.compiler/compiler ~polyglot-version]
   ;; [org.graalvm.compiler/compiler-management ~polyglot-version]
   ;; [org.graalvm.truffle/truffle ~polyglot-version]
   [com.fasterxml.jackson.core/jackson-core ~jackson-version] ;; https://github.com/FasterXML/jackson-core
   [com.fasterxml.jackson.dataformat/jackson-dataformat-cbor ~jackson-version :exclusions [[com.fasterxml.jackson.core/jackson-databind]]]
   [com.fasterxml.jackson.dataformat/jackson-dataformat-smile ~jackson-version :exclusions [[com.fasterxml.jackson.core/jackson-databind]]]
   ;; clj
   [org.clojure/clojure "1.12.3"] ;; https://clojure.org/releases/downloads
   [org.clojure/tools.logging "1.3.0"] ;; https://github.com/clojure/tools.logging
   [cheshire/cheshire "6.1.0"] ;; https://github.com/dakrone/cheshire
   ;; cljs
   [thheller/shadow-cljs "3.2.0"] ;; https://github.com/thheller/shadow-cljs
   ;; cljc
   [com.widdindustries/cljc.java-time "0.1.21"] ;; https://github.com/henryw374/cljc.java-time
   ]

  :plugins [[lein-environ "1.2.0"] ;; https://github.com/weavejester/environ
            [lein-cloverage "1.2.4"] ;; https://github.com/cloverage/cloverage
            [lein-shadow "0.4.1"] ;; https://gitlab.com/nikperic/lein-shadow
            [lein-asset-minifier "0.4.7" :exclusions [org.clojure/clojure]] ;; https://github.com/yogthos/lein-asset-minifier
            [com.jakemccrary/lein-test-refresh "0.26.0"] ;; https://github.com/jakemcc/test-refresh
            [lein-ancient "0.7.0"] ;; https://github.com/xsc/lein-ancient
            [lein-shell "0.5.0"] ;; https://github.com/hypirion/lein-shell
            ]

  :global-vars {*warn-on-reflection* true}

  :jvm-opts ["-XX:+UnlockExperimentalVMOptions"
             "-XX:+EnableJVMCI"
             "-Dpolyglot.engine.WarnInterpreterOnly=false" ;; disable polyglot warning - TODO: revisit sometime and see if we can get JIT copilation working
;;             "--upgrade-module-path=/home/jules/.m2/repository/org/graalvm/compiler/compiler/24.2.2/compiler-24.2.2.jar:/home/jules/.m2/repository/org/graalvm/compiler/compiler-management/24.2.2/compiler-management-24.2.2.jar:/home/jules/.m2/repository/org/graalvm/sdk/graal-sdk/24.2.2/graal-sdk-24.2.2.jar"
             ]

  :min-lein-version "2.5.0"
  :uberjar-name "m3.jar"
  :clean-targets ^{:protect false} [:target-path "target/shadow" ".shadow-cljs"]

  :source-paths ["src/clj" "src/cljc" "src/cljs"]
  :test-paths ["test/clj" "test/cljc" "test/cljs"]
  :resource-paths ["resources" "target/cljsbuild"]

  :shadow-cljs {:builds {:test
                         {:target :node-test
                          :output-dir "target/shadow"
                          :output-to "target/shadow/test.js"
                          :ns-regexp "-test$"
                          :autorun false}
                         :dev
                         {:target :node-script
                          :output-dir "target/node"
                          :output-to "target/node/repl.js"
                          :main m3.repl/-main
                          :devtools {:autoload true}}
                         :npm
                         {:target :npm-module
                          :output-dir "npm-dist"
                          :entries [m3.js]}}}

  :profiles {:dev
             {:repl-options {;;:init-ns m3.repl
                             }
              :dependencies [[cider/piggieback "0.6.1"]
                             [binaryage/devtools "1.0.7"]
                             [prone "2021-04-23"]
                             [nrepl "1.5.0"]
                             [pjstadig/humane-test-output "0.11.0"]
                             [com.fasterxml.jackson.core/jackson-databind ~jackson-version]
                             [com.google.code.gson/gson "2.12.1"]]

              :plugins [[cider/cider-nrepl "0.57.0"]
                        [org.clojure/tools.namespace "1.5.0" :exclusions [org.clojure/tools.reader]]
                        [refactor-nrepl "3.11.0" :exclusions [org.clojure/clojure3]]]

              :injections [(require 'pjstadig.humane-test-output)
                           (pjstadig.humane-test-output/activate!)]

              :env {:dev true}}

             :uberjar
             {:hooks [minify-assets.plugin/hooks]
              :prep-tasks ["compile" ["cljsbuild" "once" "min"]]
              :env {:production true}
              :aot :all
              :omit-source true}

             :cloverage ;; output is written to ./target/coverage/index.html
             {:cloverage {:fail-threshold 95}}}

  :aliases {"test-cljs" ["do"
                         ["shadow" "compile" "test"]
                         ["shell" "node" "target/shadow/test.js"]]
            "quick-test" ["run" "-m" "m3.test-runner"]
            "build-npm" ["do"
                         ["shadow" "compile" "npm"]
                         ["shell" "bash" "-c"
                          "cp -r resources/schemas npm-dist/schemas"]]
            "clean-all" ["do"
                         ["clean"]
                         ["shell" "bash" "-c"
                          "rm -rf npm-dist/*.js npm-dist/*.js.map npm-dist/schemas"]]})
