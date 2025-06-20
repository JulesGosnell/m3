(def polyglot-version "24.2.1")
(def jackson-version "2.19.0")

(defproject org.clojars.jules_gosnell/m3 "0.1.0-SNAPSHOT"
  :description "A pure Clojure/ClojureScript JSON validator"
  :url "https://github.com/JulesGosnell/m3"
  :license {:name "Apache License, Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}

  :dependencies
  [;; java
   [org.slf4j/slf4j-simple "2.1.0-alpha1"]                                  ;; https://github.com/qos-ch/slf4j
   [org.graalvm.polyglot/polyglot ~polyglot-version]                        ;; https://github.com/oracle/graal
   [org.graalvm.polyglot/js ~polyglot-version :extension "pom"]             ;; https://github.com/oracle/graal
   [com.fasterxml.jackson.core/jackson-core ~jackson-version]               ;; https://github.com/FasterXML/jackson-core
   [com.fasterxml.jackson.dataformat/jackson-dataformat-cbor ~jackson-version :exclusions [[com.fasterxml.jackson.core/jackson-databind]]]
   [com.fasterxml.jackson.dataformat/jackson-dataformat-smile ~jackson-version :exclusions [[com.fasterxml.jackson.core/jackson-databind]]]
   ;; clj
   [org.clojure/clojure "1.12.1"]                                           ;; https://clojure.org/releases/downloads
   [org.clojure/tools.logging "1.3.0"]                                      ;; https://github.com/clojure/tools.logging
   [cheshire/cheshire "6.0.0"]                                              ;; https://github.com/dakrone/cheshire
   ;; cljs
   [thheller/shadow-cljs "3.1.7"]                                           ;; https://github.com/thheller/shadow-cljs
   ;; cljc
   [com.widdindustries/cljc.java-time "0.1.21"]                             ;; https://github.com/henryw374/cljc.java-time
   ]

  :plugins [[lein-environ "1.2.0"]                                          ;; https://github.com/weavejester/environ
            [lein-cloverage "1.2.4"]                                        ;; https://github.com/cloverage/cloverage
            [lein-shadow "0.4.1"]                                           ;; https://gitlab.com/nikperic/lein-shadow
            [lein-asset-minifier "0.4.7" :exclusions [org.clojure/clojure]] ;; https://github.com/yogthos/lein-asset-minifier
            [com.jakemccrary/lein-test-refresh "0.26.0"]                    ;; https://github.com/jakemcc/test-refresh
            ]

  :min-lein-version "2.5.0"
  :uberjar-name "m3.jar"
  :clean-targets ^{:protect false} [:target-path "target/shadow"]

  :source-paths ["src/clj" "src/cljc" "src/cljs"]
  :test-paths ["test/clj" "test/cljc" "test/cljs"]
  :resource-paths ["resources" "target/cljsbuild"]

  :shadow-cljs {:builds {:test
                         {:target :node-test
                          :output-dir "target/shadow"
                          :output-to "target/shadow/test.js"
                          :runner-ns "m3.runner"
                          :ns-regexp "-test$"
                          :autorun true}
                         :dev
                         {:target :node-script
                          :output-dir "target/node"
                          :output-to "target/node/repl.js"
                          :main m3.repl/-main
                          :devtools {:autoload true}}}}  

  :profiles {:dev {:repl-options {:init-ns m3.repl}
                   :dependencies [[cider/piggieback "0.6.0"]
                                  [binaryage/devtools "1.0.7"]
                                  [prone "2021-04-23"]
                                  [nrepl "1.3.1"]
                                  [pjstadig/humane-test-output "0.11.0"]]

                   :source-paths ["env/dev/clj"]
                   :plugins [[cider/cider-nrepl "0.56.0"]
                             [org.clojure/tools.namespace "1.5.0" :exclusions [org.clojure/tools.reader]]
                             [refactor-nrepl "3.11.0" :exclusions [org.clojure/clojure]]]

                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]

                   :env {:dev true}}

             :uberjar {:hooks [minify-assets.plugin/hooks]
                       :source-paths ["env/prod/clj"]
                       :prep-tasks ["compile" ["cljsbuild" "once" "min"]]
                       :env {:production true}
                       :aot :all
                       :omit-source true}
             :cloverage
             {:cloverage {:fail-threshold 90}}};; output is written to ./target/coverage/index.html

  :aliases {"test-cljs" ["shadow" "compile" "test"]}

  )
