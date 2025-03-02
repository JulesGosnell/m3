(defproject m3 "0.1.0-SNAPSHOT"
  :description "A pure Clojure/ClojureScript JSON validator"
  :url "https://github.com/JulesGosnell/m3"
  :license {:name "Apache License, Version 2.0"
            :url "https://www.apache.org/licenses/LICENSE-2.0"}

  :dependencies
  [[org.slf4j/slf4j-simple "2.0.17"]                                        ;; https://github.com/qos-ch/slf4j
   [org.graalvm.polyglot/polyglot "24.1.2"]                                 ;; https://github.com/oracle/graal
   [org.graalvm.polyglot/js "24.1.2" :extension "pom"]                      ;; https://github.com/oracle/graal
   [com.fasterxml.jackson.core/jackson-core "2.18.2"]                       ;; https://github.com/FasterXML/jackson-core
   [org.clojure/clojure "1.12.0"]                                           ;; https://clojure.org/releases/downloads
   [org.clojure/clojurescript "1.11.132" :scope "provided"]                 ;; https://github.com/clojure/clojurescript
   [org.clojure/tools.logging "1.3.0"]                                      ;; https://github.com/clojure/tools.logging
   [camel-snake-kebab "0.4.3"]                                              ;; https://github.com/clj-commons/camel-snake-kebab
   [cheshire/cheshire "5.13.0"]                                             ;; https://github.com/dakrone/cheshire
   [com.widdindustries/cljc.java-time "0.1.21"]                             ;; https://github.com/henryw374/cljc.java-time
   ]

  :jvm-opts ["-Xmx4G"]

  :plugins [[lein-environ "1.2.0"]                                          ;; https://github.com/weavejester/environ
            [lein-cloverage "1.2.4"]                                        ;; https://github.com/cloverage/cloverage
            [lein-cljsbuild "1.1.8"]                                        ;; https://github.com/emezeske/lein-cljsbuild
            [lein-asset-minifier "0.4.7" :exclusions [org.clojure/clojure]] ;; https://github.com/yogthos/lein-asset-minifier
            [com.jakemccrary/lein-test-refresh "0.25.0"]                    ;; https://github.com/jakemcc/test-refresh
            ]

  :min-lein-version "2.5.0"
  :uberjar-name "m3.jar"
  :clean-targets ^{:protect false}
  [:target-path
   [:cljsbuild :builds :app :compiler :output-dir]
   [:cljsbuild :builds :app :compiler :output-to]]

  :source-paths ["src/clj" "src/cljc" "src/cljs"]
  :test-paths ["test/clj" "test/cljc" "test/cljs"]
  :resource-paths ["resources" "target/cljsbuild"]

  :cljsbuild
  {:builds {:min
            {:source-paths ["src/cljs" "src/cljc" "env/prod/cljs"]
             :compiler
             {:output-to        "target/cljsbuild/public/js/app.js"
              :output-dir       "target/cljsbuild/public/js"
              :source-map       "target/cljsbuild/public/js/app.js.map"
              :optimizations :advanced
              :infer-externs true
              :pretty-print  false}}
            :app
            {:source-paths ["src/cljs" "src/cljc" "env/dev/cljs"]
             :compiler
             {:main "m3.dev"
              :asset-path "/js/out"
              :output-to "target/cljsbuild/public/js/app.js"
              :output-dir "target/cljsbuild/public/js/out"
              :source-map true
              :optimizations :none
              :pretty-print  true}}}}

  :profiles {:dev {:repl-options {:init-ns m3.repl}
                   :dependencies [[cider/piggieback "0.5.3"]
                                  [binaryage/devtools "1.0.7"]
                                  [prone "2021-04-23"]
                                  [nrepl "1.1.1"]
                                  [pjstadig/humane-test-output "0.11.0"]]

                   :source-paths ["env/dev/clj"]
                   :plugins [[cider/cider-nrepl "0.47.1"]
                             [org.clojure/tools.namespace "1.5.0"
                              :exclusions [org.clojure/tools.reader]]
                             [refactor-nrepl "3.10.0"
                              :exclusions [org.clojure/clojure]]]

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
             {:plugins [[lein-cloverage "1.2.4"]] ;; https://github.com/cloverage/cloverage
              :cloverage {;; https://stackoverflow.com/questions/406230/regular-expression-to-match-a-line-that-doesnt-contain-a-word
                          :test-ns-regex [#"^((?!m3\.integration-test).)*$"]
                          ;; output is written to ./target/coverage/index.html
                          :fail-threshold 65}}})
