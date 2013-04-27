(defproject finaljourney "0.1.0-SNAPSHOT"
  :description "Final Journey, Ludum Dare #26 game programming competition entry by @zorcam"
  :url "http://finaljourney.herokuapp.com"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [ring/ring-jetty-adapter "1.1.0"]
                 [crate "0.2.1"]
                 [enfocus "1.0.1"]
                 [compojure "1.1.1"]
                 ;;[hiccup "1.0.1"]
                 [ring "1.1.6"]
                 ;;[cheshire "3.0.0"]
                 [com.keminglabs/singult "0.1.6"]
                 ]
  :plugins [[lein-cljsbuild "0.2.9"]]
  :hooks [leiningen.cljsbuild]
  :min-lein-version "2.0.0"
  :main finaljourney.server
  :cljsbuild {:builds
              [{:source-paths ["src"],
                :compiler {:pretty-print true,
                           :output-to "resources/public/js/cljs.js",
                           :optimizations :whitespace},
                :jar true}]})
