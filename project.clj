(defproject cosheet "0.1.2-SNAPSHOT"
  :description "Free form spreadsheet"
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 ;; Server side
                 [ring/ring-core "1.4.0"]
                 [ring/ring-jetty-adapter "1.5.1"]
                 [ring-transit "0.1.4"]
                 [compojure "1.5.1"]
                 [hiccup "1.0.5"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 ;; Client side
                 [org.clojure/clojurescript "1.8.40"]
                 [reagent "0.5.1"]
                 [cljs-ajax "0.5.4"]
                 ;; buddy authentication
                 [buddy/buddy-auth "2.1.0"]
                 [buddy/buddy-sign "2.2.0"]
                 ]
  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-ring "0.9.7"]
            [cider/cider-nrepl "0.11.0"]]
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[lein-binplus "0.6.2"]]}}
  :prep-tasks ["javac" "compile"]
  :source-paths ["src" "src_cljc"]
  :hooks [leiningen.cljsbuild]
  :cljsbuild {:builds [{:source-paths ["src_cljc" "src_cljs"]
                        :compiler {:output-to "resources/public/js/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true
                                   :preamble ["reagent/react.js"]}}]}
  :main ^:skip-aot cosheet.server.routes ;; So we don't need lein ring.
  :ring {:handler cosheet.server.routes/app
         ;; Keep lein ring from reloading tests.
         :reload-paths ["src" "src_cljc"]}
  :bin {:name "cosheet"}
  ;; :target-path "target/%s" ;; breaks: lein ring uberjar
  )

;;; To run from command line:
;;;    lein ring server-headless 3000
;;; To make an executable jar file (requires that Java is installed)
;;;    lein bin
;;;    Then, for mac OS, chmod a+x; add .command to name

;;; This should work, but doesn't:
;;; To make a standalone (no need for java) mac executable
;;;    lein uberjar
;;;    javapackager -deploy -native image -outdir target -outfile cosheet.app -srcfiles target/cosheet-*-standalone.jar -appclass cosheet.server.routes -name "Cosheet" -title "Cosheet"

