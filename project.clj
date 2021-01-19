(defproject agile-stats "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [clojure.java-time "0.3.2"]
                 [org.clojure/tools.cli "0.4.1"]]
  :main ^:skip-aot agile-stats.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
