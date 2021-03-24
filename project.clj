(defproject agile-stats "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.2"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/data.json "1.0.0"]
                 [clj-http "3.12.0"]
                 [clojure.java-time "0.3.2"]
                 [org.clojure/tools.cli "0.4.1"]
                 [org.clojure/tools.reader "1.3.4"]
                 [cheshire "5.10.0"]]
  :main ^:skip-aot agile-stats.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
