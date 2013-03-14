(defproject xn.client "0.1.0-SNAPSHOT"
  :description "XN Logic data import tools"
  :url "http://xnlogic.com"
  :jvm-opts ["-Xmx2g" "-XX:+TieredCompilation" "-XX:TieredStopAtLevel=1"]
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [org.clojure/data.json "0.2.0"]
                 [clj-http "0.6.3"]
                 [clojure-csv/clojure-csv "2.0.0-alpha2"]
                 [clj-stacktrace "0.2.5"]
                 [fipp "0.1.0-SNAPSHOT"]]
  :plugins [[lein-kibit "0.0.7"]]
  :repl-options {:port 59999})
