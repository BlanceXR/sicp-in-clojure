(defproject sicp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [criterium "0.4.1"]
                 [net.mikera/core.matrix "0.7.0"]
                 [com.cemerick/pomegranate "0.2.0"]
                 [org.clojure/math.combinatorics "0.0.4"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [org.clojure/tools.trace "0.7.5"]
                 [incanter "1.4.1"]]
  :dev-dependencies [[alembic "0.1.1"]]
  ;;TODO: edit if going to run long time
  :jvm-opts ["-XX:+TieredCompilation" "-XX:TieredStopAtLevel=1"]
  :bootclasspath true
  :profiles {:dev
             {:dependencies [[alembic "0.1.1"]]}})
