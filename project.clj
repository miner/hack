(defproject hack "0.2.9-SNAPSHOT" 
  :min-lein-version "2.0.0"
  :dependencies [
                 [racehub/om-bootstrap "0.6.1" :exclusions [org.clojure/clojure]]
                 [com.velisco/wilkins "0.2.1"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/data.int-map "0.2.4"]
                 [org.clojure/core.async "0.3.443"]
                 [org.clojure/tools.macro "0.1.5"]
                 [com.datomic/datomic-free "0.9.5561.56"  :exclusions [joda-time]]
                 [instaparse "1.4.7"]
                 ;;[clj-webdriver "0.5.1"]
                 [org.clojure/core.logic "0.8.11"]
                 [quil "2.6.0"]
                 [primitive-math "0.1.6"]
                 ;; [com.velisco/herbert "0.6.1"]
                  [com.velisco/herbert "0.7.0"]
                 [org.clojure/algo.monads "0.1.6"]
                 [org.clojure/tools.trace "0.7.9"]
                 ;; [org.clojure/core.match "0.2.2"]
                 [org.clojure/core.match "0.3.0-alpha3"]
                 [com.velisco/halfbaked "0.2.3"]
                 [com.velisco/clj-ftp "0.3.9"]
                 [metrics-clojure "2.9.0"]
                 [net.cgrand/xforms "0.9.3"]
                 [digest "1.4.5"]
                 ;; [it.unimi.dsi/sux4j "3.0.10"] ;perfect-hashing
                 [me.raynes/fs "1.4.6"]]
  :profiles {:dev  {:dependencies [   
                                   [org.clojure/spec.alpha "0.1.123"]
                                   [org.clojure/clojure "1.9.0-alpha16"]
                                   [org.clojure/core.async "0.3.443"]
                                   ;;[clj-ns-browser "1.3.0" ]
                                   [criterium "0.4.4"]]}
             :clj18 {:dependencies [   
                                   [org.clojure/clojure "1.8.0"]
                                   ]}

             :clj17 {:dependencies [   
                                   [org.clojure/clojure "1.7.0"]
                                   ]}

             :snapshot {:dependencies [[org.clojure/clojure "1.9.0-master-snapshot"]]}
             }
  :repositories [["sonatype-oss-public" {:url "https://oss.sonatype.org/content/groups/public/"}]]
  ;; :bootclasspath true
  ;; :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"]
  )
