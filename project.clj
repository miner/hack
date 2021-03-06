(defproject hack "0.2.93-SNAPSHOT" 
  :min-lein-version "2.0.0"
  :dependencies [[djblue/portal "0.11.2"]
                 [racehub/om-bootstrap "0.6.1" :exclusions [org.clojure/clojure]]
                 [com.velisco/wilkins "0.2.1"]
                 [com.velisco/strgen "0.2.1"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/data.int-map "1.0.0"]
                 [org.clojure/data.avl "0.1.0"]
                 [org.clojure/core.rrb-vector "0.1.2"]
                 [org.clojure/core.async "1.3.618"]
                 [org.clojure/tools.macro "0.1.5"]
                 [org.clojure/data.priority-map "1.0.0"]
                 [com.datomic/datomic-free "0.9.5697"  :exclusions [joda-time]]
                 [instaparse "1.4.10"]
                 ;;[clj-webdriver "0.5.1"]
                 [org.clojure/core.logic "1.0.0"]
                 [quil "3.1.0"]
                 [primitive-math "0.1.6"]
                 [com.velisco/herbert "0.7.0"]
                 [org.clojure/algo.monads "0.1.6"]
                 [org.clojure/tools.trace "0.7.11"]
                 ;; [org.clojure/core.match "0.2.2"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/tools.macro "0.1.5"]
                 [backtick "0.3.4"]
                 [com.velisco/halfbaked "0.2.3"]
                 [com.velisco/clj-ftp "0.3.15"]
                 [com.clojure-goes-fast/clj-memory-meter "0.1.3"]
                 [com.clojure-goes-fast/clj-java-decompiler "0.3.0"]
                 [metrics-clojure "2.10.0"]
                 [net.cgrand/xforms "0.19.2"]
                 [digest "1.4.10"]
                 [org.clojure/alpha.spec "0.2.177-SNAPSHOT"]
                 ;; [it.unimi.dsi/sux4j "3.0.10"] ;perfect-hashing
                 [me.raynes/fs "1.4.6"]]
  :global-vars {*warn-on-reflection* true
                *unchecked-math* :warn-on-boxed}
  :profiles {:dev  {:dependencies [[org.clojure/clojure "1.11.0-alpha1"]
                                   ;; [org.clojure/core.async "0.4.474"]
                                   ;;[clj-ns-browser "1.3.0" ]
                                   [criterium "0.4.6"]]}
             :clj18 {:dependencies [   
                                   [org.clojure/clojure "1.8.0"]
                                   ]}

             :clj17 {:dependencies [   
                                   [org.clojure/clojure "1.7.0"]
                                   ]}
             :rebl {:dependencies [
                                   [org.clojure/core.async "1.3.618"]
                                   [REBL "0.9.220"]
                                   ]
                    :main cognitect.rebl}
             
             :snapshot {:dependencies [[org.clojure/clojure "1.10.0-master-snapshot"]]}
             }
  :jvm-opts ["-Djdk.attach.allowAttachSelf"]
  :repositories [["sonatype-oss-public" {:url "https://oss.sonatype.org/content/groups/public/"}]]
  ;; :bootclasspath true
  ;; :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"]
  )

