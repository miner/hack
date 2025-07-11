(defproject hack "0.4.0-SNAPSHOT" 
  :min-lein-version "2.0.0"
  :dependencies [[djblue/portal "0.59.1"]
                 [dom-top "1.0.9"]
                 [com.datomic/local "1.0.291"]
                 [com.taoensso/telemere "1.0.1"]
                 ;;[com.datomic/client-api "1.0.67"]
                 [racehub/om-bootstrap "0.6.1" :exclusions [org.clojure/clojure]]
                 [com.velisco/wilkins "0.2.1"]
                 [com.velisco/strgen "0.2.5"]
                 [org.clojure/data.csv "1.1.0"]
                 [org.clojure/math.combinatorics "0.3.0"]
                 [org.clojure/data.int-map "1.3.0"]
                 [org.clojure/data.avl "0.2.0"]
                 [org.clojure/core.rrb-vector "0.2.0"]
                 [org.clojure/core.async "1.8.741"]
                 [org.clojure/tools.macro "0.2.1"]
                 [org.clojure/data.priority-map "1.2.0"]
                 [instaparse "1.5.0"]
                 [nrepl/nrepl "1.3.1"]
                 ;;[clj-webdriver "0.5.1"]
                 [org.clojure/core.logic "1.1.0"]
                 [quil "4.3.1563"]
                 [primitive-math "0.1.6"]
                 [com.velisco/herbert "0.7.0"]
                 [org.clojure/algo.monads "0.2.0"]
                 [org.clojure/tools.trace "0.8.0"]
                 ;; [org.clojure/core.match "0.2.2"]
                 [org.clojure/core.match "1.1.0"]
                 [org.clojure/tools.macro "0.2.1"]
                 [backtick "0.3.5"]
                 [com.velisco/halfbaked "0.2.3"]
                 [com.velisco/clj-ftp "1.2.0"]
                 [com.clojure-goes-fast/clj-memory-meter "0.4.0"]
                 [com.clojure-goes-fast/clj-java-decompiler "0.3.7"]
                 [metrics-clojure "2.10.0"]
                 [net.cgrand/xforms "0.19.6"]
                 [digest "1.4.10"]
                 ;; [org.clojure/spec.alpha "0.3.214"]
                 ;; [it.unimi.dsi/sux4j "3.0.10"] ;perfect-hashing
                 [org.clojars.quoll/tiara "0.4.0"]
                 ;; [com.github.nubank/morse "2023.10.06.02"]
                 [clj-commons/fs "1.6.311"]]
  :global-vars {*warn-on-reflection* true
                ;; *unchecked-math* :warn-on-boxed
                }
  :profiles {:dev  {:source-paths ["src" "dev"]
                    :dependencies [[org.clojure/clojure "1.12.1"]
                                   ;; [org.clojure/core.async "1.5.640"]
                                   ;;[clj-ns-browser "1.3.0" ]
                                   [criterium "0.4.6"]]}
             :clj18 {:dependencies [[org.clojure/clojure "1.8.0"]
                                    [nrepl "1.3.1"]
                                    ]}

             :clj17 {:dependencies [   
                                    [org.clojure/clojure "1.7.0"]
                                    ]}
             :rebl {:dependencies [
                                   [org.clojure/core.async "1.8.741"]
                                   [REBL "0.9.220"]
                                   ]
                    :main cognitect.rebl}
             
             :snapshot {:dependencies [[org.clojure/clojure "1.13.0-master-snapshot"]]}
             }
  :jvm-opts ["-Djdk.attach.allowAttachSelf"]
  :repositoriesXXX {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"
                 ;; for Morse
                 "jitpack" "https://jitpack.io"}

  ;;  :repositories [["releases" {:url "https://repo.clojars.org"}]]

  ;; :bootclasspath true
  ;; :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"]
  )

