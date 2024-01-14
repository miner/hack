(ns miner
  (:require [clojure.main]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))


(println "  Loading..." *file*)

(defn miner-init []
  (in-ns 'user)
  (apply require clojure.main/repl-requires)
  (require '[criterium.core :as crit :refer [quick-bench]])
  (prn '(require [criterium.core :as crit :refer [quick-bench]]))
  (require '[miner :refer [print-repl print-cp]])
  ;; standard REPL setup
  (set! *print-namespace-maps* false)
  (set! *print-length* 100)
  (set! *print-level* 100)
  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)
  true)

(defn miner-repl
  ([] (println "Miner REPL") (clojure.main/repl :init miner-init))
  ([args] (when args (println "Ignoring args" args)) (miner-repl)))

(defn -main []
  (miner-repl))

(defn replace-prefix [s prefix replacement]
  (if (str/starts-with? s prefix)
    (str replacement (subs s (count prefix)))
    s))

(defn print-cp []
  "Print Java class path"
  (let [home (System/getProperty "user.home")
        cwd (System/getProperty "user.dir")
        maven-home (str home "/.m2/repository")
        cp (->> (.split (System/getProperty "java.class.path")
                        (System/getProperty "path.separator"))
             (map #(replace-prefix % maven-home "MVN"))
             (map #(replace-prefix % cwd "."))
             (map #(replace-prefix % home "~")))]
    (println "CWD  = " cwd)
    (println "HOME = " home)
    (println "MVN  = " maven-home)
    (println "CLASSPATH =")
    (pprint cp)))
        
(defn print-repl []
  (println "Clojure" (clojure-version) (or *command-line-args* ""))
  (println "CWD =" (System/getProperty "user.dir"))
  (let [stars '[*print-namespace-maps* *print-length* *print-level* *warn-on-reflection*
                *unchecked-math*]]
    (doseq [star stars]
      (println star (eval star))))
  nil)

(println "  Loaded local" *file*)
