(ns miner.test-hack
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [miner.strgen :as sg]))

(deftest show-info
  (testing "Show test info"
    (println)
    (println "  ** Test Info **")
    (let [info (clojure.edn/read-string (slurp  "project.clj"))]
      (println " " (second info) (nth info 2)))
    (println "  Clojure" (clojure-version))
    (println "  Java" (System/getProperty "java.version"))
    (println "  vendor:" (or (System/getProperty "java.vendor.version")
                             (System/getProperty "java.vendor")))
    (println "  Java home:" (System/getProperty "java.home"))
    (println)
    true))

