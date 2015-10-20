(ns miner.testf
 (:require miner.wilkins))

(defn my-platform []
  (println "Clojure version = " (clojure-version))
  (println *clojure-version*)
  (println  #x/condf [(and clj jdk) "Clojure" cljs "ClojureScript" clr "CLR" else "Unknown"])
  (println "has reducers? " #x/condf [clojure.core.reducers/map "YES" else "NO"])
  (println "has java.io.File? " #x/condf [(class? java.io.File) "File" else "No File"])
  nil)


(defn clr? []
  #x/condf [clr true [clj "1.5+"] false])

(defn jdk16? []
  #x/condf [jdk1.6.* true])

