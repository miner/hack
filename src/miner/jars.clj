(ns miner.jars
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def jars (filter #(.endsWith % ".jar") 
                  (str/split (System/getProperty "java.class.path") 
                             (re-pattern (System/getProperty "path.separator")))))

(def repo (str (io/file (System/getProperty "user.home") ".m2" "repository")))

(defn relative [root ^String full-path]
  (if (.startsWith full-path repo)
    (subs full-path (inc (.length repo)))
    full-path))

(defn components [path]
  (str/split (relative repo path) (re-pattern (java.io.File/separator))))

(defn version [path]
  (let [cv (components path)
        len (count cv)
        ver (nth cv (- len 2))
        name (nth cv (- len 3))
        group (subvec cv 0 (- len 3))]
    (vector (symbol (str/join "." group) name) ver)))

    
