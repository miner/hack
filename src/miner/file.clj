(ns miner.file
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Should work for most projects, but looks like it would be slow and maybe brittle in assuming
;; that first element of classpath is inside the project
(defn project-root-from-classpath []
  (let [cp1 (first (str/split (System/getProperty "java.class.path") 
                              (re-pattern (System/getProperty "path.separator")) 2))]
    (.getParentFile (io/file cp1))))

;; Another idea:  Use an empty file in resources directory to find yourself.
;; Assumes a certain project layout with "resources" one level below project root.

#_ (def resources-dir (.getParentFile (io/as-file (io/resource "plantilus.marker"))))

#_ (def project-root (.getParentFile resources-dir))
