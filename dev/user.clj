
(in-ns 'user)

(println "  Loading..." *file*)

(require 'miner)

(comment
(load-file "/Users/miner/Dropbox/clj/miner.clj")
(require '[criterium.core :refer :all])
(println '[criterium.core :refer :all])

(require '[clj-memory-meter.core :refer [measure]])
(println '[clj-memory-meter.core :refer [measure]])

(require '[clj-java-decompiler.core :refer [decompile]])
(println '[clj-java-decompiler.core :refer [decompile]])
)


(println "  Loaded local" *file*)

