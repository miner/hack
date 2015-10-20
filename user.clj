(println "(do (use 'clojure.core.async) (require '[miner.vb :as vb]))")
(println "(doseq [x (sort-by (juxt :court :ts) (vb/run-gym))] (prn x))")

