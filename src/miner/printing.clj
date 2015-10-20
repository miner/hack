(ns miner.printing
  ;;
)

;; on to beta9

(defn test42 [pdup reval]
  (let [str42 (binding [*print-dup* pdup] (pr-str (Integer. 42)))]
    (println "printed as " str42)
    (binding [*read-eval* reval]
      (read-string str42))))

(defn run42 []
  (doseq [pdup [true false]
          reval [true false]]
    (println)
    (println "*print-dup* =" pdup)
    (println "*read-eval* = " reval)
    (try 
      (let [result (test42 pdup reval)]
        (println " result =" result (type result)))
      (catch RuntimeException ex (println ex)))))


(defn rd
  ([] (rd *in*))
  ([stream] (rd stream :default nil))
  ([stream & {:keys [eof readers default] :as opts}]
     (println "rd str = " stream)
     (println "  eof     " (:eof opts))
     (println "  readers " (:readers opts))
     (println "  default " (:default opts))
     (println "  opts    " opts)))


     
(defn rs
  "Reads one object from the string s. Returns nil when s is nil or empty.

  Reads data in the edn format (subset of Clojure data):
  http://edn-format.org

  opts is a map as per clojure.edn/read"
  ([s] (rs s :eof nil))
  ([s & {:keys [eof readers default] :as opts}]
     (when s (clojure.lang.EdnReader/readString s opts))))

;; anomaly  
;; (rs nil :eof true) => nil
;; (rs "" :eof true) => true

