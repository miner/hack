(ns miner.weighted)

;; mailing list question on weighted rand-nth

;; My code is not so good.
;;
;; Consider much better approach:
;; https://gist.github.com/ghadishayban/a26cc402958ef3c7ce61
;;
;; Vose's alias method
;; http://www.keithschwarz.com/darts-dice-coins/


(def coll [{:weight 2, :value 10} {:weight 5, :value 20} {:weight 7, :value 30}])

(defn nth2
  ;; two-dimensional nth
  ([vv i j] (nth2 vv i j nil))
  ([vv i j not-found] (nth (nth vv i nil) j not-found)))

(defn peekj
  ([vv j] (peekj vv j nil))
  ([vv j not-found] (nth (peek vv) j not-found)))

(defn peek2
  ([vv] (peek2 vv nil))
  ([vv not-found] (peek (peek vv) not-found)))

;; converted weighted maps [{:weight int :value int}+]
;; to vector of subvectors [[(:= lo int) (:= hi int) (:= val int)]+]
;; where lo and hi are calculated as ranges [lo, hi) based on offset ranges
;; effectively buckets for rand-int to map to values by binary-search
(defn spread3 [vms]
  (reduce (fn [res {:keys [weight value]}]
            (let [offset (peekj res 1 0)]
              (conj res [offset (+ offset weight) value])))
          []
          vms))

(defn find-val3 [vspread n]
  (when (and vspread (pos? (count vspread)) (<= (nth2 vspread 0 0) n (peekj vspread 1)))
    (loop [lo 0 hi (dec (count vspread))]
      (when (<= lo hi)
        (let [i (+ lo (quot (- hi lo) 2))
              n1 (nth2 vspread i 1)
              n0 (nth2 vspread i 0)]
          (cond (and (>= n n0) (< n n1))   (nth2 vspread i 2)
                (< n n0) (recur lo (dec i))
                :else (recur (inc i) hi)))))))

(defn rand-weighted [vspread]
  (let [smax (peekj vspread 1)]
    (find-val3 vspread (rand-int smax))))

(defn test3 []
  (let [sss (spread3 coll)
        smax (peekj sss 1)]
    (frequencies (map #(find-val3 sss %) (repeatedly (* 1000 smax) #(rand-int smax))))))

(defn test2 [ws]
  (let [vspread (spread3 ws)
        smax (peekj vspread 1)]
    (frequencies (repeatedly (* 1000 smax) #(rand-weighted vspread)))))

