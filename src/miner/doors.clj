(ns miner.doors)

;; https://rosettacode.org/wiki/100_doors#Clojure
;; 100 doors
;; toggle every door
;; toggle every second door
;; toggle third, etc, through 100th only
;; report state after last pass
;; Hint: only perfect squares should be open

(defn toggle-nth [v step]
  (reduce (fn [v i] (update v i not)) v (range 0 (count v) step)))

(defn doors [cnt]
  (let [state (vec (repeat cnt false))]
    (keep-indexed (fn [i x] (when x i))
                  (reduce toggle-nth state (range 1 cnt)))))

(defn run-doors []
  (println "100 doors, 100 toggling passes.  Open door list:" (doors 100)))



(defn tog [ds i]
  (if (contains? ds i)
    (disj ds i)
    (conj ds i)))

(defn door-indices [cnt]
  (mapcat (fn [step] (range 0 cnt step)) (range 1 cnt)))
 
(defn red-doors [cnt]
  (reduce (fn [rset n] (tog rset n)) #{} (door-indices cnt)))

;; cool, but a bit slower than obvious
(defn trset-doors [cnt]
  (transduce (mapcat (fn [step] (range 0 cnt step)))
             (completing tog)
             #{}
              (range 1 cnt)))


;; still slow
(defn freq-doors [cnt]
  (keep (fn [[k v]] (when (odd? v) k)) (frequencies (door-indices 100))))


;; faster to use Java arrays, but a bit ugly
(defn btog [^booleans barr step]
  (reduce (fn [^booleans ba i] (aset-boolean ba i (not (aget ba i))) ba)
          barr
          (range 0 (alength barr) step)))

;; was fastest
(defn bdoors [cnt]
  (let [barr (boolean-array cnt)]
    (keep-indexed (fn [i b] (when b i))
                  (reduce btog barr (range 1 cnt)))))

;; basically same speed as bdoors
(defn abdoors [cnt]
  (let [barr (boolean-array cnt)
        bres  ^booleans (reduce btog barr (range 1 cnt))]
    (areduce bres i r [] (if (aget bres i) (conj r i) r))))


;; pretty good
(defn togi [v i]
  (update v i not))

(defn tr-doors [cnt]
  (transduce (mapcat (fn [step] (range 0 cnt step)))
             (completing togi
                         (fn [v] (keep-indexed (fn [i x] (when x i)) v)))
             (vec (repeat 100 false))
             (range 1 cnt)))


;; slower
(defn fdoors [cnt]
  (let [v (vec (repeat cnt false))]
    (keep-indexed (fn [i b] (when b i))
                  (reduce togi v (for [step (range 1 cnt) i (range 0 cnt step)] i)))))


;; new fastest
(defn bfdoors [cnt]
  (let [barr (boolean-array cnt)]
    (doseq [step (range 1 cnt)]
      (doseq [i (range 0 cnt step)]
        ;; side-effecting barr
        (aset-boolean barr i (not (aget barr i)))))
    (keep-indexed (fn [i b] (when b i)) barr)))


;; same speed as bfdoors
(defn abfdoors [cnt]
  (let [barr (boolean-array cnt)]
    (doseq [step (range 1 cnt)]
      (doseq [i (range 0 cnt step)]
        ;; side-effecting barr
        (aset-boolean barr i (not (aget barr i)))))
    (areduce barr i r [] (if (aget barr i) (conj r i) r))))
