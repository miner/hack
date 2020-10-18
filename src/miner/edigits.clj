(ns miner.edigits)


;; Write a function that takes a sequence of integers. You're trying to get all 10 digits by
;; looking through the numbers sequentially. When you have found one instance of every
;; decimal digit, return whatever number you were on when you found the last one. If you get
;; to the end of the sequence without finding all the digits (for instance, maybe there was
;; no 9), then just return nil.



;; fast with bits, pretty simple, but somewhat clever to use bits as a set
(defn digit-search [coll]
  ;; digs starts as 10 bits corresponding to 0-9 chars
  (loop [coll coll digs (bit-shift-left 1023 (long \0))]
    (when-first [i coll]
      (let [digs (long (reduce (fn [bits ch] (bit-clear bits (long ch))) digs (str i)))]
        (if (zero? digs)
          i
          (recur (rest coll) digs))))))








(defn char->digit [^Character ch]
  (- (long ch) (long \0)))

(defn digits [n]
  (map char->digit (str n)))

(defn ndigs [coll]
  (loop [coll coll digs (set (range 10))]
    (when-first [i coll]
      (let [digs2 (reduce disj digs (digits i))]
        (if (empty? digs2)
          i
          (recur (rest coll) digs2))))))

;; actually better to use transients, see farther below

(defn reddigs [coll]
  (let [result (reduce (fn [res i]
                         (let [res (reduce disj res (digits i))]
                           (if (empty? res)
                             (reduced i)
                             res)))
                       (set (range 10))
                       coll)]
    (when (int? result)
      result)))


(defn digit-search1 [coll]
  (loop [coll coll digs #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}]
    (when-first [i coll]
      (let [digs (reduce disj digs (str i))]
        (if (empty? digs)
          i
          (recur (rest coll) digs))))))

;; zero count is slightly faster than empty? but perhaps not worth it
;; transients are somewhat faster
(defn dsearch [coll]
  (loop [coll coll digs (transient #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})]
    (when-first [i coll]
      (let [digs (reduce disj! digs (str i))]
        (if (zero? (count digs))
          i
          (recur (rest coll) digs))))))





(def ^:const ten-bits (bit-shift-left 1023 (long \0)))
;; 287948901175001088

(defn bsearch4 [coll]
  ;; digs starts as 10 bits
  (loop [coll coll digs ten-bits]
    (when-first [i coll]
      (let [digs (long (transduce (map long) (completing bit-clear) digs (str i)))]
        (if (zero? digs)
          i
          (recur (rest coll) digs))))))




;; much slower to do map over str
(defn bsearch5 [coll]
  ;; digs starts as 10 bits
  (loop [coll coll digs ten-bits]
    (when-first [i coll]
      (let [digs (long (reduce bit-clear digs (map long (str i))))]
        (if (zero? digs)
          i
          (recur (rest coll) digs))))))

#_ (assert (< (long \0) (long \9) 64))


;; maybe fastest so far, but not pretty
(defn xds [coll]
  (transduce
   identity
   (completing
    (fn [digs i]
      (let [digs (transduce (map long) (completing bit-clear) digs (str i))]
        (if (zero? digs)
          (reduced (reduced i))
          digs)))
    #(when (reduced? %) (deref %)))
   ;; digs starts as 10 bits
   (bit-shift-left 1023 (long \0))
   coll))




;; slightly slower
(defn xds6 [coll]
  (let [result (reduce (fn [digs i]
                         (let [digs (transduce (map long) (completing bit-clear) digs (str i))]
                           (if (zero? digs)
                             (reduced (reduced i))
                             digs)))
                       ;; digs starts as 10 bits
                       (bit-shift-left 1023 (long \0))
                       coll)]
    (when (reduced? result)
      (deref result))))




(defn xds7 [coll]
  (let [result (reduce (fn [digs i]
                         (let [mask (transduce (map long) (completing bit-set) 0 (str i))
                               digs (bit-and-not digs mask)]
                           (if (zero? digs)
                             (reduced (reduced i))
                             digs)))
                       ;; digs starts as 10 bits
                       (bit-shift-left 1023 (long \0))
                       coll)]
    (when (reduced? result)
      (deref result))))

;; too slow
(defn xds8 [coll]
  (transduce (map #(list (transduce (map long) (completing bit-set) 0 (str %)) %))
             (completing
              (fn [digs [mask i]]
                (let [digs (bit-and-not digs mask)]
                  (if (zero? digs)
                    (reduced (reduced i))
                    digs)))
              #(when (reduced? %) (deref %)))
             (bit-shift-left 1023 (long \0))
             coll))


(defn smoke-digs
  ([] (smoke-digs digit-search))
  ([digit-search]
  (assert (= (digit-search   [5175 4538 2926 5057 6401 4376 2280 6137]) 5057))
  ;; digits found: 517- 4-38 29-6 -0
  (assert (= (digit-search   [5719 7218 3989 8161 2676 3847 6896 3370]) 3370))
  ;; digits found: 5719 -2-8 3--- --6- ---- --4- ---- ---0
  (assert (nil? (digit-search   [4883 3876 7769 9846 9546 9634 9696 2832])))
  ;; digits found: 48-3 --76 ---9 ---- -5-- ---- ---- 2---
  ;; 0 and 1 are missing
  true))

          
      
;; from the solutions (changed the names)

;; Crazy cgrand solution, scans string of all numbers and reads result using offsets.
;; Notice the semicolon which causes the read-string to return nil appropriately.  Example
;; of s: "5175 4538 2926 5057 6401 4376 2280 6137 ;0123456789"
(defn cgrand [nums]
  (let [ds "0123456789"
        s (str (apply print-str nums) " ;" ds)
        i (transduce (map #(->> (.indexOf s (int %)) (.lastIndexOf s " "))) max -1 ds)]
    (read-string {:eof nil} (subs s (inc i)))))

;; branchless
(defn cgrand2 [nums]
  (nth nums (->> nums
                 (map-indexed (fn [i n] (zipmap (str n) (repeat i))))
                 (reduce #(into %2 %1) {})
                 vals
                 (cons Integer/MAX_VALUE)
                 (take-last 10)
                 (reduce max))
       nil))




(defn mchampine [nums]
  (ffirst (filter #(= 10 (count (second %)))
                  (zipmap nums (rest (reductions #(into %1 (str %2)) #{} nums))))))


(defn mchampine2 [ns]
  (get ns (count (take-while #(< (count %) 10) (rest (reductions #(into %1 (str %2)) #{}
                                                                 ns))))))


(defn mc3 [nums]
  (nth nums
       (count (take-while #(pos? (count %))
                          (rest (reductions #(reduce disj! %1 (str %2))
                                            (transient #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
                                            nums))))
       nil))



(defn mc32 [nums]
  (let [n (first (keep-indexed #(when (zero? (count %2)) %)
                          (rest (reductions #(reduce disj! %1 (str %2))
                                            (transient #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
                                            nums))))]
    (when n
      (nth nums n nil))))




(defn mc4 [nums]
  (first (mapcat (fn [i digs] (when (zero? (count digs)) (list i)))
                 nums
                 (rest (reductions #(reduce disj! %1 (str %2))
                                   (transient #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
                                   nums)))))




;; fastest mc
(defn mc5 [nums]
  (loop [results (rest (reductions #(reduce disj %1 (str %2))
                                   #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
                                   nums))
         nums nums]
    (when-first [digs results]
      (if (zero? (count digs))
        (first nums)
        (recur (rest results) (rest nums))))))



(defn mc6 [nums]
  (first (remove nil? (map (fn [n digs] (when (empty? digs) n))
                           nums
                           (rest (reductions #(reduce disj %1 (seq (str %2)))
                                             #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
                                             nums))))))






(defn zelark [xs]
  (let [n (reduce (fn [acc x]
                    (let [digits (into acc (str x))]
                      (if (= (count digits) 10) (reduced x) digits)))
                  #{} xs)]
    (when (number? n) n)))


(defn zelark3 [xs]
  (let [res (reduce (fn [acc x]
                      (let [acc (reduce disj acc (str x))]
                        (if (zero? (count acc)) (reduced x) acc)))
                    #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
                    xs)]
    (when-not (set? res) res)))


;; slightly faster, but not pretty
(defn zelark4 [xs]
  (reduce (fn [acc x]
            (when x
              (let [acc (reduce disj acc (str x))]
                (if (zero? (count acc)) (reduced x) acc))))
          #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
          (conj (vec xs) nil)))


;; assuming vnums is already a vector
(defn zelark41 [vnums]
  {:pre [(vector? vnums)]}
  (reduce (fn [acc x]
            (when x
              (let [acc (reduce disj! acc (str x))]
                (if (zero? (count acc)) (reduced x) acc))))
          (transient #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
          (conj vnums nil)))


;; not faster
(defn zelark5 [xs]
  (let [xv (vec xs)
        ilast (dec (count xv))]
  (reduce-kv (fn [acc i x]
               (let [acc (reduce disj acc (str x))]
                 (cond (zero? (count acc)) (reduced x)
                       (= i ilast) (reduced nil)
                       :else acc)))
          #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
          xv)))



(defn ds-vector [vnums]
  {:pre [(vector? vnums)]}
  (reduce (fn [digs n]
            (let [digs (when n (reduce disj! digs (str n)))]
              (if (zero? (count digs)) (reduced n) digs)))
          (transient #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
          (conj vnums nil)))


(defn dsv2 [vnums]
  {:pre [(vector? vnums)]}
  (reduce (fn [digs n]
            (let [digs (when n (reduce disj! digs (str n)))]
              (if (zero? (count digs)) (reduced n) digs)))
          (transient #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
          (conj vnums nil)))


(defn tsv2 [vnums]
  {:pre [(vector? vnums)]}
  (transduce
   identity
   (completing
    (fn [digs n]
      (let [digs (reduce disj! digs (str n))]
        (if (zero? (count digs)) (reduced n) digs)))
    #(when (number? %) %))
    (transient #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
    vnums))
