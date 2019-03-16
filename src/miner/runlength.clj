(ns miner.runlength)

;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-317-when-you-see-a-job-apply/

;; (rle [:a :a :a :b :c :d :d :d :d])
;;=> ([3 :a] [1 :b] [1 :c] [4 :d])
;;
;; (rld '([3 :a] [1 :b] [1 :c] [4 :d]))
;;=> (:a :a :a :b :c :d :d :d :d)



;; doesn't work on infinite list so isn't lazy enough
;; something about sequence?
;; experimenting with unreduced but not sure about that
;; look at how partition-all does it

(defn rle-nolazy
  ([]
   (fn [rf]
     (let [cv (volatile! 0)
           xv (volatile! (Object.))]
       (fn
         ([] (rf))
         ([result] (unreduced (if (zero? @cv)
                                (rf result)
                                (rf result [@cv @xv]))))
         ([result input]
          (let [x @xv]
            (if (= input x)
              (do (vswap! cv inc)
                  (rf result))
              (let [c @cv]
                (vreset! xv input)
                (vreset! cv 1)
                (if (zero? c)
                  (rf result)
                  (rf result [c x]))))))))))

  ([coll] (sequence (rle-nolazy) coll)))



(defn rle-good
  ([]
   (fn [rf]
     (let [cv (volatile! 0)
           xv (volatile! (Object.))]
       (fn
         ([] (rf))
         ([result] (unreduced (if (zero? @cv)
                                (rf result)
                                (rf result [@cv @xv]))))
         ([result input]
          (let [x @xv]
            (if (= input x)
              (do (vswap! cv inc)
                  (rf result))
              (let [c @cv]
                (vreset! xv input)
                (vreset! cv 1)
                (if (zero? c)
                  (rf result)
                  (rf result [c x]))))))))))

  ([coll]
   (if-not (seq coll)
     ()
     (loop [x (first coll) c 1 coll (rest coll) res []]
       (cond (empty? coll) (seq (conj res [c x]))
             (= x (first coll)) (recur x (inc c) (rest coll) res)
             :else (recur (first coll) 1 (rest coll) (conj res [c x])))))))


;; SEM: LOOK AT SOURCE OF PARTITION-BY for handling reduced?

(defn rle
  ([]
   (fn [rf]
     (let [cv (volatile! 0)
           xv (volatile! (Object.))]
       (fn
         ([] (rf))
         ([result] (unreduced (if (zero? @cv)
                                (rf result)
                                (rf result [@cv @xv]))))
         ([result input]
          (let [x @xv]
            (if (= input x)
              (do (vswap! cv inc)
                  (rf result))
              (let [c @cv]
                (vreset! xv input)
                (vreset! cv 1)
                (if (zero? c)
                  (rf result)
                  (rf result [c x]))))))))))

  ([coll]
   (if-not (seq coll)
     ()
     (loop [x (first coll) c 1 coll (rest coll) res []]
       (cond (empty? coll) (seq (conj res [c x]))
             (= x (first coll)) (recur x (inc c) (rest coll) res)
             :else (recur (first coll) 1 (rest coll) (conj res [c x])))))))



(defn count-leading [x coll]
  (loop [cnt 0 coll coll]
    (if (and (seq coll) (= x (first coll)))
      (recur (inc cnt) (rest coll))
      cnt)))
        

(defn rle2
  ([]
   (fn [rf]
     (let [cv (volatile! 0)
           xv (volatile! (Object.))]
       (fn
         ([] (rf))
         ([result] (unreduced (if (zero? @cv)
                                (rf result)
                                (rf result [@cv @xv]))))
         ([result input]
          (let [x @xv]
            (if (= input x)
              (do (vswap! cv inc)
                  (rf result))
              (let [c @cv]
                (vreset! xv input)
                (vreset! cv 1)
                (if (zero? c)
                  (rf result)
                  (rf result [c x]))))))))))



  ([coll]
   (lazy-seq
    (when-let [coll (seq coll)]
      (let [x (first coll)
            cnt (count-leading x coll)]
        (cons [cnt x] (rle2 (lazy-seq (drop cnt coll)))))))))



(defn rle3
  ([]
   (fn [rf]
     (let [cv (volatile! 0)
           xv (volatile! (Object.))]
       (fn
         ([] (rf))
         ([result] (unreduced (if (zero? @cv)
                                (rf result)
                                (rf result [@cv @xv]))))
         ([result input]
          (let [x @xv]
            (if (= input x)
              (do (vswap! cv inc)
                  (rf result))
              (let [c @cv]
                (vreset! xv input)
                (vreset! cv 1)
                (if (zero? c)
                  (rf result)
                  (rf result [c x]))))))))))

  ([coll]
   (lazy-seq
    (when-first [x coll]
      (let [cnt (count-leading x coll)]
        (cons [cnt x] (rle2 (drop cnt coll))))))))






;; nice simple version, but can't handle infinite lists

(defn prle
  ([] (comp (partition-by identity) (map (juxt count peek))))
  ([coll] (sequence (prle) coll)))


;; transducer can handle infinite input if we do the take within, but not on the outside
;; (sequence (comp (take 2) (rle)) (list* :a :b (repeat :z)))

(comment
  ;; infinite input
  (take 2 (rle (list* :a :b :b (repeat :x))))

  )

#_
(defn lrle
   ([coll] (if-let [coll (seq coll)]
             (lrle (first coll) 1 (rest coll) [])
             ()))
   ([target cnt coll result]
    ...))

 
;;;;; JUNK

(defn rle-works
  ([]
   (fn [rf]
     (let [vv (volatile! nil)]
       (fn
         ([] (rf))
         ([result] (if-let [v @vv]
                     (unreduced (rf result [(count v) (peek v)]))
                     (rf result)))
         ([result input]
          (if-let [v @vv]
            (if (= input (peek v))
              (do (vswap! vv conj input)
                  (rf result))
              (do (vreset! vv [input])
                  (rf result [(count v) (peek v)])))
          (do (vreset! vv [input])
              (rf result))))))))
  ([coll] (sequence (rle-works) coll)))


(defn rle-OK
  ([]
   (fn [rf]
     (let [vv (volatile! nil)]
       (fn
         ([] (rf))
         ([result] (if-let [v @vv]
                     (rf result [(count v) (peek v)])
                     (rf result)))
         ([result input]
          (if-let [v @vv]
            (if (= input (peek v))
              (do (vswap! vv conj input)
                  (rf result))
              (do (vreset! vv [input])
                  (rf result [(count v) (peek v)])))
          (do (vreset! vv [input])
              (rf result))))))))
  ([coll] (sequence (rle-OK) coll)))


(defn rle-not-better
  ([]
   (fn [rf]
     (let [xcv (volatile! nil)]
       (fn
         ([] (rf))
         ([result] (if-let [[x c] @xcv]
                     (rf result [c x])
                     (rf result)))
         ([result input]
          (if-let [[x c] @xcv]
            (if (= input x)
              (do (vreset! xcv (list x (inc c)))
                  (rf result))
              (do (vreset! xcv (list input 1))
                  (rf result [c x])))
            (do (vreset! xcv (list input 1))
                (rf result))))))))
  ([coll] (sequence (rle-not-better) coll)))


(defn rle-samish
  ([]
   (fn [rf]
     (let [cv (volatile! 0)
           xv (volatile! nil)]
       (fn
         ([] (rf))
         ([result] (if (zero? @cv)
                     (rf result)
                     (rf result [@cv @ xv])))
         ([result input]
          (let [c @cv]
            (if (zero? @cv)
              (do (vswap! cv inc)
                  (vreset! xv input)
                  (rf result))
              (let [x @xv]
                (if (= input x)
                  (do (vswap! cv inc)
                      (rf result))
                  (do (vreset! xv input)
                      (vreset! cv 1)
                      (rf result [c x])))))))))))

  ([coll] (sequence (rle-samish) coll)))




(defn rle-fastest
  ([]
   (fn [rf]
     (let [cv (volatile! 0)
           xv (volatile! (Object.))]
       (fn
         ([] (rf))
         ([result] (if (zero? @cv)
                     (rf result)
                     (rf result [@cv @xv])))
         ([result input]
          (let [x @xv]
            (if (= input x)
              (do (vswap! cv inc)
                  (rf result))
              (let [c @cv]
                (vreset! xv input)
                (vreset! cv 1)
                (if (zero? c)
                  (rf result)
                  (rf result [c x]))))))))))

  ([coll] (sequence (rle-fastest) coll)))







(defn rle-trans-no-faster
  ([]
   (let [peek! (fn [tv] (nth tv (dec (count tv))))]
   (fn [rf]
     (let [vv (volatile! nil)]
       (fn
         ([] (rf))
         ([result] (if-let [tv @vv]
                     (unreduced (rf result [(count tv) (peek! tv)]))
                     (rf result)))
         ([result input]
          (if-let [tv @vv]
            (if (= input (peek! tv))
              (do (vswap! vv conj! input)
                  (rf result))
              (do (vreset! vv (transient [input]))
                  (rf result [(count tv) (peek! tv)])))
          (do (vreset! vv (transient [input]))
              (rf result)))))))))
  ([coll] (sequence (rle-trans-no-faster) coll)))







(defn smoke
  ([] (smoke rle))
  ([rle]
   (let [abx (concat [:a :b] (repeat :x))
         xyxz  (concat (repeat 10000 :x) (repeat 10000 :y) (repeat 10000 :x)
                           (repeat 10000 :z))]

     (assert (= (rle [:a :a :a :b :c :d :d :d :d])
             '([3 :a] [1 :b] [1 :c] [4 :d])))
     (assert (= (rle []) ()))
     (assert (= (rle [:a]) '([1 :a])))
     (assert (= (rle [:a :a]) '([2 :a])))
     (assert (= (rle [:a :a :b]) '([2 :a] [1 :b])))
     (assert (= (rle (repeat 10000 :x)) '([10000 :x])))
     (assert (= (rle xyxz)
                '([10000 :x] [10000 :y] [10000 :x] [10000 :z])))
  #_   (assert (= (take 1 (rle abx))
              '([1 :a])))

  #_ (assert (= (rld '([3 :a] [1 :b] [1 :c] [4 :d]))
                '(:a :a :a :b :c :d :d :d :d)))

  )
  true))


(require '[criterium.core :as cc])

(defn ben [& fs]
  (doseq [f fs]
    (println)
    (println (str f))
    (cc/quick-bench (smoke f))))

