;;; http://johnj.com/from-elegance-to-speed.html
;;;
;;; Blog post says this `smt-8` was slow so he re-wrote in Common Lisp and got 300x
;;; improvement.  He also changed the approach along the way so it's not a straight
;;; comparison.  I can get a good improvement in regular Clojure.

(ns miner.smt
  (:require [criterium.core :as cc]
            [miner.repl :as repl]))

;; original
(defn smt-8 [times]
  (->> times
       (partition 8 1)
       (map (juxt identity
                  (comp (partial apply -)
                        (juxt last first))))
       (filter (comp (partial > 1000) second))))


;; 20% improvement using `keep`
(defn smt-a [times]
  (->> times
       (partition 8 1)
       (keep (fn [part]
               (let [d (- (last part) (first part))]
                 (when (> 1000 d)
                   [part d]))))))

;; 4x using transducers
(defn smt-b [times]
  (let [v (vec times)
        width 8]
    (into []
          (comp (map (fn [i] (subvec v i (+ i width))))
                (keep (fn [part]
                        (let [d (- (last part) (first part))]
                          (when (> 1000 d)
                            [part d])))))
          (range (- (count v) (dec width))))))

(defn smt-c [times]
  (let [v (vec times)
        width 8]
    (into []
          (keep (fn [i]
                  (let [d (- (v (+ i (dec width))) (v i))]
                    (when (> 1000 d)
                      [(subvec v i (+ i width)) d]))))
          (range (- (count v) (dec width))))))


;; (set! *unchecked-math* :warn-on-boxed)


(defn smt-e [times]
  (let [v (vec times)
        width 8]
    (into []
          (keep (fn [i]
                  (let [d (- ^long (v (+ ^long i (dec width))) ^long (v i))]
                    (when (> 1000 d)
                      [(subvec v i (+ ^long i width)) d]))))
          (range (- (count v) (dec width))))))

;; (set! *unchecked-math* false)



;; fastest, with compatible format
;; requires long hints
;; range expression compensates for "reverse" conjing into list, which is slightly faster
;; than conjing onto a vector.
(defn smt-fast [times]
  (let [v (vec times)
        width 8]
    (into ()
          (keep (fn [i]
                  (let [d (- ^long (v (+ (dec width) ^long i)) ^long (v i))]
                    (when (> 1000 d)
                      (list (subvec v i (+ ^long i width)) d)))))
          (range (- (count v) (inc width)) -1 -1))))




;; test data
(defonce huge (vec (take (long 1e7) (iterate #(+ % (rand-int 1000)) 0))))

(defonce big (vec (take (long 1e6) (iterate #(+ % (rand-int 1000)) 0))))

(defonce small (vec (take 100 (iterate #(+ % (rand-int 300)) 0))))


(defn ben [& smtfs]
   (when smtfs
     (assert (apply = (smt-8 big) (map #(% big) (remove #{smt-8} smtfs)))))
   (doseq [smtf (or smtfs (list smt-8))]
     (println)
     (println (repl/as-symbol smtf))
     (cc/quick-bench (count (smtf big)))))






(defn xmt [times]
  (let [v (vec times)]
  (into []
        (comp (map #(subvec v % (+ % 8)))
              (keep #(let [d (- (% 7) (% 0))]
                       (when (> 1000 d)
                         (list % d)))))
        (range (- (count v) 7)))))


;;; borrowed from transmuters
;;;  BUT ultimately not the best approach

(defmacro vlswap!
  "Non-atomically swaps the value of the volatile as if:
   (apply f current-value-of-vol args). Returns the value that
   was swapped in.  Requires that volatile always holds a long."
  [vol f & args]
  (let [v (with-meta vol {:tag 'clojure.lang.Volatile})
        tagged (with-meta (gensym) {:tag 'long})]
    `(let [~tagged (.deref ~v)] (long (.reset ~v (~f ~tagged ~@args))))))


;; based on preserving-reduced from clojure/core.clj
;; the extra level of `reduced` preserves the early termination value
(defn rf-reduce [rf result xs]
  (let [rrf (fn [r x] (let [ret (rf r x)] (if (reduced? ret) (reduced ret) ret)))]
    (reduce rrf result xs)))

(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;; q clojure.lang.PersistentQueue
(defn push
  "Push x onto PersistentQueue q, popping q as necessary to keep count <= limit"
  ([q x] (conj q x))
  ([q limit x]
   ;; {:pre [(pos? limit)]}
   (if (>= (count q) limit)
     (recur (pop q) limit x)
     (conj q x))))

(defn slide
  ([n] (slide n []))
  ([n init]
   (fn [rf]
     (let [qv (volatile! (queue init))]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [q (vswap! qv push n input)]
            (if (< (count q) n)
              result
              (rf result (seq q))))))))))

;;; end transmuters


;; too slow
(defn smt-slide [times]
  (let [v (vec times)
        width 8]
    (into []
          (comp (slide 8)
                (keep (fn [part]
                        (let [d (- (last part) (first part))]
                          (when (> 1000 d)
                            [part d])))))
          times)))




(defn vdiff [v]
  (- (peek v) (nth v 0)))

;; about 3x better using transducers
(defn smt-sem [times]
  (into [] 
        (comp (slide 8)
              (map vec)
              (filter #(> 1000 (vdiff %)))
              (map (juxt identity vdiff)))
        times))

(defn diff [coll]
  (- (last coll) (first coll)))

(defn limitf [limit]
  (fn [coll] (> limit (diff coll))))

(defn smt2 [times]
  (into [] 
        (comp (slide 8)
              (filter (limitf 1000))
              (map (juxt identity diff)))
        times))

(defn sm3 [times]
  (into [] 
        (comp (slide 8)
              (filter (limitf 1000)))
        times))

(defn smt3 [times]
  (into [] (map (juxt identity diff)) (sm3 times)))



(defn dmt [times]
  (into [] 
        (comp (slide 8)
              (filter (limitf 1000))
              (map #(conj % (diff %))))
        times))




;;; SEM: better rep -- just groups.  If necessary conj the diff at the front.  But probably
;;; cheaper to recalc when you need it.


(defn dgroupf [limit]
  (fn [coll]
    (let [d (diff coll)]
      (when (> limit d)
        (conj coll d)))))

(defn dmt2 [times]
  (into []
        (comp (slide 8)
              (keep (dgroupf 1000)))
        times))

(defn smt5 [times]
  (into [] (map (juxt rest first) (dmt2 times))))




(defn xsmt-8 [times]
  (map first (smt-8 times)))


(defn xsmt [times]
  (into [] 
        (comp (slide 8)
              (map vec)
              (filter #(> 1000 (vdiff %))))
        times))


;; slightly different structure than original but Clojure =
;; subvec is much faster than taking partitions
;; list is slightly faster than vector
(defn rmt [times]
  (let [v (vec times)]
    (keep (fn [i]
           (let [d (- (v (+ 7 i)) (v i))]
             (when (> 1000 d)
               (list (subvec v i (+ i 8)) d))))
          (range (- (count v) 7)))))



(defn xrmt [times]
  (let [v (vec times)]
    (into []
          (keep (fn [i]
                  (let [d (- (v (+ 7 i)) (v i))]
                    (when (> 1000 d)
                      (list (subvec v i (+ i 8)) d)))))
          (range (- (count v) 7)))))

(defn xrmt2 [times]
  (let [v (vec times)]
    (into ()
          (keep (fn [i]
                  (let [d (- (v (+ 7 i)) (v i))]
                    (when (> 1000 d)
                      (list (subvec v i (+ i 8)) d)))))
          (range (- (count v) 9) -1 -1))))

;; almost fastest, with compatible format
;; but assumes vector input
(defn xrmt32 [v]
  (let [width 8]
    (into ()
          (keep (fn [i]
                  (let [d (- ^long (v (+ (dec width) (long i))) ^long (v i))]
                    (when (> 1000 d)
                      (list (subvec v i (+ (long i) width)) d)))))
          (range (- (count v) (inc width)) -1 -1))))



(defn xrmt4 [times]
  (let [v (vec times)
        width 8]
    (into []
          (keep (fn [i]
                  (let [d (- ^long (v (+ (dec width) (long i))) ^long (v i))]
                    (when (> 1000 d)
                      (list (subvec v i (+ (long i) width)) d)))))
          (range (- (count v) (dec width))))))


(defn xrmt5 [times]
  (let [v (vec times)
        width 8]
    (sequence
          (keep (fn [i]
                  (let [d (- ^long (v (+ (dec width) (long i))) ^long (v i))]
                    (when (> 1000 d)
                      (list (subvec v i (+ (long i) width)) d)))))
          (range (- (count v) (dec width))))))

;; (set! *unchecked-math* :warn-on-boxed)


;; fastest, with compatible format
;; requires long hints
;; strange range to reverse into list (slightly faster consing)
(defn xrmt34 [times]
  (let [v (vec times)
        width 8]
    (into ()
          (keep (fn [i]
                  (let [d (- ^long (v (+ (dec width) ^long i)) ^long (v i))]
                    (when (> 1000 d)
                      (list (subvec v i (+ ^long i width)) d)))))
          (range (- (count v) (inc width)) -1 -1))))


(defn xrmt37 [times]
  (let [v (vec times)
        width 8]
    (into ()
          (keep (fn [^long i]
                  (let [d (- ^long (v (+ (dec width) i)) ^long (v i))]
                    (when (> 1000 d)
                      (list (subvec v i (+ i width)) d)))))
          (range (- (count v) (inc width)) -1 -1))))



;; slow without hints.  cost for abstracting window-diff function
(defn xrmt36 [times]
  (let [v (vec times)
        width 8
        window-diff (fn [v i]
                      (- (v (+ (dec width) i)) (v i)))]
    (into ()
          (keep (fn [i]
                  (let [d (window-diff v i)]
                    (when (> 1000 d)
                      (list (subvec v i (+ i width)) d)))))
          (range (- (count v) (inc width)) -1 -1))))

;; without hints for timing
(defn xrmt35 [times]
  (let [v (vec times)
        width 8]
    (into ()
          (keep (fn [i]
                  (let [d (- (v (+ (dec width) i)) (v i))]
                    (when (> 1000 d)
                      (list (subvec v i (+ i width)) d)))))
          (range (- (count v) (inc width)) -1 -1))))





;; fastest, with compatible format
(defn xrmt3 [times]
  (let [v (vec times)
        width 8]
    (into ()
          (keep (fn [i]
                  (let [i (long i)
                        d (- ^long (v (+ (dec width) i)) ^long (v i))]
                    (when (> 1000 d)
                      (list (subvec v i (+ i width)) d)))))
          (range (- (count v) (inc width)) -1 -1))))

;; (set! *unchecked-math* false)



;; not faster with cast
(defn xrmt31 [times]
  (let [v (vec times)
        width 8]
    (into ()
          (keep (fn [i]
                  (let [i (long i)
                        d (- (long (v (+ (dec width) i))) (long (v i)))]
                    (when (> 1000 d)
                      (list (subvec v i (+ i width)) d)))))
          (range (- (count v) (inc width)) -1 -1))))



(defn xrmt4 [times]
  (let [v (vec times)
        width 8]
    (into []
          (keep (fn [i]
                  (let [i (long i)
                        d (- ^long (v (+ (dec width) i)) ^long (v i))]
                    (when (> 1000 d)
                      (list (subvec v i (+ i width)) d)))))
          (range (- (count v) (dec width))))))
