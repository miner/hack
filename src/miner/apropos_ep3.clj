;; No (ns...) because not supposed to be useful code.

;; Best code is digs


;; experiments inspired by @apropos_cast episode #3
;; which also mentioned my solution for episode #2



(defn WORKS-digits+rev [n]
  (loop [vdigits [n]]
    (let [remaining (peek vdigits)]
      (if (< remaining 10)
        (let [rev (conj (pop vdigits) remaining)]
          (into (seq rev) rev))
        (recur (conj (pop vdigits) (rem remaining 10) (quot remaining 10)))))))


(defn digits+rev [n]
  (loop [digits (list n)]
    (let [remaining (peek digits)]
      (if (< remaining 10)
        (let [leading (conj (pop digits) remaining)]
          (concat leading (reverse leading)))
        (recur (conj (pop digits) (rem remaining 10) (quot remaining 10)))))))


(defn digs1 [n]
  (loop [vdigits [n]]
    (let [remaining (peek vdigits)]
      (if (< remaining 10)
        (rseq vdigits)
        (recur (conj (pop vdigits) (rem remaining 10) (quot remaining 10)))))))

;; slower than digs with loop/2
(defn digs-in-one [n]
  (loop [digits (list n)]
    (let [remaining (peek digits)]
      (if (< remaining 10)
        digits
        (recur (conj (pop digits) (rem remaining 10) (quot remaining 10)))))))



;; But I don't like mixing two parts like this, even if it's slightly faster
(defn digs+rev [n]
  #_ {:pre [(int? n) (>= n 0)]}
  (loop [digits () rev [] remainder n]
    (if (< remainder 10)
      (concat (conj digits remainder) (conj rev remainder))
      (let [d (rem remainder 10)]
      (recur (conj digits d)
             (conj rev d)
             (quot remainder 10))))))


(defn digs2
  ([n] (digs2 () n))
  ([digits remainder]
    (if (< remainder 10)
      (conj digits remainder)
      (recur (conj digits (rem remainder 10)) (quot remainder 10)))))

;; getting silly
(defn digs2+rev [n]
  (digs2 (reverse (digs2 n)) n))

(defn digs3+rev [n]
  (digs2 (vec (digs2 n)) n))

(defn digs4+rev [n]
  (let [ds (digs2 n)]
    (concat ds (reverse ds))))

;; @mfikes int->ints
;; consumes stack
(defn int->ints [n]
  (if (< n 10)
    [n]
    (conj (int->ints (quot n 10)) (rem n 10))))


;; not as fast as the list version
(defn rdigs [n]
  (loop [digits [] remainder n]
    (if (< remainder 10)
      (conj digits remainder)
      (recur (conj digits (rem remainder 10)) (quot remainder 10)))))




;; simple but slower
(defn sdigs [n]
  (map #(- (long %) (long \0)) (str n)))

;; about half the time but being eager with mapv, but still twice list digs
(defn svdigs [n]
  (mapv #(- (long %) (long \0)) (str n)))



;; slightly faster than sdigs, but still much slower than digs
(defn cdigs [n]
  (mapv #(Character/digit ^Character % 10) (str n)))



(defn digits+rev [n]
  (let [ds (digs n)]
    (concat ds (reverse ds))))


(defn rev-digits [n]
  (loop [acc [n]]
    (let [remainder (peek acc)]
      (if (< remainder 10)
        acc
        (recur (conj (pop acc) (rem remainder 10) (quot remainder 10)))))))



;; best, simple, fast
(defn digits [n]
  {:pre [(int? n) (>= n 0)]}
  (loop [digs () remainder n]
    (if (< remainder 10)
      (conj digs remainder)
      (recur (conj digs (rem remainder 10)) (quot remainder 10)))))

(defn digits+rev [n]
  (let [ds (digits n)]
    (concat ds (reverse ds))))


;; based on preserving-reduced from clojure/core.clj
;; the extra level of `reduced` preserves the early termination value
(defn rf-reduce [rf result xs]
  (let [rrf (fn [r x] (let [ret (rf r x)] (if (reduced? ret) (reduced ret) ret)))]
    (reduce rrf result xs)))

;; They asked for a stateful transducer solution...

(defn revcat
  "Returns a lazy sequence of items from coll followed by the items in reverse order.  When
  no collection is provided, returns a stateful transducer that buffers a reversed collection
  of inputs.  Each input passes through to the output.  On reaching the end of input, the
  buffered elements are passed in as extra inputs. Inspired by @apropos_cast episode #3."
  ([]
   (fn [rf]
     (let [rv (volatile! ())]
       (fn
         ([] (rf))
         ([result] (rf (unreduced (rf-reduce rf result @rv))))
         ([result input]
          (vswap! rv conj input)
          (rf result input))))))
  ([coll] (concat coll (reverse coll))))

(defn apropos3 [n]
  (sequence (revcat) (digits n)))



  
