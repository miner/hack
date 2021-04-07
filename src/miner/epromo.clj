(ns miner.epromo)

;; https://gist.github.com/ericnormand/098143e8cdbecef0faeba92f730b9cba
;;
;; Essentially, you need to swap the element with its immediate predecessor (if it
;; exists). Write a function promote that takes a predicate and a list. If the predicate is
;; true, the element should be promoted.



;; SEM hacked version of @sw transducer.  Now fastest.  Seems like the best answer to me.
(defn xpromote
  ([pred]
   (fn [rf]
     (let [buf (volatile! ::void)]
       (fn
         ([] (rf))
         ([res] (let [b @buf]
                  (if (identical? b ::void)
                    (rf res)
                    (rf (unreduced (rf res b))))))
         ([res x] (if (pred x)
                    (rf res x)
                    (let [b @buf]
                      (vreset! buf x)
                      (if (identical? b ::void)
                        res
                        (rf res b)))))))))
  ([pred xs]
   (sequence (xpromote pred) xs)))


;;; also inspired by @sw, call pred once per item
;;; changed buf to singleton ::void
;;; refactored
;;; transients improve speed
(defn promote [pred coll]
  (loop [res (transient [])
         p ::void
         xs coll]
    (cond (empty? xs) (persistent! (if (identical? p ::void) res (conj! res p)))
          (pred (first xs)) (recur (conj! res (first xs)) p (rest xs))
          (identical? p ::void) (recur res (first xs) (rest xs))
          :else (recur (conj! res p) (first xs) (rest xs)))))




(defn promote1 [pred coll]
  (if (empty? coll)
    coll
    (reduce (fn [r x]
              (let [p (peek r)]
                (if (and (pred x) (not (pred p)))
                  (-> (pop r) (conj x) (conj p))
                  (conj r x))))
            [(first coll)]
            (rest coll))))






;; storing buffer b at end of result
(defn sp2 [pred coll]
  (if (empty? coll)
    coll
    (let [rb (reduce (fn [rb x]
                       (let [b (peek rb)]
                          (if (pred x) 
                            (conj (conj (pop rb) x) b)
                            (if (identical? b ::init)
                              (conj (pop rb) x)
                              (conj rb x)))))
                     [::init]
                     coll)]
      (if (identical? (peek rb) ::init)
        (pop rb)
        rb))))





;; higher arity conj is a bit slower so we unwrapped it above for better performance
(defn promote-submitted [pred coll]
  (if (empty? coll)
    coll
    (reduce (fn [r x]
              (let [p (peek r)]
                (if (and (pred x) (not (pred p)))
                  (conj (pop r) x p)
                  (conj r x))))
            [(first coll)]
            (rest coll))))


;; slower to try to cache too much.  Probably because cheap pred.


(defmacro assert?
  ([pred form result]
   `(do (assert (~pred ~form ~result)) true))
  ([pred form result & more]
   `(and (assert? ~pred ~form ~result)
         (assert? ~pred ~@more))))


(defn smoke-promo [promote]
  (assert? =
           (promote even? [1 3 5 6]) '(1 3 6 5)
           (promote even? [])  ()
           (promote even? [2 1]) '(2 1)
           (promote even? [0 2 4 6]) '(0 2 4 6)
           (promote even? [0 1 2 3 4 5 6]) '(0 2 1 4 3 6 5)
           (promote even? [1 2 2 2 2]) '(2 2 2 2 1)
           (promote even? (range 0 20 2)) (range 0 20 2)
           (promote even? (range 100))
           [0 2 1 4 3 6 5 8 7 10 9 12 11 14 13 16 15 18 17 20 19 22 21 24 23 26 25 28 27 30
  29 32 31 34 33 36 35 38 37 40 39 42 41 44 43 46 45 48 47 50 49 52 51 54 53 56 55 58 57 60
  59 62 61 64 63 66 65 68 67 70 69 72 71 74 73 76 75 78 77 80 79 82 81 84 83 86 85 88 87 90
            89 92 91 94 93 96 95 98 97 99]
           (take 10 (promote even? (range 1000))) [0 2 1 4 3 6 5 8 7 10]))


;; faster @steffan-westcott
(defn sw-promote [pred xs]
  (if (seq xs)
    (loop [res [] a (first xs) ys (rest xs)]
      (if (seq ys)
        (let [b (first ys)]
          (if (or (pred a) (not (pred b)))
            (recur (conj res a) b (rest ys))
            (recur (conj res b) a (rest ys))))
        (conj res a)))
    []))

;; but slower (assuming cheap pred)
(defn sw-promote2 [pred xs]
  (loop [res [] buf [] xs xs]
    (if (seq xs)
      (let [[x & r] xs]
        (if (pred x)
          (recur (conj res x) buf r)
          (recur (into res buf) [x] r)))
      (into res buf))))

;; SEM list buf is slightly faster than vector.  swp4 with ::void is much faster

(defn sw-promote3 [pred xs]
  (loop [res [] buf () xs xs]
    (if (seq xs)
      (let [[x & r] xs]
        (if (pred x)
          (recur (conj res x) buf r)
          (recur (into res buf) (list x) r)))
      (into res buf))))




;;; SEM hack  -- much faster than sw-promote2
(defn swp4 [pred xs]
  (loop [res [] buf ::void xs xs]
    (cond (empty? xs) (if (identical? buf ::void) res (conj res buf))
          (pred (first xs)) (recur (conj res (first xs)) buf (rest xs))
          (identical? buf ::void) (recur res (first xs) (rest xs))
          :else (recur (conj res buf) (first xs) (rest xs)))))







;; SEM idea: if pred x, it can't be replaced so you can skip next.  Works but extra
;; complication doesn't buy much speed.


;;; SEM idea: partition-by to get runs

;;; very slow
(defn twp [pred coll]
  (let [cpred (complement pred)]
    (loop [res (into [] (take-while pred) coll) xs (sequence (drop-while pred) coll)]
      (if (empty? xs)
        res
        (let [cnt (count res)
              r (into res (take-while cpred) xs)
              dcnt (- (count r) cnt)
              ys (into [] (comp (drop dcnt) (take-while pred)) xs)]
          (if (empty? ys)
            r
            (recur (conj (into (pop r) ys) (peek r))
                   (drop (+ dcnt (count ys)) xs))))))))





(defn lpromo2 [pred coll]
  (if (empty? coll)
    coll
    (loop [r [] a (first coll) bs (rest coll)]
      (if (empty? bs)
        (conj r a)
        (let [pa (pred a)
              b (first bs)
              pb (pred b)]
          (cond (and pa pb (next bs)) (recur (conj r a b) (second bs) (nnext bs))
                pa (recur (conj r a) b (rest bs))
                pb (recur (conj r b) a (rest bs))
                :else (recur (conj r a)  b (rest bs))))))))




;; basically the same as sw
(defn lpromo [pred coll]
  (if (empty? coll)
    coll
    (loop [r [] a (first coll) bs (rest coll)]
      (cond (empty? bs) (conj r a)
            (pred a) (recur (conj r a) (first bs) (rest bs))
            (pred (first bs)) (recur (conj r (first bs)) a (rest bs))
            :else (recur (conj r a)  (first bs) (rest bs))))))



;; hacked by SEM, but not faster to cache (quick pred)
(defn swp2 [pred xs]
  (if (seq xs)
    (loop [res [] a (first xs) pa (pred (first xs)) ys (rest xs)]
      (if (seq ys)
        (let [b (first ys)
              pb (pred b)]
          (if (or pa (not pb))
            (recur (conj res a) b pb (rest ys))
            (recur (conj res b) a pa (rest ys))))
        (conj res a)))
    []))



;; slower @arthurulacerda
(defn ar-promote
  [pred coll]
  (reduce (fn [acc curr]
            (if (empty? acc)
              (conj acc curr)
              (if (and (-> acc last pred not)
                       (-> curr pred))
                (conj (vec (drop-last acc)) curr (last acc))
                (conj acc curr))))
          []
          coll))




(defn peek! [tv]
  (nth tv (dec (count tv))))


;; transients actually slower!
(defn promote4 [pred coll]
  (if (empty? coll)
    coll
    (persistent!
     (reduce (fn [r x]
              (let [p (peek! r)
                    r (pop! r)]
                (if (and (pred x) (not (pred p)))
                  (conj! (conj! r x) p)
                  (conj! (conj! r p) x))))
            (transient [(first coll)])
            (rest coll)))))






;; @jaihindhreddy had a transducer version

;; SEM:  I wonder about `unreduced` vs propagating reduced result????  I'm now thinking
;; unreduced is the right way. (And that's how @sw did it.)

(defn promotor [pred]
  (fn [rf]
    (let [v (volatile! [])]
      (fn
        ([] (rf))
        ([result]
          (let [[x :as buf] @v]
            (if (empty? buf)
              (rf result)
              (let [result (rf result x)]
                (if (reduced? result)
                  result
                  (rf result))))))
        ([result input]
          (if (pred input)
            (rf result input)
            (let [[x :as buf] @v]
              (vreset! v [input])
              (if (empty? buf)
                result
                (rf result x)))))))))


(defn jr-promote [pred coll]
  (into [] (promotor pred) coll))



;; @sw transducer version.  Nice, but seems strange to use the vector buf, plus cat
(defn swxp
  ([pred]
   (comp
     (fn [rf]
       (let [buf (volatile! [])]
         (fn
           ([] (rf))
           ([res] (rf (unreduced (rf res @buf))))
           ([res x]
            (if (pred x)
              (rf res [x])
              (let [y @buf]
                (vreset! buf [x])
                (rf res y)))))))
     cat))
  ([pred xs]
   (sequence (swxp pred) xs)))




;; contributed @i0cus, lazy
(defn i-promote [pred nums]
    (if (seq nums)
      (let [[n1 n2] (take 2 nums)]
        (if (nil? n2)
          (list n1)
          (if (pred n1)
            (cons n1 (lazy-seq (i-promote pred (drop 1 nums))))
            (if (pred n2)
              (cons n2 (lazy-seq (i-promote pred (cons n1 (drop 2 nums)))))
              (cons n1 (lazy-seq (i-promote pred (drop 1 nums))))))))
      (list)))



;; @sw's take on lazy-seq (which should surround cons for better laziness)
(defn swi-promote [pred xs]
  (let [promote' (fn promote' [pred buf xs]
                   (lazy-seq
                    (if-let [xss (seq xs)]
                      (let [x (first xss)
                            rs (rest xss)]
                        (if (pred x)
                          (cons x (promote' pred buf rs))
                          (concat buf (promote' pred [x] rs))))
                      buf)))]
    (promote' pred [] xs)))


;; slightly faster, but not good enough.  Why is smi-promote1 faster?  JIT/cache???
;; Anyway, I like the look of this one.
(defn smi-promote [pred xs]
  (let [promo (fn promo [buf xs]
                (lazy-seq
                 (cond (empty? xs) (when-not (identical? buf ::void) (list buf))
                       (pred (first xs)) (cons (first xs) (promo buf (rest xs)))
                       (identical? buf ::void) (promo (first xs) (rest xs))
                       :else (cons buf (promo (first xs) (rest xs))))))]
    (promo ::void xs)))



;; SEM change buf to ::void, faster than swi, almost as good as transducer version
(defn smi-promote1 [pred xs]
  (let [promote' (fn promote' [pred buf xs]
                   (lazy-seq
                    (if-let [xss (seq xs)]
                      (let [x (first xss)
                            rs (rest xss)]
                        (if (pred x)
                          (cons x (promote' pred buf rs))
                          (if (identical? buf ::void)
                            (promote' pred x rs)
                            (cons buf (promote' pred x rs)))))
                      (when-not (identical? buf ::void) (list buf)))))]
    (promote' pred ::void xs)))


;; prettier but slower!!!
(defn smi-promote2 [pred xs]
  (let [promo (fn promo [pred buf xs]
                 (cond (empty? xs) (if (identical? buf ::void) () (list buf))
                       (pred (first xs)) (lazy-seq (cons (first xs) (promo pred buf (rest xs))))
                       (identical? buf ::void) (recur pred (first xs) (rest xs))
                       :else (lazy-seq (cons buf (promo pred (first xs) (rest xs))))))]
    (promo pred ::void xs)))




;; NOT WORTH IT
(defn smi-promote23 [pred xs]
  (let [promo (fn promo [buf xs]
                (lazy-seq
                 (if-let [xs (seq xs)]
                   (if (pred (first xs))
                     (cons (first xs) (promo buf (rest xs)))
                     (if (identical? buf ::void)
                       (promo (first xs) (rest xs))
                       (cons buf (promo (first xs) (rest xs)))))
                   (if (identical? buf ::void) () (list buf)))))]
    (promo ::void xs)))






;; slower than 2 but maybe cleaner to use first as buf???  I guess not.
(defn smi-promote3 [pred xs]
  (let [promo (fn promo [pred xs]
                (let [buf (first xs)
                      xs (rest xs)]
                 (cond (empty? xs) (if (identical? buf ::void) () (list buf))
                       (pred (first xs)) (lazy-seq (cons (first xs)
                                                         (promo pred (cons buf (rest xs)))))
                       (identical? buf ::void) (recur pred xs)
                       :else (lazy-seq (cons buf (promo pred xs))))))]
    (promo pred (cons ::void xs))))
