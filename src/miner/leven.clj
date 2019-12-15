;;; Levenshtein distance
;;; Challenge https://purelyfunctional.tv/?p=40244
;;; https://en.wikipedia.org/wiki/Levenshtein_distance

(ns miner.leven)

(defn leven-slow [xs ys]
  (cond (= xs ys) 0
        (empty? xs) (count ys)
        (empty? ys) (count xs)
        :else (min (inc (leven-slow (rest xs) ys))
                   (inc (leven-slow xs (rest ys)))
                   (+ (if (= (first xs) (first ys)) 0 1)
                      (leven-slow (rest xs) (rest ys))))))


;; Memoized recursive function, a mash-up of memoize and fn
(defmacro mrfn
  "Returns an anonymous function like `fn` but recursive calls to the given `name` within
  `body` use a memoized version of the function, potentially improving performance (see
  `memoize`).  Only simple argument symbols are supported, not varargs or destructing or
  multiple arities.  Memoized recursion requires explicit calls to `name` so the `body`
  should not use recur to the top level."
  [name args & body]
  {:pre [(simple-symbol? name) (vector? args) (seq args) (every? simple-symbol? args)]}
  (let [akey (if (= (count args) 1) (first args) args)]
    ;; name becomes extra arg to support recursive memoized calls
    `(let [f# (fn [~name ~@args] ~@body)
           mem# (atom {})]
       (fn mr# [~@args]
         (if-let [e# (find @mem# ~akey)]
           (val e#)
           (let [ret# (f# mr# ~@args)]
             (swap! mem# assoc ~akey ret#)
             ret#))))))

(defn levenshtein
  "returns the Levenshtein distance between the strings a and b"
  [a b]
  (let [mrlev (mrfn lev [xs ys]
                    (cond (empty? xs) (count ys)
                          (empty? ys) (count xs)
                          :else (min (inc (lev (rest xs) ys))
                                     (inc (lev xs (rest ys)))
                                     (+ (if (= (first xs) (first ys)) 0 1)
                                        (lev (rest xs) (rest ys))))))]
    (if (= a b)
      0
      (mrlev (seq a) (seq b)))))



;; vectorizing is slightly faster, probably because it runs in "reverse" naturally
;; but I suspect the original vectorization is somewhat expensive
(defn leven6
  "returns the Levenshtein distance between the strings a and b"
  [a b]
  (let [mrlev (mrfn lev [xs ys]
                    (cond (empty? xs) (count ys)
                          (empty? ys) (count xs)
                          :else (min (inc (lev (pop xs) ys))
                                     (inc (lev xs (pop ys)))
                                     (+ (if (= (peek xs) (peek ys)) 0 1)
                                        (lev (pop xs) (pop ys))))))]
    (if (= a b)
      0
      (mrlev (vec a) (vec b)))))


(defn leven7
  "returns the Levenshtein distance between the strings a and b"
  [a b]
  (let [mrlev (mrfn lev [xs ys]
                    (cond (empty? xs) (count ys)
                          (empty? ys) (count xs)
                          :else (min (inc (lev (pop xs) ys))
                                     (inc (lev xs (pop ys)))
                                     (+ (if (= (peek xs) (peek ys)) 0 1)
                                        (lev (pop xs) (pop ys))))))]
    (if (= a b)
      0
      (mrlev (into () a) (into () b)))))

;; somewhat faster to run in reverse
(defn leven2
  "returns the Levenshtein distance between the strings a and b"
  [a b]
  (let [mrlev (mrfn lev [xs ys]
                    (cond (empty? xs) (count ys)
                          (empty? ys) (count xs)
                          :else (min (inc (lev (rest xs) ys))
                                     (inc (lev xs (rest ys)))
                                     (+ (if (= (first xs) (first ys)) 0 1)
                                        (lev (rest xs) (rest ys))))))]
    (if (= a b)
      0
      (mrlev (reverse a) (reverse b)))))

;; not faster than leven2
(defn leven3
  "returns the Levenshtein distance between the strings a and b"
  [a b]
  (let [mrlev (mrfn lev [xs ys]
                    (cond (zero? (count xs)) (count ys)
                          (zero? (count ys)) (count xs)
                          :else (min (inc (lev (pop xs) ys))
                                     (inc (lev xs (pop ys)))
                                     (+ (if (= (peek xs) (peek ys)) 0 1)
                                        (lev (pop xs) (pop ys))))))]
    (if (= a b)
      0
      (mrlev (vec a) (vec b)))))




(defn smoke-test-lev
  ([] (smoke-test-lev levenshtein))
  ([lev]
   (assert (zero? (lev "" "")))
   (assert (= 1 (lev "test" "tent")))
   (assert (= 1 (lev "tent" "test")))
   (assert (= 2 (lev "lawn" "flaw")))
   (assert (= 2 (lev "flaw" "lawn")))
   (assert (= 3 (lev "kitten" "sitting")))
   (assert (= 3 (lev "sitting" "kitten")))
   (assert (zero? (lev "levenshtein" "levenshtein")))
   (assert (= 1 (lev "blevenshtein" "alevenshtein")))
   (assert (= 2 (lev "levenshtein" "levenshtien")))
   true))


#_ (quick-bench (smoke-test-lev leven-slow))
;; Execution time mean : 14.2 sec

#_ (quick-bench (smoke-test-lev levenshtein))
;; Execution time mean : 1.25 ms



;; https://gist.github.com/ericnormand/704f07e8aa041edace319afead3aac73
;; Eric's version

(defn elev
  ([a b]
   (let [f (memoize elev)]
    (f f a b (count a) (count b))))
  ([f a b i j]
   (cond
     (zero? i)
     j

     (zero? j)
     i

     :else
     (min
      (inc (f f a b (dec i) j))
      (inc (f f a b i (dec j)))
      (+ (if (= (.charAt ^String a (dec i))
                (.charAt ^String b (dec j)))
           0
           1)
         (f f a b (dec i) (dec j)))))))


;; refactored by SEM
;; fastest
(defn elev2 [a b]
  (let [f (mrfn f [i j]
                (cond
                 (zero? i)  j
                 (zero? j)  i
                 :else (min
                        (inc (f (dec i) j))
                        (inc (f i (dec j)))
                        (+ (if (= (.charAt ^String a (dec i))
                                  (.charAt ^String b (dec j))) 0 1)
                           (f (dec i) (dec j))))))]
    (f (count a) (count b))))


#_ (quick-bench (smoke-test-lev elev2))
;; Execution time mean : 0.6 ms




(defmacro dmrfn
  "Returns an anonymous function like `fn` but recursive calls to the given `name` within
  `body` use a memoized version of the function, potentially improving performance (see
  `memoize`).  Only simple argument symbols are supported, not varargs or destructing or
  multiple arities.  Memoized recursion requires explicit calls to `name` so the `body`
  should not use recur to the top level."
  [name args & body]
  {:pre [(simple-symbol? name) (vector? args) (seq args) (every? simple-symbol? args)]}
  (let [akey (if (= (count args) 1) (first args) args)]
    ;; name becomes extra arg to support recursive memoized calls
    `(let [f# (fn [~name ~@args] ~@body)
           mem# (atom {})]
       (fn mr# [~@args]
         (if-let [e# (find @mem# ~akey)]
           (deref (val e#))
           (let [d# (delay (f# mr# ~@args))]
             (swap! mem# assoc ~akey d#)
             @d#))))))


(defmacro memofn
  [name args & body]
  `(let [cache# (atom {})]
     (fn ~name [& args#]
       (let [update-cache!# (fn update-cache!# [state# args#]
                              (if-not (contains? state# args#)
                                (assoc state# args#
                                       (delay
                                        (let [~args args#]
                                          ~@body)))
                                state#))]
         (let [state# (swap! cache# update-cache!# args#)]
           (-> state# (get args#) deref))))))


;; no noticeable speed difference using delays -- probably better in a multi-threaded environment
(defn elev3 [a b]
  (let [f (dmrfn f [i j]
                (cond
                 (zero? i)  j
                 (zero? j)  i
                 :else (min
                        (inc (f (dec i) j))
                        (inc (f i (dec j)))
                        (+ (if (= (.charAt ^String a (dec i))
                                  (.charAt ^String b (dec j))) 0 1)
                           (f (dec i) (dec j))))))]
    (f (count a) (count b))))

(defn elev4 [a b]
  (let [f (memofn f [i j]
                (cond
                 (zero? i)  j
                 (zero? j)  i
                 :else (min
                        (inc (f (dec i) j))
                        (inc (f i (dec j)))
                        (+ (if (= (.charAt ^String a (dec i))
                                  (.charAt ^String b (dec j))) 0 1)
                           (f (dec i) (dec j))))))]
    (f (count a) (count b))))




#_
(def nstrs (map (juxt str #(str (inc %))) (range 10001000 10002000)))

#_
(def mstrs (into (repeat 100 (first nstrs)) nstrs))

#_
(quick-bench (reduce + (pmap #(apply elev3 %) mstrs)))


;;; another good blog post
;;; https://www.occasionalenthusiast.com/efficient-recursive-levenshtein-edit-distance-algorithm/

;;; gives grid (matrix) implementation
;;; SEM fixed typo

(defn ld-grid-recursive
  "Calculate the Levenshtein (edit) distance recursively using
  a grid representation"
  ([seqa seqb]
   (ld-grid-recursive seqa seqb 1 (vec (range (inc (count seqa))))))
  ;
  ([seqa seqb r vx]
  (if (empty? seqb)
    (peek vx)
    (let [r-fn (fn [v [a x0 x1]]
                 (let [k (if (= a (first seqb)) 0 1)]
                   (conj v (min (+ x1 1)           ; above
                                (+ (peek v) 1)     ; left
                                (+ x0 k)))))       ; diag
          vx (reduce r-fn [r] (map vector seqa vx (rest vx)))]
      (ld-grid-recursive seqa (rest seqb) (inc r) vx)))))




;; refactored by SEM
(defn lev-grid [a b]
  (let [as (seq a)]
    (loop [bs (seq b) r 1 qx (vec (range (inc (count a))))]
      (if (empty? bs)
        (peek qx)
        (let [stepx (fn [rx [a xdiag xup]]
                      ;; min of above, left, diag
                      (conj rx (min (inc xup)           
                                    (inc (peek rx))
                                    (+ xdiag (if (= a (first bs)) 0 1)))))
              rx (reduce stepx [r] (map vector as qx (rest qx)))]
          (recur (rest bs) (inc r) rx))))))

;; pretty good
(defn lev-loops [a b]
  (let [as (seq a)]
    (loop [bs (seq b) r 1 qx (vec (range (inc (count a))))]
      (if (empty? bs)
        (peek qx)
        (let [stepx (fn [rx a xdiag xup]
                      ;; min of above, left, diag
                      (conj rx (min (inc xup)           
                                    (inc (peek rx))
                                    (+ xdiag (if (= a (first bs)) 0 1)))))
              rx (loop [as as qx (seq qx) nqx (rest qx) rx [r]]
                   (if (empty? as)
                     rx
                     (recur (rest as) (rest qx) (rest nqx)
                            (stepx rx (first as) (first qx) (first nqx)))))]
          (recur (rest bs) (inc r) rx))))))





;; FASTEST by far, from above blog
(defn ld-grid-iter
  "Calculate the Levenshtein (edit) distance iteratively using
  a grid representation"
  [seqa seqb]
  (let [va (vec seqa)
        vb (vec seqb)
        clen (inc (count va))
        rlen (inc (count vb))
        rx (vec (range clen))]
    (loop [r 1, c 1, rx rx, ry (assoc rx 0 1)]
      (if (= r rlen)
        (peek rx)
        (if (= c clen)
          (recur (inc r) 1 ry (assoc ry 0 (inc r)))
          (let [k (if (= (va (dec c)) (vb (dec r))) 0 1)
                ry (assoc ry c (min (+ (rx c) 1)             ; above
                                    (+ (ry (dec c)) 1)       ; left
                                    (+ (rx (dec c)) k)))]    ; diagonal
            (recur r (inc c) rx ry)))))))


;; SEM refactored
;; NEW FASTEST but only slightly
(defn lev-gi
  "Calculate the Levenshtein (edit) distance iteratively using
  a grid representation"
  [a b]
  (let [va (vec a)
        acnt (count va)
        vb (vec b)
        bcnt (count vb)]
    ;; qx is row of values above
    ;; rx is current row being constructed
    (loop [r 1, c 1, qx (vec (range (inc acnt))), rx [1]]
      (if (> r bcnt)
        (peek qx)
        (if (> c acnt)
          (recur (inc r) 1 rx [(inc r)])
          (recur r (inc c) qx (conj rx 
                                    ;; above, left, diag
                                    (min (inc (qx c))
                                         (inc (rx (dec c)))
                                         (+ (qx (dec c))
                                            (if (= (va (dec c)) (vb (dec r))) 0 1))))))))))




;; not sure the extra complication is worth saving an extra vector, not noticeably faster
(defn lev-gx9
  "Calculate the Levenshtein (edit) distance iteratively using
  a grid representation"
  [a b]
  (let [va (vec a)
        acnt (count va)
        vb (vec b)
        bcnt (count vb)]
    (cond (zero? acnt) bcnt
          (zero? bcnt) acnt
          :else
              ;; qx is row of values above
              ;; rx is current row being constructed
              (loop [r 1, c 1, qx (vec (range (inc acnt))), rx [1]]
                (if (> c acnt)
                  (if (= r bcnt)
                    (peek rx)
                    (recur (inc r) 1 rx [(inc r)]))
                  (recur r (inc c) qx (conj rx 
                                            ;; above, left, diag
                                            (min (inc (qx c))
                                                 (inc (rx (dec c)))
                                                 (+ (if (= (va (dec c)) (vb (dec r))) 0 1)
                                                    (qx (dec c)))))))))))





