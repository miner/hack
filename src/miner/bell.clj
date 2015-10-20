(ns miner.bell)

;; Original idea:
;; http://maurits.wordpress.com/2012/05/23/calculating-bell-numbers-in-a-single-tweet/

;; Background:
;; http://en.wikipedia.org/wiki/Bell_number

(defn maurits-bell [n]
  (loop [n n s [1] b s]
    (if (= n 1)
      b
      (recur (dec n)
             (reduce #(conj % (+ %2 (last %))) [(last s)] s)
             (conj b (last s))))))

;; Note (bell 0) produces an error in the original code, but it probably should be [] or [1] as a
;; special case.

;; SEM -- I don't like the use of `last`.  peek is much faster on vectors.

(defn faster-bell [n]
  (loop [n n s [1] b s]
    (if (<= n 1)
      b
      (recur (dec n)
             (reduce #(conj % (+ %2 (peek %))) [(peek s)] s)
             (conj b (peek s))))))

#_ (faster-bell 10)
;=> [1 1 2 5 15 52 203 877 4140 21147]



;; Special case (bell 0) ==> [1] -- there's only one way to partition the empty set.
;; In the usual case, returns a vector of n bell numbers.

;; better termination, use peek instead of last, (bell 0) ==> [1]
(defn works-bell [n]
  (loop [n (dec n) s [1] bs []]
    (let [prev (peek s)]
      (if (<= n 1)
        (conj bs prev)
        (recur (dec n)
               (reduce #(conj % (+ %2 (peek %))) [prev] s)
               (conj bs prev))))))

;; mauritis-bell does too much work on the last recursion and throws it all away, only the last s
;; matters.

(defn my-bell [n]
  (case n
    (0 1) [1]
    (loop [n (dec n) s [1] bs [1]]
      (if (== n 1) 
        (conj bs (peek s))
        (recur (dec n)
               (reduce #(conj % (+ %2 (peek %))) [(peek s)] s)
               (conj bs (peek s)))))))


;; Most of the time you probably only want the nth value (not the full sequence) so it can be
;; simplified like this:
(defn bnth [n]
  (case n
    (0 1) 1
    (loop [n (dec n) s [1]]
      (if (zero? n)
        (peek s)
        (recur (dec n) (reduce #(conj % (+ %2 (peek %))) [(peek s)] s))))))


;; from the comments on the original blog post
(defn rbell
  ([] (rbell [1]))
  ([p] (lazy-seq
        (cons (last p) (rbell (reductions + (last p) p))))))

#_ (take 10 (rbell))

;; My correction for the first "zero-th" element, just to be consistent
(defn rbell2
  ([] (conj (rbell [1]) 1))
  ([p] (lazy-seq
        (cons (last p) (rbell (reductions + (last p) p))))))

;; I wonder if there's a way to do it in reverse so we can avoid the last calls?
;; untested
(defn rbell3
  ([] (conj (rbell [1]) 1))
  ([p] (lazy-seq
        (cons (first p) (rbell (reductions + (first p) p))))))
