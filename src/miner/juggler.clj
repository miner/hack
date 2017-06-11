;; http://mathworld.wolfram.com/JugglerSequence.html
;; Rather surprisingly, all integers appear to eventually reach 1, a conjecture that holds
;; at least up to 10^6 (E. W. Weisstein, Jan. 23, 2006).

(ns miner.juggler)

;; Note: juggler starting at 115 can overflow long temporarily

;; Doesn't matter for sqrt.  Would matter for integer +/-
(def max-precise-long-double (double (dec (bit-shift-left 1 53))))

(defn floor [x]
  (if (< x (double Long/MAX_VALUE))
    (long (Math/floor x))
    (bigint (Math/floor x))))

(defn juggler [j]
  ;; returns next juggler value after j
  (if (even? j)
    (floor (Math/sqrt j))
    (floor (Math/sqrt (* j j j)))))

;; Oops, vlast was really just built-in peek
(defn vlast [vect]
  ;; last element of vector, or nil for empty vector
  (when (pos? (count vect))
    (nth vect (dec (count vect)))))

;; SOME OF THIS MOVED TO halfbaked

;; simple and lazy, but kind of slow
(defn conv-seq
  ([f x] (conv-seq f x 1000))
  ([f x limit]
   (dedupe (take limit (iterate f x)))))


;; Does not expect nil as a starting X
(defn converge-seq
  "Eager iteration of f, starting with x.  Terminates when result is repeated
  consecutively (reaches a fixed point).  Equality check calls eq? (default =).  Returns nil
  if limit count is exceeded (default 1000)."
  ([f x] (converge-seq f x 1000 = []))
  ([f x limit] (converge-seq f x limit = []))
  ([f x limit eq?] (converge-seq f x limit eq? []))
  ([f x limit eq? res]
   (if (eq? (peek res) x)
     res
     (when (pos? limit)
       (recur f (f x) (dec limit) eq? (conj res x))))))


(defn jseq [n]
  (if (pos? n)
    (converge-seq juggler n)
    []))

(defn jsteps [n]
  (dec (count (jseq n))))


;; should try a transducer version


(def jug77 [77, 675, 17537, 2322378, 1523, 59436, 243, 3787, 233046, 482, 21, 96, 9, 27,
            140, 11, 36,6, 2, 1])

(assert (= jug77 (jseq 77)))

;; number of steps to reach 1 for n=1,2...
;; "steps" is one less than resulting sequence count
(def a7320 [0, 1, 6, 2, 5, 2, 4, 2, 7, 7, 4, 7, 4, 7, 6, 3, 4, 3, 9, 3, 9, 3, 9, 3, 11, 6,
            6, 6, 9, 6, 6, 6, 8, 6, 8, 3, 17, 3, 14, 3, 5, 3, 6, 3, 6, 3, 6, 3, 11, 5, 11,
            5, 11, 5, 11, 5, 5, 5, 11, 5, 11, 5, 5, 3, 5, 3, 11, 3, 14, 3, 5, 3, 8, 3, 8, 3,
            19, 3, 8, 3, 10, 8, 8, 8, 11, 8, 10, 8, 11, 8, 11, 8, 11, 8, 8, 8, 11])

;; Not sure what's happening with this but it give Arithmetic Overflow
;; (assert (= a7320 (map jsteps (range 1 (inc (count a7320))))) )

