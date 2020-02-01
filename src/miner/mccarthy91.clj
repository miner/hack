(ns miner.mccarthy91)

;; McCarthy-91 function (1968)
;; http://oeis.org/A103847
;; https://en.wikipedia.org/wiki/McCarthy_91_function

;; benchmark numbers are old.  Should be retested with current environment.
;; With Clojure 1.10.1, it seems like the naive implementation with nested-recursion is just
;; as fast as the others.  Surprising, but maybe the Java version matters.  Could be a JIT
;; optimization that's making it fast.  Type hints matter a lot.  Unboxed math a little bit.


(defn m91 [n]
  (if (> n 100)
      (- n 10)
	  (m91 (m91 (+ n 11)))))


;; huge performance difference with hints
(defn mh91 ^long [^long n]
  (if (> n 100)
      (- n 10)
	  (mh91 (mh91 (+ n 11)))))

;; keeping track of all the internal states
;; v = [n], result is vector of all n values
(defn seq91 [v]
  (let [n (peek v)]
    (if (> n 100)
      (conj v (- n 10))
      (-> v
          (conj (+ n 11))
          seq91
          seq91))))

;; hinting doesn't help seq91, you've got to pay the collection tax




#_
(defn m91t [n]
  (print " " n)
  (if (> n 100)
      (- n 10)
	  (m91t (m91t (+ n 11)))))

#_
(defn nr91t
  ([n] (nr91t 1 n))
  ([c n]
   (print " " n)
    (cond (zero? c) n
	  	  (> n 100) (recur (dec c) (- n 10))
		  :else (recur (inc c) (+ n 11)))))



(defn seqnr91
  ([v] (seqnr91 1 v))
  ([c v]
   (let [n (peek v)]
     (cond (zero? c) v
           (> n 100) (recur (dec c) (conj v (- n 10)))
           :else (recur (inc c) (conj v (+ n 11)))))))

;; interesting than number of internal steps consistently goes down, and is always even
#_
(sequence (comp (map vector) (map seqnr91) (map count) (filter #(> % 2))) (range 200))




(defn nr91
  ([n] (nr91 1 n))
  ([c n]
    (cond (zero? c) n
	  	  (> n 100) (recur (dec c) (- n 10))
		  :else (recur (inc c) (+ n 11)))))

;; loop helps with primitives when it can (need to confirm, reference)
(defn nr91b [n]
  (loop [c 1 n n]
    (if (zero? c)
		n
	  	(if (> n 100)
			(recur (dec c) (- n 10))
		  	(recur (inc c) (+ n 11))))))

(defn nr91c [n]
  (loop [c 1 n n]
    (cond (zero? c) n
	  	  (> n 100) (recur (dec c) (- n 10))
		  :else (recur (inc c) (+ n 11)))))

(defn s91 [n]
  (if (> n 100)
      (- n 10)
	  (recur (s91 (+ n 11)))))

;; much faster with just hints
(defn sh91 ^long [^long n]
  (if (> n 100)
      (- n 10)
	  (recur (sh91 (+ n 11)))))


;; a bit faster with unchecked math

(set! *unchecked-math* :warn-on-boxed)



;; fastest 8.880903 µs
(defn nr91a
  ([n] (nr91a 1 n))
  ([^long c ^long n]
    (cond (zero? c) n
	  	  (> n 100) (recur (dec c) (- n 10))
		  :else (recur (inc c) (+ n 11)))))


;; overhinting doesn't help
(defn nr91aaa
  (^long [^long n] (nr91aaa 1 n))
  (^long [^long c ^long n]
    (cond (zero? c) n
	  	  (> n 100) (recur (dec c) (- n 10))
		  :else (recur (inc c) (+ n 11)))))

;; about the same
(defn nr91d [^long n]
  (loop [c 1 n n]
    (cond (zero? c) n
	  	  (> n 100) (recur (dec c) (- n 10))
		  :else (recur (inc c) (+ n 11)))))

(defn um91 ^long [^long n]
  (if (> n 100)
      (- n 10)
	  (um91 (um91 (+ n 11)))))

;; fast 10.816939 µs
(defn su91 ^long [^long n]
  (if (> n 100)
      (- n 10)
	  (recur (su91 (+ n 11)))))

(set! *unchecked-math* false)



(defn smoke-jmc
  ([] (smoke-jmc m91))
  ([jmc]
   (assert (= 9100 (reduce + (map jmc (range 100)))))
   true))

   
