(ns miner.dijkstra-primes)


;;; SEM: I don't recommend this code.  It does a linear search of existing composites which
;;; is not a good idea.  A similar concept using a map for bookkeeping is implemented in my
;;; sieve.clj `oprimes`.  Keeping this file for historical purposes.  Don't use it.

;; ----------------------------------------------------------------------
;; http://heinrichhartmann.com/2016/04/03/Dijkstra's-Prime-Number-Algorithm.html
;; https://github.com/HeinrichHartmann/DijkstraPrimes/blob/master/Primes.lua

;; Converted to Clojure by SEM.  Note that there are lots of shadowing and recursive calls in
;; the Clojure code to avoid the mutation in the original code.  The Clojure loops are a bit
;; ugly.  Not sure if this is the best way to do things.  However, the performance is pretty
;; good.
;;
;; By the way, it was a bit of a challenge to get everything right with the differences between
;; Lua 1-based tables and Clojure 0-based vectors.  I found an embarrassing off-by-one bug
;; in my earlier attempt that didn't manifest until the 218th prime.


;; Note: Lua uses 1-based arrays
;;
;; function primes(N)
;;   local P, Q, x, limit = {2}, {}, 1, 4
;;   local is_prime = function(x)
;;     for k = 2, #Q do
;;       if x > Q[k] then Q[k] = Q[k] + P[k] end
;;       if x == Q[k] then return false end
;;     end
;;     return true
;;   end
;;   while #P < N do
;;     repeat
;;       x = x + 2
;;       if x >= limit then
;;         Q[#Q+1], limit = limit, P[#Q+2]^2
;;       end
;;     until is_prime(x)
;;     P[#P+1] = x
;;   end
;;   return P
;; end


;; Transient don't directly support peek! and update! so I wrote these convenience functions
;; to make it easier to port code from standard sequence functions to transients.  These
;; only work for transient vectors.  peek! is a misnomer in that it is not actually
;; destructive.  However, the bang pattern for transient transformation holds so that's what
;; I call it.

#_
(defn peek! [tv]
  (nth tv (dec (count tranv))))

;; See CLJ-1848.  Full patch should handle arities as real update does, but call assoc!
#_
(defn update! [tv i f v]
  (assoc! tv i (f (tv i) v)))


;; See CLJ-1872 for empty? breakage on transient
#_
(defn empty?! [tv] (zero? (count tv)))


;; SEM need a whole file of transient work-arounds!


;; using transients for performance
(defn dijkstra-primes [maximum]
  ;; p is vector of primes found
  ;; q is vector of multiples of corresponding primes
  ;; x is value to test
  ;; lim is the limit for this test

  (if (< maximum 3)
    []
    (let [peek! (fn [tv] (nth tv (dec (count tv))))
          update! (fn [tv i f val] (assoc! tv i (f (tv i) val)))

          q-prime (fn [x q p]
                    ;; returns [q2... true] if x is prime, [q2... false] if not
                    ;; q2 is updated vector of multiples
                    ;; p is vector of known primes
                    (let [cnt (count q)]
                      (loop [k 1 q q]
                        (if (< k cnt)
                          (if (> x (q k))
                            (recur k (update! q k + (p k)))
                            (if (= x (q k))
                              (conj! q false)
                              (recur (inc k) q)))
                          (conj! q true))))) ]

      (loop [p (transient [2]) q (transient []) x 3 lim 4]
        ;; debug (println "\np =" p "\n")
        (if (>= x maximum)
          (persistent! p)
          (let [q (if (>= x lim) (conj! q lim) q)
                lim (if (>= x lim)
                      (let [p2 (long (p (count q)))] (* p2 p2))
                      lim)
                qp (q-prime x q p)]
            (recur (if (peek! qp) (conj! p x) p) (pop! qp) (+ x 2) lim)))))))


;; https://primes.utm.edu/lists/small/10000.txt
(defn smoke []
  (= 104729 (last (dijkstra-primes 10000))))


;; Another reference:
;; https://vanemden.wordpress.com/2011/01/15/another-scoop-by-dijkstra/
;;
;; [What we called "q"] maintains a location for each prime and ensures that each of these
;; locations contains the least multiple of the prime that is not less than the number under
;; consideration. These multiples need only an addition to be updated; a division is never
;; needed.


