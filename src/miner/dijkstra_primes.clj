(ns miner.dijkstra-primes)

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

(defn prime? [x q p]
  ;; returns [true q] if x is prime, [false q] if not
  ;; q is updated vector of multiples
  ;; p is vector of known primes
  (let [cnt (count q)]
    (loop [k 1 q q]
      ;; debug   (println q x k)
      (if (< k cnt)
        (if (> x (q k))
          (recur k (update q k + (p k)))
          (if (= x (q k))
            [false q]
            (recur (inc k) q)))
        [true q]))))

(defn next-limit ^long [q p]
  ;; square of the next prime
  (let [p2 (long (p (count q)))]
    (* p2 p2)))


(defn dijkstra-primes [n]
  ;; p is vector of primes found
  ;; q is vector of multiples of corresponding primes
  ;; x is value to test
  ;; lim is the limit for this test
  (loop [p [2] q [] x 3 lim 4]
    ;; debug (println "\np =" p "\n")
    (if (>= (count p) n)
      p
      (let [q (if (>= x lim) (conj q lim) q)
            lim (if (>= x lim) (next-limit q p) lim)
            [px? q] (prime? x q p)]
        (recur (if px? (conj p x) p) q (+ x 2) lim)))))


;; https://primes.utm.edu/lists/small/10000.txt
(defn smoke-test []
  (= 104729 (last (dijkstra-primes 10000))))


;; Another reference:
;; https://vanemden.wordpress.com/2011/01/15/another-scoop-by-dijkstra/
;;
;; [What we called "q"] maintains a location for each prime and ensures that each of these
;; locations contains the least multiple of the prime that is not less than the number under
;; consideration. These multiples need only an addition to be updated; a division is never
;; needed.


