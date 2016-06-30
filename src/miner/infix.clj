(ns miner.infix)

;; Published as https://gist.github.com/miner/5224709
;;
;; Inspired by the excellent book "The Joy of Clojure".
;; http://fogus.me/fun/unfix/infix-src.html
;; 
;; Converted to be used as a data-reader by Steve Miner.  The main change is to preserve
;; the prefix forms rather than calculating the result.  Also, the reader gets symbols, not
;; the actual functions.

(def && #(and % %2))
(def || #(or  % %2))

(def ops '[- + * / < > && || =])
(def rank (zipmap ops (iterate inc 1)))
(def op? rank)


(defn infix
  [[a b & [c d & m]]]
  (cond
   (vector? a) (recur (list* (infix a) b c d m))
   (vector? c) (recur (list* a b (infix c) d m))
   (op? b) (if (and d (< (rank b 0) (rank d 0)))
             (recur (list a b (infix (list* c d m))))
             (recur (list* (list b a c) d m)))
   :else a))


(def infix-reader infix)


(comment

  (binding [*data-readers* {'x/infix #'infix-reader}]
    (read-string "#x/infix [2 + 3 * 4]"))
  ;=> (+ 2 (* 3 4))

  (binding [*data-readers* {'x/infix #'infix-reader}]
    (read-string "#x/infix [2 * b + 10 * [7 - a]]"))
  ;=> (+ (* 2 b) (* 10 (- 7 a)))

  ;; for any kind of user input it would be better to use clojure.edn
  (clojure.edn/read-string {:readers {'x/infix #'infix-reader}} "#x/infix [2 * b + 10 * [7 - a]]")
  ;=> (+ (* 2 b) (* 10 (- 7 a)))
  
  )




(comment 
;;; hmmm, maybe the e is superfluous
(defn infix-ORIG
  [[a b & [c d e & m]]]
  (cond
   (vector? a) (recur (list* (infix-ORIG a) b c d e m))
   (vector? c) (recur (list* a b (infix-ORIG c) d e m))
   (op? b) (if (and d (< (rank b 0) (rank d 0)))
             (recur (list a b (infix-ORIG (list* c d e m))))
             (recur (list* (list b a c) d e m)))
   :else a))
)

(comment
  ;; make a generative test with random ints and ops
  ;; check if always (= (infix-ORIG gen) (infix gen))
  ;; answer YES, so the e arg is superfluous
  
  (require '[clojure.test.check :as tc])
  (require '[clojure.test.check.properties :as prop])
  (require '[clojure.test.check.generators :as gen])
  
  (def ggg (gen/fmap (fn [[xs os]] (vec (rest (interleave os xs))))
                     (gen/tuple (gen/vector gen/int 100)
                                (gen/vector (gen/elements ops) 100))))
  
  (def ppp (prop/for-all [a ggg] (= (infix a) (infix-ORIG a))))

  (tc/quick-check 100000 ppp)

  )
