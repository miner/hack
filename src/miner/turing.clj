(ns miner.turing
  (:require [clojure.string :as str]))

;;; TOTALLY IMPRACTICAL -- don't use this!

;; https://www.quantamagazine.org/amateur-mathematicians-find-fifth-busy-beaver-turing-machine-20240702/

;; https://data.jigsaw.nl/Rado_1962_OnNonComputableFunctions_Remastered.pdf

;;; https://en.wikipedia.org/wiki/Busy_beaver
;;; In theoretical computer science, the busy beaver game aims at finding a terminating
;;; program of a given size that produces the most output possible

;; Busy Beaver (N) is a Turing Machine with N rules that writes the most ones (but not infinitely).
;; This is an uncomputable function (because of the halting problem).  For small N, you can
;; test, but the space grows so fast that it's impractical for most cases.  For N=5, the
;; result takes more than 14 millions steps to write 4098 ones.  You need to use complicated
;; math to prove that is a maximum, you can't just test everything.

;;; Turing Machine with infinite tape of 0s.  State is a rule label. Reads current location value
;;; (0 or 1).  State rule says what to do: write 0 or 1, move left or right, new state rule.
;;; Rules labeled with letters A, B, C, etc. but I used keywords (lower).


;;; Note: winning the game is usually defined by writing the most ones, which is not always
;;; the same as most steps.


(def state-labels [:a :b :c :d :e :f :g :h :i :j :k :l :m :n])
;;; nil = halt
;;; (mapv keyword (map str "abcdefghijklmn"))

;;; direction :R right :L left
;;; pick Nth for 0 or 1


(def beaver1 {:a [[1 :R nil] nil]})

(def beaver2 {:a [[1 :R :b] [1 :L :b]]
              :b [[1 :L :b] [1 :R nil]]})
;;; my results don't exactly agree with wiki

(def beaver3 {:a [[1 :R :b] [1 :R nil]]
              :b [[0 :R :c] [1 :R :b]]
              :c [[1 :L :c] [1 :L :a]]})

(def beaver4 {:a [[1 :R :b] [1 :L :b]]
              :b [[1 :L :a] [0 :L :c]]
              :c [[1 :R nil] [1 :L :d]]
              :d [[1 :R :d] [0 :R :a]]})
;;; 107 steps, thirteen "1"s total

;;; beaver5 not shown -- takes over 14 million steps

(defn bvec [s]
  (assert (= 3 (count s)))
  (let [[zo lr st] s
        zo (if (= zo \1) 1 0)
        lr (if (= lr \R) :R :L)
        st (keyword (str/lower-case st))]
    [zo lr (when-not (= st :h) st)]))
    

(defn beaver-rule [vstr]
  (zipmap state-labels (partition 2 (map bvec vstr))))

;; order is column first, from the wiki examples
(def b3 ["1RB" "1RH" "0RC" "1RB" "1LC" "1LA"])


(defn brun [brules]
  (loop [tape {} at 0 state :a steps 0]
    (if (> steps 1000)
      {:non-halting steps :tape tape}
      (if state
        (let [x (get tape at 0)
              [w lr st2] (nth (brules state) x)
              at2 (if (= lr :R) (inc at) (dec at))]
          (do (assert (or (= lr :L) (= lr :R)))
              (assert (or (= x 0) (= x 1)))
              (recur (assoc tape at w) at2 st2 (inc steps))))
        {:steps steps :ones (count (remove zero? (vals tape))) :tape (mapv val (sort tape))}))))

