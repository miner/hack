(ns miner.rosettaloop)

;; Show a nested loop which searches a two-dimensional array filled with random numbers
;; uniformly distributed over [1 .. 20]. The loops iterate rows and columns of the array
;; printing the elements until the value 20 is met. Specifically, this task also shows how
;; to break out of nested loops.




;; https://rosettacode.org/wiki/Loops/Nested#Clojure
 
(defn create-matrix [width height]
  (for [_ (range width)]
    (for [_ (range height)]
      (inc (rand-int 20)))))

;; i/o hacked a bit by SEM but I don't like the logic
(defn print-matrix [matrix]
  (loop [[row & rs] matrix]
    (when (= (loop [[x & xs] row]
               (print x "")
               (cond (= x 20) :stop
                     xs (recur xs)
                     :else :continue))
             :continue)
      (when rs (recur rs)))))


(defn lpm [matrix]
  (loop [rows matrix continue true]
    (when-let [row (when continue (first rows))]
      (recur (rest rows) (loop [xs row cont continue]
                           (if (empty? xs)
                             cont
                             (when cont
                               (print (first xs) "")
                               (recur (rest xs) (not= (first xs) 20)))))))))



#_
(print-matrix (create-matrix 10 10))


(def sample '((8 2 16 6 1 11 19 19 8 1)
              (19 15 4 5 19 11 3 7 13 18)
              (8 16 13 11 13 20 4 5 2 1)
              (2 8 2 8 4 12 7 11 10 3)
              (13 9 15 12 4 15 1 14 11 19)
              (18 5 14 13 10 16 17 16 10 16)
              (13 17 4 9 15 4 4 11 14 16)
              (20 2 5 14 15 7 17 16 10 16)
              (9 4 20 13 12 12 15 20 9 18)
              (2 16 10 15 2 9 4 16 20 19)))


(defn take-until
  "Returns a lazy sequence of successive items from coll while
  (pred item) returns true. pred must be free of side-effects.
  Returns a transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([pred]
     (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
            (if (pred input)
              (ensure-reduced (rf result input) )
              (rf result input))))))
  ([pred coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (if (pred (first s))
          (list (first s))
          ;; or maybe (cons (first s) nil) -- everything else is cons
          (cons (first s) (take-until pred (rest s))))))))

(defn pm [matrix]
  (doseq [x (sequence (comp cat (take-until #(= 20 %))) matrix)]
    (print "" x))
  (println))


(defn rpm1 [matrix]
  (reduce (fn [res row]
            (reduce (fn [r x]
                      (print x "")
                      (when (= x 20)
                        (reduced (reduced true))))
                    res
                    row))
            nil
            matrix))


            

(defn flat-until [pred matrix]
  (reduce (fn [res row]
            (reduce (fn [r x]
                      (if (pred x)
                        (reduced (reduced (conj r x)))
                        (conj r x)))
                    res
                    row))
          []
          matrix))

(defn pm3 [matrix]
  (run! #(print % "") (flat-until #(= 20 %) matrix)))


(defn fpm1 [matrix]
  (take-until #(= 20 %) (for [row matrix
                              x row]
                          x)))



;;; all of these are slower than create-matrix
(defn matrix-2d [n]
  (partition n (repeatedly (* n n) #(inc (rand-int 20)))))

(defn matrix-2dt [n]
  (into [] (partition-all n) (repeatedly (* n n) #(inc (rand-int 20)))))

(defn matrix-2dr [n]
  (repeatedly n (fn [] (repeatedly n #(inc (rand-int 20))))))
