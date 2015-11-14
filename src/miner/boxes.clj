;; http://blog.clojuregirl.com/clojure_recursion.html

(defn boxes-problem [volume {:keys [a] :as box}]  
  (let [volume-of-a-box (* a a a)] ;; Volume of the box
    (if-not (contains? box :inner-box) ;; Is another box inside?
      (+ volume volume-of-a-box) 
      (recur (+ volume volume-of-a-box) (:inner-box box)))))

(comment
  (boxes-problem 0 {:a 5 :inner-box {:a 4 :inner-box
                                     {:a 3 :inner-box {:a 2 :inner-box {:a 1}}}}})
  )

;;=> 225



;; My solution

(defn cubed [x]
  (* x x x))

(defn bp
  ([box] (bp 0 box))
  ([volume box]
   (if box
     (recur (+ volume (cubed (:a box))) (:inner-box box))
     volume)))
