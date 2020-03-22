(ns miner.depth)

;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-369-refactoring-replace-body-with-callback/

;; Challenge: Your task is to write a function that finds the maximum depth of any given value.
;; Don't forget the collections include lists, vectors, maps, and sets.


;; maps need special handling as sequence of map-entries (extra level), but (seq {}) is nil.

(defn depth
  ([val] (depth val 0))
  ([val d]
   (if (coll? val)
     (let [d2 (if (map? val) d (inc d))]
       (reduce (fn [r x] (max r (depth x d2))) (inc d) val))
     d)))

;; My theory is that the JVM can compile this efficiently and doesn't have to pay full
;; recursion costs.




;; Eric's solution.  Slower than mine.
;; https://gist.github.com/ericnormand/a95c071dab885d82f193e747725ef22a
(defn edepth
  [v]
  (cond
   (map? v)
       (recur (concat (keys v) (vals v)))
   (coll? v)
       (inc (reduce max 0 (map edepth v)))
   :else
       0))


(defn smoke-depth
  ([] (smoke-depth depth))
  ([fdepth]
   (assert (= (fdepth nil) 0))
   (assert (= (fdepth 0) 0))
   (assert (= (fdepth ()) 1))
   (assert (= (fdepth {}) 1))
   (assert (= (fdepth {:a {}}) 2))
   (assert (= (fdepth {:a 1 :b 2}) 1))
   (assert (= (fdepth []) 1))
   (assert (= (fdepth [[]]) 2))
   (assert (= (fdepth [[nil]]) 2))
   (assert (= (fdepth [[0] [2] [1 [2]]])  3))
   (assert (= (fdepth (list [[#{[0]}]])) 5))
   (assert (= (fdepth {:a {:aa 11 :bb {:ccc [333]}}}) 4))
   (assert (= (fdepth [{:a [{:aa 11 :bb {:ccc [333]}}]}]) 6))
   (let [nested (take 100 (iterate list 0))]
     (assert (= (fdepth nested) 100))
     (assert (= (fdepth (list :a nested :b {:a [nested :x]} :c nested)) 103)))

   ;; borrowed tests from gist
   (assert (= 0 (fdepth 0)))

   (assert (= 1 (fdepth [])))
   (assert (= 2 (fdepth [ [] ])))
   (assert (= 3 (fdepth [[0] [2] [1 [2]]])))

   (assert (= 1 (fdepth (list))))
   (assert (= 2 (fdepth (list (list)) )))
   (assert (= 3 (fdepth (list (list 0) (list 2) (list 1 (list 2))))))

   (assert (= 1 (fdepth #{})))
   (assert (= 2 (fdepth #{ #{} })))
   (assert (= 3 (fdepth #{#{0} #{2} #{1 #{2}}})))

   (assert (= 1 (fdepth {})))
   (assert (= 2 (fdepth {:a {}})))
   (assert (= 4 (fdepth {:a {:a 0
                            :b {:a 2}
                            :c {:a 1
                                :b {:a 2}}}})))
   
   (assert (= 5 (fdepth #{[0]
                         {:a [1
                              (list 2
                                    3
                                    [4])]}})))

   (assert (= 0 (fdepth 0)))
   (assert (= 0 (fdepth nil)))
   (assert (= 0 (fdepth "string")))
   
   (assert (= 1 (fdepth '(1 2))))
   (assert (= 1 (fdepth [])))
   (assert (= 1 (fdepth {:a :b})))
   (assert (= 1 (fdepth #{\a \b})))
   (assert (= 1 (fdepth (seq "string"))))
   
   (assert (= 2 (fdepth [1 [2]])))
   (assert (= 2 (fdepth {:a [1]})))
   (assert (= 2 (fdepth {[:a] 1})))
   (assert (= 2 (fdepth {[:a] [1]})))
   (assert (= 2 (fdepth {{} [1]})))
   (assert (= 3 (fdepth {{[] 1} [1]})))
   (assert (= 3 (fdepth [[0] [2] [1 [2]]])))
   
   true))





;; (seq amap) introduce extra level of depth with map-entries so we `(dec d)` to compensate.
;; Be careful about empty map!  (seq {}) ==> nil so it needs special case, too.

(defn depth-WORKS
  ([val] (depth-WORKS val 0))
  ([val d]
   (cond (map? val) (if-let [ents (seq val)] (recur ents (dec d)) (inc d))
         (coll? val) (reduce (fn [r x] (max r (depth-WORKS x (inc d)))) (inc d) val)
         :else d)))


(defn depth-OK
  ([val] (depth-OK val 0))
  ([val d]
   (if (coll? val)
     (cond (empty? val) (inc d)
           (map? val) (recur (seq val) (dec d))
           :else (reduce (fn [r x] (max r (depth-OK x (inc d)))) (inc d) val))
     d)))



(defn dep4
  ([val] (dep4 val 0))
  ([val d]
   (cond (not (coll? val)) d
         (empty? val) (inc d)
         (map? val) (max (dep4 (keys val) d) (dep4 (vals val) d))
         :else (reduce (fn [r x] (max r (dep4 x (inc d)))) (inc d) val))))



(defn dep5
  ([val] (dep5 val 0))
  ([val d]
   (cond (map? val)  (reduce (fn [r x] (max r (dep5 x d))) (inc d) val)
         (coll? val) (reduce (fn [r x] (max r (dep5 x (inc d)))) (inc d) val)
         :else d)))



;; BUG -- maps will get an ext level

;; arity version
(defn depth-BUG
  ([val] (depth-BUG val 0))
  ([val d]
   (if (coll? val) 
     (reduce (fn [r x] (max r (depth-BUG x (inc d)))) (inc d) val)
     d)))



;; relative slow, buggy for maps
(defn ldep-BUG
  ([val] (ldep-BUG 0 (list 0 val)))
  ([deepest dvv]
   (if (empty? dvv)
     deepest
     (let [d (peek dvv)
           v (peek (pop dvv))
           more (pop (pop dvv))]
       (if (coll? v)
         (if (empty? v)
           (recur (max deepest (inc d)) more)
           (recur deepest (into more (interleave v (repeat (inc d))))))
         (recur (max deepest d) more))))))



;;  but BUGGY on maps
(defn rdep
  ([val] (rdep val 0))
  ([coll d]
   (cond (not (coll? coll)) d
         (empty? coll) (inc d)
         :else  (apply max (map #(rdep % (inc d)) coll)))))




;;; ----------------------------------------------------------------------


;; Maybe related -- sum of depths
;; https://stackoverflow.com/questions/59515105/how-to-calculate-the-sum-of-all-depths-in-a-tree-using-basic-recur

;; a leaf node is anything that's not a vector. And a node with children is a vector of form:
;; [value [child1 child2 ...]

;; sum of depths including intermediate nodes (see smoke-sum for examples)

;; accepted answer
(defn sum-depths
  ([tree]
   (sum-depths tree 0))
  ([node depth]
   (if-not (vector? node)
     depth
     (do
       (apply
        +
        depth
        (for [child-node (second node)]
          (sum-depths child-node (inc depth))))))))


;; SEM slightly hacked
;; much faster, will stack overflow on very deep tree
(defn sum-depths2
  ([tree]   (sum-depths2 tree 0))
  ([node depth]
   (if (vector? node)
     (reduce (fn [r child-node] (+ r (sum-depths2 child-node (inc depth))))
             depth
             (peek node))
     depth)))



;; not so fast, but can handle very deep tree without stack overflow
;; Transducer with into is a bit faster than old-fashioned mapcat, and maybe count on vector
;; is a win.  (Again, slightly hacked version of Stack Overflow answer)
(defn sum-depths3 [tree]
  (loop [depth 0 nodes [tree] total 0]
    (if (empty? nodes)
      total
      (recur (inc depth)
             (into [] (comp (filter vector?) (mapcat peek)) nodes)
             (+ total (* (count nodes) depth))))))





;; DEEP tree -- will cause stack overflow on recursive versions
(def tree
  (loop [i 0 t [:a [:b]]]
    (if (< i 10000)
      (recur (inc i)
             [:a [t]])
      t)))


(defn smoke-sum [fsum-depths]
  (assert (= (fsum-depths [:root [:a1 [:b1 [:a2 :b2]] :c1]])
             7))

  (assert (= (fsum-depths ["COM" [["B" [["C" [["D" [["E" ["F" ["J" [["K" ["L"]]]]]] "I"]]]]
                                        ["G" ["H"]]]]]])
             42))


  true)
