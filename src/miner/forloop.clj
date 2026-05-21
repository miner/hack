(ns miner.forloop)

;;; Experimental.  Not ready for prime-time.

;;; brave search came up with this macro

(defmacro for-loop [[sym init check change :as params] & steps]
  (cond (not (vector? params)) 
            (throw (Error. "Binding form must be a vector for for-loop"))
        (not= 4 (count params)) 
            (throw (Error. "Binding form must have exactly 4 arguments in for-loop"))
        :default `(loop [~sym ~init value# nil]
                    (if ~check
                      (let [new-value# (do ~@steps)]
                        (recur ~change new-value#))
                      value#))))


;;; SEM: but IMHO, that's a trivial example.  You really need a way to handle mutations in
;;; the body.  Other people want `break` and `continue`.

;;; reddit discussion
;;; https://www.reddit.com/r/Clojure/comments/whzn2u/how_to_convert_code_from_imperative_to_functional/

;;; video of talk "Solving Problems the Clojure Way" by Rafal Dittwald
;;; https://www.youtube.com/watch?v=vK1DazRK_a0

;;; I'm thinking about my approach to porting Knuth's baxter? test from C.  It was
;;; complicated but in theory it could be turned into a macro.
;;; See also my baxter.clj



(defn demo [n]
  (for-loop [i 10 (< i n) (inc (inc i))]
            (println " i =" i)
            i))


(defn demo1 [n]
  (doseq [i (range 10 n 2)]
    (println " i =" i))
  ;; do we really need the final result?
  (last (range 10 n 2)))
    

;;; I guess I want a way to capture all the mutable locals into loop args and have a
;;; pseudo-set notation that would update them.

;;; don't forget about `binding` and `set!`

;;; http://gettingclojure.wikidot.com/cookbook:functional-programming

;;; also `with-local-vars`
;;; https://clojuredocs.org/clojure.core/with-local-vars

;;; kind of ugly, but it might make porting imperative style for loops easier

;;; Kyle Kingsbury "Aphyr" wrote a `loopr` macro that's very fancy:
;;; https://aphyr.com/posts/360-loopr-a-loop-reduction-macro-for-clojure
