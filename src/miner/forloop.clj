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

