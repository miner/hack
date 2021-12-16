(ns eric.backspace
  (:require [clojure.string :as str]))


;; https://gist.github.com/ericnormand/576b85aadf1d003c09919841ce6cb2fd

(defn apply-bs1 [s]
  (str/join (reduce (fn [stack c]
                      (if (= c \#)
                        (if (empty? stack) stack (pop stack))
                        (conj stack c)))
                    []
                    s)))

(defn smoke-bs [apply-bs]
  (assert (= (apply-bs "abc#")  "ab"))
  (assert (= (apply-bs "abc###")  ""))
  (assert (= (apply-bs "###abc") "abc"))
  (assert (= (apply-bs "there###eir") "their"))
  true)

;; fastest  -- mutation and interop
(defn apply-bs [^String s]
  (loop [sb (StringBuilder. s) octo (.indexOf s "#")]
    (cond (pos? octo) (let [sb (.delete sb (dec octo) (inc octo))]
                        (recur sb (.indexOf sb "#" (dec octo))))
          (neg? octo) (.toString sb)
          :else  (let [sb (.deleteCharAt sb 0)]
                   (recur sb (.indexOf sb "#"))))))




