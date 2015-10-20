
;; http://stackoverflow.com/questions/3517083/will-the-clojure-compiler-automatically-evaluate-expressions-of-literals-at-comp


(defmacro preprocess [f & args]
  (let [x# (apply (resolve f) args)]
    `~x#))

(def a (preprocess some-time-consuming-function some-literal))

(def b (preprocess some-other-time-consuming-function a))
