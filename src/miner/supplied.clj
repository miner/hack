(ns miner.supplied
  (:use clojure.test))

;; From the mailing list and CLJ-1508


(deftest supplied-p-in-destructuring
  ;; (let [{:keys [a b c d] :p? {a a-p? b b-p? c c-p? d d-p?} :or {a 1}} {:b 2 :c 3 }]
  (let [{:keys [a b c d] :or {a 1} :as argmap} {:b 2 :c 3 }
        supplied? (partial contains? argmap)
        a-p? (supplied? :a)
        b-p? (supplied? :b)
        c-p? (supplied? :c)
        d-p? (supplied? :d)]
    (is (= a 1))
    (is (false? a-p?))
    (is (= 2 b))
    (is (true? b-p?))
    (is (= 3 c))
    (is (true? c-p?))
    (is (nil? d))
    (is (false? d-p?))))
