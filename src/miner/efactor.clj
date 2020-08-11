(ns miner.efactor)

;; Eric Challenge: stringify prime factorization given factors

;; https://gist.github.com/ericnormand/087eab23272b3ed0d7a8e3007b699a1d


;; Works with unordered factors
;; faster with transducers
(defn factors->string [factors]
  (apply str (reduce * 1 factors) " = "
         (sequence (comp (map (fn [[f cnt]] (if (= cnt 1) f (str f "^" cnt))))
                         (interpose " x "))
                   (sort (frequencies factors)))))





(defn zfact [factors]
  (apply str (reduce * 1 factors) " = "
         (into [] (comp (map (fn [[f cnt]]
                                  (if (> cnt 1)
                                    (str f "^" cnt)
                                    f)))
                         (interpose " x "))
                   (sort (frequencies factors)))))



;; sequence version
(defn strfact [factors]
  (let [fcnts (frequencies factors)]
    (apply str (reduce * 1 factors) " = "
           (interpose " x " (map (fn [[f cnt]]
                                   (if (> cnt 1)
                                     (str f "^" cnt)
                                     f))
                                 (sort fcnts))))))



;; assuming factors is vector in order, but not faster
(defn gfact [factors]
  (let [gs (reduce (fn [res x] (let [g (peek res)]
                                 (if (and g (= (peek g) x))
                                   (conj (pop res) (conj g x))
                                   (conj res [x]))))
                   []
                   factors)]
    (apply str (reduce * 1 factors) " = "
           (interpose " x " (map (fn [vf] (let [cnt (count vf)]
                                            (if (> cnt 1)
                                             (str (peek vf) "^" cnt)
                                             (peek vf))))
                                 gs)))))



(defn pbfact [factors]
  (let [gs (into [] (partition-by identity) factors)]
    (apply str (reduce * 1 factors) " = "
           (interpose " x " (map (fn [fs] (let [cnt (count fs)]
                                            (if (> cnt 1)
                                             (str (peek fs) "^" cnt)
                                             (peek fs))))
                                 gs)))))







(defn smoke-fact [factors->string]
  (assert (= (factors->string [2 2 2 3]) "24 = 2^3 x 3"))
  (assert (= (factors->string [7]) "7 = 7"))
  (assert (= (factors->string [2 2 7]) "28 = 2^2 x 7"))
  true)

