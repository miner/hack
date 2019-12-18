(ns miner.gsort
  (:require [clojure.test.check.generators :as gen]
            [clojure.alpha.spec :as s]
            [clojure.alpha.spec.gen :as g]))

;; Challenge from Eric Normand: list generator.
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-331-tool-shadow-cljs/
;;
;; "Use another implementation of sort as a model. Compare your version of sort to the
;; model.  Generate random lists and sort them. (Is this cheating? It doesn’t matter. I’m
;; putting it out there.) Now it’s your turn. Get creative! Let’s see how many wacky ways we
;; can think of."




;; SEM: prefer the spec g over check gen
;; slightly different API






;; overly active -- regens every element
(defn BADgen-one-type [& gens]
  (if (seq gens)
    (let [gens (vec gens)
          igen (fn [i] (nth gens i))]
      (gen/bind (gen/choose 0 (dec (count gens)))
                igen))
    (gen/return nil)))

;; locks on one type forever
(defn LOCKEDgen-one-type [& gens]
  (if (seq gens)
    (let [gens (vec gens)]
      (nth gens (rand-int (count gens))))
    (gen/return nil)))


(def comparable-gens1 [(g/int) (g/string-alphanumeric) (g/symbol) (g/keyword) (g/boolean)
                      (g/return nil)])


;; look for a better nilable gen???

(defn gen-mostly
  ([gen] (gen-mostly gen nil))
  ([gen const] (g/frequency [[5 gen] [1 (g/return const)]])))
  
(def comparable-gens (map gen-mostly
                          [(g/int) (g/string-alphanumeric) (g/symbol) (g/keyword) (g/boolean)]))

(def gen-sorted-list (g/fmap sort (g/one-of (map g/list comparable-gens))))


(def gen-sorted-nested-list (g/fmap sort (g/one-of (concat
                                                       (map g/list comparable-gens)
                                                       (map g/list (map g/vector
                                                                          comparable-gens))))))




;; (def BADgen-sorted-list-rec
;;  (g/fmap sort (g/one-of (map (fn [gn] (g/recursive-gen g/vector gn)) comparable-gens))))


;; SEM needs work
(defn stable-sorting? [sortfn coll]
  (let [cn (sort (map-indexed #(vector %2 %) coll))
        sn (map-indexed #(vector %2 %) (sortfn coll))]
    (= (map first cn) (map first sn))))
    


(defn ascending? [coll]
  (empty? (filter pos? (map compare coll (rest coll)))))


(defn test-sorting [sort-fn coll]
  (let [result (sort-fn coll)]
    (and (ascending? result)
         (= (count result) (count coll))
         (= (frequencies result) (frequencies coll)))))
  

(defn rasc? [coll]
  (if-let [rst (next coll)]
    (not= ::fail (reduce (fn [r x] (if (pos? (compare r x)) (reduced ::fail) x))
                         (first coll)
                         rst))
    true))

(defn rasc2? [coll]
  (or (empty? coll)
      (not= ::fail (reduce (fn [r x] (if (pos? (compare r x)) (reduced ::fail) x)) coll))))



(defn rasc3? [coll]
  (or (empty? coll)
      (not= ::fail (reduce (fn [r x] (if (pos? (compare r x)) (reduced ::fail) x))
                           (first coll)
                           (rest coll)))))


(defn asc4? [coll]
  (loop [cs coll]
    (cond (empty? (rest cs)) true
          (pos? (compare (first cs) (second cs))) false
          :else (recur (rest cs)))))


(defn smoke-sort
  ([] (smoke-sort 100))
  ([n]
   (assert (every? ascending? (g/sample gen-sorted-nested-list n)))
   true))







;; Eric had gen/long, but I couldn't find that.  I used gen/int.
(def gen-eric
  (gen/fmap
   (fn [[init nums]]
     (next (reductions + init nums)))
   (gen/tuple gen/int (gen/vector gen/nat))))

;; my version...
(def gen-sem
  (gen/let [init gen/int
            nums (gen/list gen/nat)]
    (reductions + init nums)))

