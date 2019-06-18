(ns miner.gsort
  (:require [clojure.test.check.generators :as gen]
            [clojure.spec-alpha2 :as s]
            [clojure.spec-alpha2.gen :as g]))

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

(def comparable-gens (map (fn [gn] (g/frequency [[5 gn] [1 (g/return nil)]]))
                          [(g/int) (g/string-alphanumeric) (g/symbol) (g/keyword) (g/boolean)]))

(def gen-sorted-list (g/fmap sort (g/one-of (map g/list comparable-gens))))


(def gen-sorted-nested-list (g/fmap sort (g/one-of (concat
                                                       (map g/list comparable-gens)
                                                       (map g/list (map g/vector
                                                                          comparable-gens))))))




;; (def BADgen-sorted-list-rec
;;  (g/fmap sort (g/one-of (map (fn [gn] (g/recursive-gen g/vector gn)) comparable-gens))))
