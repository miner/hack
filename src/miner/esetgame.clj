(ns miner.esetgame)

;; https://gist.github.com/ericnormand/9675d9ebea9f5e3b66474fdef9e3d1b6

;; Eric's Challenge:  Set Game
;;
;; Each card has four properties:
;;
;; Color (red, purple, green)
;; Number (1, 2, 3)
;; Shading (empty, lined, full)
;; Shape (squiggle, oval, diamond)
;;
;; Three cards form a set if all of the properties are total matches or total mismatches.

(def properties #{:color :number :shading :shape})

(def colors #{:red :purple :green})

(def numbers #{1 2 3})

(def shadings #{:empty :lines :full})

(def shapes #{:squiggle :oval :diamond})


(defn property-set? [coll]
  (or (apply = coll)
      (apply distinct? coll)))


(defn legal-card? [card]
  (and (colors (:color card))
       (numbers (:number card))
       (shadings (:shading card))
       (shapes (:shape card))))


(defn card-set? [cards]
  ;;{:pre [(every? legal-card? cards)]}
  (every? (fn [prop] (property-set? (map prop cards))) properties))


;; assuming exactly 3 cards, you can count the set of each property
;; 1 is unique, 3 is all different, only 2 fails
(defn card-set3? [cards]
  ;;{:pre [(= (count cards) 3) (every? legal-card? cards)]}
  (every? (fn [prop] (odd? (count (into #{} (map prop) cards)))) properties))

;; faster, based on Steffan Westcott solution
(defn card-set4? [cards]
  ;;{:pre [(= (count cards) 3) (every? legal-card? cards)]}
  (every? (fn [prop] (odd? (count (reduce (fn [res c] (conj! res (prop c)))
                                          (transient #{})
                                          cards))))
          properties))


(defn pcount [prop cards]
  (count (reduce (fn [res c] (conj! res (prop c)))
                 (transient #{})
                 cards)))

;; fastest
(defn card-set41? [cards]
  ;;{:pre [(= (count cards) 3) (every? legal-card? cards)]}
  (every? (fn [prop] (odd? (pcount prop cards)))
          properties))







(defn card-set44? [cards]
  ;;{:pre [(= (count cards) 3) (every? legal-card? cards)]}
  (let [pcount (fn [prop]
                 (count (reduce (fn [res c] (conj! res (prop c)))
                                (transient #{})
                                cards)))]
    (every? (fn [prop] (odd? (pcount prop))) properties)))


(defn card-set48? [cards]
  ;;{:pre [(= (count cards) 3) (every? legal-card? cards)]}
  (let [pcount (fn [prop]
                 (count (reduce (fn [res c] (conj! res (prop c)))
                                (transient #{})
                                cards)))]
    (empty? (into () (comp (map pcount) (remove odd?)) properties))))





;; not quite as fast, but we have small counts of cards so not best case for transducers
(defn xcount [prop cards]
  (transduce (map prop)
             (completing conj! count)
             (transient #{})
             cards))

(defn card-set42? [cards]
  ;;{:pre [(= (count cards) 3) (every? legal-card? cards)]}
  (every? (fn [prop] (odd? (xcount prop cards)))
          properties))



;; not fast, but not bad
(defn card-set47? [cards]
  ;;{:pre [(= (count cards) 3) (every? legal-card? cards)]}
  (let [xcount (fn [prop]
                 (transduce (map prop)
                            (completing conj! count)
                            (transient #{})
                            cards))]
  (empty? (sequence (comp (map xcount) (remove odd?)) properties))))




(defn set3? [p a b c]
  (if (= (p a) (p b))
    (= (p a) (p c))
    (and (not= (p a) (p c))
         (not= (p b) (p c)))))

(defn card-set49? [cards]
  (every? (fn [prop] (apply set3? prop cards))
          properties))

;; not faster to let all

(defn pset3? [p a b c]
  (let [pa (p a)
        pb (p b)]
    (if (= pa pb)
      (= pa (p c))
      (let [pc (p c)]
        (and (not= pa pc)
             (not= pb pc))))))

(defn card-set50? [cards]
  (every? (fn [prop] (apply pset3? prop cards))
          properties))






;; Steffan Westcott
(defn west-set? [cards]
  (every? #(-> (map % cards) set count odd?) [:color :number :shading :shape]))



;;; works but slow and obscure!  And brittle (assume 3 cards again)
(defn card-set5? [cards]
  (every? odd? (vals (frequencies (mapcat vals cards)))))

(defn card-set6? [cards]
  (->> cards
       (mapcat vals)
       frequencies
       vals
       (every? odd?)))

(def pvals (apply juxt properties))

(defn card-set61? [cards]
    (->> cards
         (mapcat pvals)
         frequencies
         vals
         (every? odd?)))




(defn card-set7? [cards]
  (every? (fn [prop] (odd? (count (into #{} (map prop) cards))))
          properties))


;; UNKNOWN Mac 2E-86-D1-CC-3E-64



;; maybe faster?
(defn pset?
  ([a] true)
  ([a b] true)
  ([a b c] (or (= a b c) (distinct? a b c)))
  ([a b c & more] (let [coll (list* b c more)]
                    (or (apply = coll)
                        (apply distinct? coll)))))

(defn card-set2? [cards]
  (every? (fn [k] (apply pset? (map k cards))) properties))


(def goods [{:color :purple :number 3 :shape :diamond :shading :full}
                       {:color :red    :number 3 :shape :diamond :shading :lines}
                       {:color :green  :number 3 :shape :diamond :shading :empty}])

(defn smoke-cards
  ([] (smoke-cards card-set?))
  ([card-set?] 
   (assert (card-set? [{:color :purple :number 3 :shape :diamond :shading :full}
                       {:color :red    :number 3 :shape :diamond :shading :lines}
                       {:color :green  :number 3 :shape :diamond :shading :empty}]))

   (assert (not (card-set? [{:color :purple :number 3 :shape :diamond :shading :full}
                            {:color :red    :number 3 :shape :diamond :shading :lines}
                            {:color :purple :number 3 :shape :diamond :shading :empty}])))
   true))

