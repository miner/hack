(ns miner.mlp)

;;; 07/12/23  14:32 by miner -- draft
;;; https://www.thedinkpickleball.com/mlp-season-2-draft-results/
;;; you need team and pick to account for trades
;;; you can't just assume the original snake draft sequence

(def draft
  [:chi :Ben
   :uta :ALW
   :dc :Riley
   :bro :Catherine
   :orl :AnnaBright
   :col :JW
   :az :Andiamo
   :tex :Dylan
   :dal :Jiggy
   :atl :Parris
   :ba :Lea
   :mia :Fed
   :mia :Tyson
   :ba :Etta
   :atl :Simone
   :dal :Callie
   :tex :Jorja
   :az :VivienneDavid
   :col :Meg
   :orl :Andrei
   :bro :Koop
   :dc :JackieK
   :uta :Irina
   :chi :Jessie
   :chi :Lacy
   :uta :Wilson
   :atl :Tellez
   :bro :Hayden
   :dc :JadeK
   :col :MaggieB
   :az :Dekel
   :tex :Stratman
   :dal :Allyce
   :orl :Zane
   :ba :Rafa
   :mia :Tyra
   :mia :MaryB
   :ba :Connor
   :orl :RachelRohrabacher
   :dal :Jay
   :tex :Travis
   :az :VivianGlozman
   :col :Collin
   :dc :Alshon
   :bro :Tyler
   :atl :HunterJohnson
   :uta :AJ
   :chi :ErikLange
   ])

(defn teams [draft]
  (transduce (comp (partition-all 2) (map-indexed (fn [i [t p]] [[(inc i) p] t])))
             (fn ([r [ip t]] (update r t conj ip))
               ([r] (update-vals r #(into (sorted-map) %))))
             {}
             draft))

(def mlp (teams draft))

(defn print-mlp
  ([mlp] (clojure.pprint/pprint (sort-by (comp first val) mlp)))
  ([] (print-mlp mlp)))

#_
([:chi {1 :Ben, 24 :Jessie, 25 :Lacy, 48 :ErikLange}]
 [:uta {2 :ALW, 23 :Irina, 26 :Wilson, 47 :AJ}]
 [:dc {3 :Riley, 22 :JackieK, 29 :JadeK, 44 :Alshon}]
 [:bro {4 :Catherine, 21 :Koop, 28 :Hayden, 45 :Tyler}]
 [:orl {5 :AnnaBright, 20 :Andrei, 34 :Zane, 39 :RachelRohrabacher}]
 [:col {6 :JW, 19 :Meg, 30 :MaggieB, 43 :Collin}]
 [:az {7 :Andiamo, 18 :VivienneDavid, 31 :Dekel, 42 :VivianGlozman}]
 [:tex {8 :Dylan, 17 :Jorja, 32 :Stratman, 41 :Travis}]
 [:dal {9 :Jiggy, 16 :Callie, 33 :Allyce, 40 :Jay}]
 [:atl {10 :Parris, 15 :Simone, 27 :Tellez, 46 :HunterJohnson}]
 [:ba {11 :Lea, 14 :Etta, 35 :Rafa, 38 :Connor}]
 [:mia {12 :Fed, 13 :Tyson, 36 :Tyra, 37 :MaryB}])



;; previous tries
(defn teams1 [draft]
  (group-by val (into {} (comp (partition-all 2) (map reverse) (map vec)) draft)))

(defn teams2 [draft]
  (transduce (comp (partition-all 2) (map reverse) (map vec))
             (fn ([r [p t]] (update r t conj p))
               ([r] (update-vals r reverse)))
             {}
             draft))

