(ns miner.parse
  (:require [instaparse.core :as p]))

(def as-and-bs
  (insta/parser
    "S = AB*
     AB = A B
     A = 'a'+
     B = 'b'+"))



