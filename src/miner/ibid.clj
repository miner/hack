(ns miner.ibid)

;; https://gist.github.com/realgenekim/607bf5b4bdb1364df8cc5f94ee589610

(def in ["a" "ibid" "ibid" "b"])

;; bad version by Gene
(defn gene-ibid 
  ([in]
   (gene-ibid (vec (first in)) (rest in)))
  ([left right]
   (println left right)
   (if (empty? right)
     ;; done
     left
     ;; else recurse
     (if (= "ibid" (first right))
       ;; repeat last left
       (gene-ibid (conj left (last left))
                  (rest right)
                  (gene-ibid (conj left (first right))
                             (rest right)))))))

;; hacked a bit by SEM, used Proctor "Ibid." notation
(defn gibid 
  ([in]
   (gibid (vector (first in)) (rest in)))
  ([left right]
   ;;(println left right)
   (if (empty? right)
     ;; done
     left
     ;; else recurse
     (if (= "Ibid." (first right))
       ;; repeat last left
       (recur (conj left (peek left))
              (rest right))
       (recur (conj left (first right))
              (rest right))))))





;; https://www.proctor-it.com/ibid/

(defn
  update_ibids
  ([authors] (update_ibids authors []))
  ([[citation_author & rest_authors :as original_authors] [last_author & _ :as new_authors]]
    (let [ibid? (fn [author] (= "Ibid." author))]
      (cond
        (empty? original_authors) (reverse new_authors)
        (and (ibid? citation_author) (not last_author))
          (throw (Exception. "Found `Ibid.` with no previous author"))
        :else (recur
          rest_authors
          (cons
            (if (ibid? citation_author)
                last_author
                citation_author)
            new_authors))))))



(def references ["Gene Kim", "Jez Humble", "Ibid.", "Gene Kim", "Ibid.", "Ibid.", "Nicole Forsgren", "Micheal Nygard", "Ibid."])
 
#_ (update_ibids [])
;;=> ()

#_ (update_ibids ["Ibid."])
;; Execution error at user/update-ibids (REPL:8).
;; Found `Ibid.` with no previous author

#_ (update_ibids references)
;;=> ("Gene Kim" "Jez Humble" "Jez Humble" "Gene Kim" "Gene Kim" "Gene Kim"
;;    "Nicole Forsgren" "Micheal Nygard" "Micheal Nygard")



;; ----------------------------------------------------------------------

(defn smoke-ibid
  ([] (smoke-ibid update_ibids))
  ([ibid]
   (assert (= (ibid ["Gene Kim", "Jez Humble", "Ibid.", "Gene Kim", "Ibid.", "Ibid.",
                     "Nicole Forsgren", "Micheal Nygard", "Ibid."])
              ["Gene Kim" "Jez Humble" "Jez Humble" "Gene Kim" "Gene Kim" "Gene Kim"
               "Nicole Forsgren" "Micheal Nygard" "Micheal Nygard"]))
   true))
                           

(defn ibids [authors]
  (reduce (fn [res auth]
            (if (= auth "Ibid.")
              (if-let [prev (peek res)]
                (conj res prev)
                (throw (ex-info "Found `Ibid.` with no previous author"
                                {:original authors :partial-result res})))
              (conj res auth)))
          []
          authors))
                  




(defn ibidc [authors]
  (reduce (fn [res auth]
            (cond (not= auth "Ibid.") (conj res auth)
                  (not (peek res)) (throw (ex-info "Found `Ibid.` with no previous author"
                                                   {:original authors :partial-result res}))
                   :else (conj res (peek res))))
          []
          authors))
