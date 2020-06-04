(ns miner.econtraindications)

;;; https://gist.github.com/ericnormand/04b22244e6ab9502326a0516eb3bdfa8

;;; Your task is to take the list of medications a patient has and a list of
;;; contraindication pairs, and determine what pairs of medications (if any) they are
;;; prescribed that don't mix well.  It should return nil if there are no contraindications
;;; and a collection of the contraindications (pairs) if there are any.  Bonus: Make it a
;;; linear algorithm (linear in the number of the patient medications and the number of the
;;; contradiction pairs).

  

(def patient-medications [{:name "Blythenal"
                           :rxNorm "blyth"}
                          {:name "Masbutol"
                           :rxNorm "masbut"}
                          {:name "Welbutril"
                           :rxNorm "welb"}])

(def contraindication-pairs [["nr913ng" "blyth"]
                             ["masbut"  "87f2h139049f"]
                             ["nr913ng" "j1j81f0"]
                             ["blyth" "welb"]
                             ["masbut"  "welb"]])

(defn contraindications [meds pairs]
  (let [medset (into #{} (map :rxNorm) meds)]
    (into nil (filter #(every? medset %)) pairs)))

;; we want nil for negative result not empty sequence



(defn set= [a b] (= (set a) (set b)))

(defn smoke-contra [contra]
  (assert (set= (contra patient-medications contraindication-pairs)
             [["masbut" "welb"] ["blyth" "welb"]]))
  (assert (nil? (contra [{:name "Blythenal" :rxNorm "blyth"}] contraindication-pairs)))
  true)





(defn red-contraindications [meds pairs]
  (let [medset (set (map :rxNorm meds))]
    (reduce (fn [res pair]
              (if (every? medset pair)
                (conj res pair)
                res))
            nil
            pairs)))


(defn mchampine [meds pairs]
  (seq (filter #(every? (set (map :rxNorm meds)) %) pairs)))

(defn mchampine2 [meds pairs]
  (let [mset (set (map :rxNorm meds))]
    (seq (filter #(every? mset %) pairs))))
