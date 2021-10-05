(ns miner.badsort)

;;; https://news.ycombinator.com/item?id=28758106

;;; counterintuitive sorting algorithm that seems wrong but actually works.
;;; It looks to me like it's an anti-bubble sort in that it pulls larger things forwards
;;; until the tail is sorted.


;;; [HN comment]
;;; tl;dr the algorithm (the array is 1-based):
;;   for i = 1 to n do
;;     for j = 1 to n do
;;       if A[i] < A[j] then
;;         swap A[i] and A[j]



(defn badsort [v]
  (reduce (fn [r i]
            (reduce (fn [r j]
                      (let [ri (r i)
                            rj (r j)]
                        (if (< ri rj)
                          (do #_ (println i j r)
                              (assoc r i rj j ri))
                        r)))
                    r
                    (range (count v))))
            v
            (range (count v))))

