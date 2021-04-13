(ns miner.eknight)

;; https://gist.github.com/ericnormand/f217a8c3dc07fe0def69532877182365
;;
;; chess board is defined as rows 1-8 and columns :A - :H.  [:A 1] is lower left
;; corner.  I guess that should be "rank" and "file" in chess terms.



(def kfiles [:A :B :C :D :E :F :G :H])

(def ifile (reduce-kv (fn [m i kf] (assoc m kf i)) {} kfiles))

(defn knight-move [[f r]]
  (let [i (ifile f)]
    (into [] (keep (fn [[fdiff rdiff]]
                     (let [r2 (+ r rdiff)]
                       (when (<= 1 r2 8)
                         (when-let [f2 (nth kfiles (+ i fdiff) nil)]
                           [f2 r2])))))
          [[-2 -1] [-1 -2] [-2 1] [-1 2] [1 2] [2 1] [1 -2] [2 -1]])))

;; note: order of offsets corresponds to given examples


;; not faster, doesn't care about order of results
(defn fkm [[f r]]
  (let [i (ifile f)]
    (for [rdiff [-2 -1 1 2]
          :let [r2 (+ r rdiff)]
          :when (<= 1 r2 8)
          fdiff [-2 -1 1 2]
          :when (odd? (+ fdiff rdiff))
          :let [f2 (nth kfiles (+ i fdiff) nil)]
          :when f2]
      [f2 r2])))



(defmacro assert?
  ([pred form result]
   `(do (assert (~pred ~form ~result)) true))
  ([pred form result & more]
   `(and (assert? ~pred ~form ~result)
         (assert? ~pred ~@more))))

(defn sort= [a b]
  (= (sort a) (sort b)))

;; use sort= if you don't care about order

(defn smoke-knight [knight-move]
  (assert? sort=
           (knight-move [:D 3]) [[:B 2] [:C 1] [:B 4] [:C 5] [:E 5] [:F 4] [:E 1] [:F 2]]
           (knight-move [:A 1]) [[:B 3] [:C 2]]))





;; @sw
(def sw-files [:A :B :C :D :E :F :G :H])

;; SEM added type hint, still I don't like using .indexOf here
(defn sw-knight-move [[from-file from-rank]]
  (let [from-x (.indexOf ^java.util.List sw-files from-file)]
    (for [dx [-2 -1 1 2]
          :let [to-file (get sw-files (+ from-x dx))]
          :when to-file
          dy (if (odd? dx) [-2 2] [-1 1])
          :let [to-rank (+ from-rank dy)]
          :when (<= 1 to-rank 8)]
      [to-file to-rank])))




(comment

  ;;; OOPS -- I got rank and file mixed up.  Rank 1-8 (rows), File :A - :H (cols) is
  ;;; correct.  Most of the stuff below has it backwards.  The logic still works.  My
  ;;; solution was corrected before posting.

;;;;; Other junk beyond here...
(def ranks [:A :B :C :D :E :F :G :H])
(def files [1 2 3 4 5 6 7 8])

(def one-up (zipmap ranks (conj (subvec ranks 1) nil)))
(def two-up (zipmap ranks (conj (subvec ranks 2) nil nil)))
                     
(def one-down (zipmap ranks (cons nil ranks)))
(def two-down (zipmap ranks (concat [nil nil] ranks)))
                     
(def one-left (zipmap files (cons nil files)))
(def two-left (zipmap files (concat [nil nil] files)))

(def one-right (zipmap files (conj (subvec files 1) nil)))
(def two-right (zipmap files (conj (subvec files 2) nil nil)))

;; order given by example
(def hops [[two-down one-left] [one-down two-left]
           [two-down one-right] [one-down two-right]
           [one-up two-right] [two-up one-right]
           [one-up two-left] [two-up one-left]])

;; my preferred order
(def hops-ORIG (concat (for [two [two-up two-down]
                        one [one-left one-right]]
                    [two one])
                   (for [one [one-up one-down]
                         two [two-left two-right]]
                     [one two])))

(defn move [[r f] [mr mf]]
  (when-let [r2 (mr r)]
    (when-let [f2 (mf f)]
      [r2 f2])))

;; position p is starting [rank, file]
;; direction dir is [up-down,left-right]
(defn knight-move-OK [p]
  (keep #(move p %) hops))


(def prank (reduce-kv (fn [m i kr] (assoc m kr (* 8 i))) {} kranks))
(defn tkm [[r f]]
  (let [anchor (dec (prank r))]
    (into [] (keep (fn [[rdiff fdiff]]
                      (let [f2 (+ f fdiff)]
                        (when (<= 1 f2 8)
                          (let [p (+ anchor f2 rdiff)]
                            (when (<= 0 p 63)
                              [(nth kranks (quot p 8)) f2]))))))
          [[-16 -1] [-8 -2] [-16 1] [-8 2] [8 2] [16 1] [8 -2] [16 -1]])))




(defn rinc [r]
  (case r
    :A :B
    :B :C
    :C :D
    :D :E
    :E :F
    :F :G
    :G :H
    nil))

(def rinc2 #(rinc (rinc %)))

(defn rdec [r]
  (case r
    :B :A
    :C :B
    :D :C
    :E :D
    :F :E
    :G :F
    :H :G
    nil))

(def rdec2 #(rdec (rdec %)))

(defn finc [f]
  (when (and f (<= 1 f 7))
    (inc f)))

(def finc2 #(finc (finc %)))

(defn fdec [f]
  (when (and f (<= 2 f 8))
    (dec f)))

(def fdec2 #(fdec (fdec %)))

(def fhops [[rdec2 fdec] [rdec fdec2]
            [rdec2 finc] [rdec finc2]
            [rinc finc2] [rinc2 finc]
            [rinc fdec2] [rinc2 fdec]])

(defn knight-move2 [p]
  (keep #(move p %) fhops))







(def hhh (for [r [-2 -1 1 2]
               f [-2 -1 1 2]
               :when (and (not= r f) (not= (- r) f))]
           [r f]))


(def hh (for [r [1 2]
              f [1 2]
              :when (and (not= r f))
              sr [+ -]
              sf [+ -]]
          [(sr r) (sf f)]))


(def ee (for [r [2 1]
              f [2 1]
              :when (and (not= r f))
              sr [- +]
              sf [- +]]
          [(sr r) (sf f)]))




(defn rplus-GOOD [r n]
  (cond (nil? r) nil
        (pos? n)  (recur (case r
                           :A :B
                           :B :C
                           :C :D
                           :D :E
                           :E :F
                           :F :G
                           :G :H
                           nil)
                         (dec n))
        (neg? n) (recur (case r
                          :B :A
                          :C :B
                          :D :C
                          :E :D
                          :F :E
                          :G :F
                          :H :G
                          nil) (inc n))
        ;; zero
        :else r))





(def r+ (zipmap ranks (conj (subvec ranks 1) nil)))
(def r- (zipmap ranks (cons nil ranks)))

(defn rplus [r n]
  (cond (nil? r) nil
        (pos? n) (recur (r+ r) (dec n))
        (neg? n) (recur (r- r) (inc n))
        :zero r))


(defn fplus [f n]
  (let [f2 (+ f n)]
    (when (<= 1 f2 8)
      f2)))

(defn km1 [[r f]]
  (for [sr [- +]
        sf [- +]
        [dr df] [[2 1] [1 2]]
        :let [r2 (rplus-GOOD r (sr dr))
              f2 (fplus f (sf df))]
        :when (and r2 f2)]
    [r2 f2]))


(defn km2 [[r f]]
  (for [sr [- +]
        sf [- +]
        [dr df] [[2 1] [1 2]]
        :let [r2 (rplus r (sr dr))
              f2 (fplus f (sf df))]
        :when (and r2 f2)]
    [r2 f2]))




(def nrank (reduce-kv (fn [m i r] (assoc m r (* 8 i))) {} ranks))

(defn rfi [r f]
  (when-let [nr (nrank r)]
    (when (<= 1 f 8)
      (+ nr (dec f)))))

(defn irf [i]
  (when (<= 0 i 63)
    [(nth ranks (quot i 8))
     (inc (rem i 8))]))

)
