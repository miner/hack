(ns miner.eknight)

;; https://gist.github.com/ericnormand/f217a8c3dc07fe0def69532877182365
;;
;; chess board is defined as rows 1-8 and columns :A - :H.  [:A 1] is lower left
;; corner.  I guess that should be "rank" and "file" in chess terms.


;; Interesting extension (not implemented):  Define all the paths to get from a position to
;; another square in so many moves.  More or less: calculated all the one-hop moves and then
;; fan out from current position until board is coverd.  This link shows interactive web
;; page illustrating moves:
;;
;; https://tamas-szabo.com/knightjumps/

;; Another extension:  Knight's tour.  Calculate a path of jumps from one corner covering
;; every square once.
;;
;; https://en.wikipedia.org/wiki/Knight's_tour

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





;; @steffan-westcott writes:  As a bit of fun, I was inspired by @sztamas to add a shortest
;; knight move path challenge!  Building on your knight-move function, add a new function
;; which takes source and destination squares and returns a collection of all shortest paths
;; of knight moves between them.


;; SEM I didn't prove it, but it looks like a Knight can cover the whole board in six or
;; fewer moves so there's no need to account for failures.  Also, pretty sure a knight won't
;; get stuck in a cycle.  BUT, he can always jump right back!  Apparently, not worth the
;; effort to avoid cycles.

(defn shortest-paths [start target]
  (let [success? (fn [path] (= target (peek path)))]
    (loop [paths [[start]]]
      (or (seq (filter success? paths))
          (recur (mapcat (fn [path] (map #(conj path %) (knight-move (peek path)))) paths))))))



(defn shortest-paths1 [start target]
  (let [success? (fn [path] (= target (peek path)))]
    (loop [paths [[start]]]
      (if-let [sols (seq (filter success? paths))]
        sols
        (recur (mapcat (fn [path] (map #(conj path %) (knight-move (peek path)))) paths))))))

(defn shortest-paths-SAFE [start target]
  (let [success? (fn [path] (= target (peek path)))]
    (loop [paths [[start]]]
      (if (empty? paths)
        nil ;; should never happen
        (or (seq (filter success? paths))
            (recur (mapcat (fn [path] (map #(conj path %) (knight-move (peek path)))) paths)))))))


;; trying to avoid cycles, but it's slower
;; be careful about empty paths (potential failure), but we know that shouldn't happen
(defn sp2 [start target]
  (let [success? (fn [path] (= target (peek path)))
        extend-path (fn [path] (map #(conj path %) (remove (set path) (knight-move (peek path)))))]
    (loop [paths [[start]]]
      (when (seq paths)
        (or (seq (filter success? paths))
            (recur (mapcat extend-path paths)))))))


(defn sp3 [start target]
  (let [success? (fn [path] (= target (peek path)))
        extend-path (fn [path] (map #(conj path %)
                                    (remove (set (pop path)) (knight-move (peek path)))))]
    (loop [paths [[start]]]
      (when (seq paths)
        (or (seq (filter success? paths))
            (recur (mapcat extend-path paths)))))))

;;; consider keeping set of path as peek, but not fast enough
(defn sp33 [start target]
  (let [success? (fn [path] (= target (peek (pop path))))
        extend-path (fn [path]
                      (let [ps (peek path)
                            path (pop path)]
                        (map #(conj path % (conj ps %))
                                    (remove ps (knight-move (peek path))))))]
      (map pop (loop [paths [[start (hash-set start)]]]
                 (when (seq paths)
                   (or (seq (filter success? paths))
                       (recur (mapcat extend-path paths))))))))


(defn sp34 [start target]
  (let [success? (fn [pathx] (= target (peek (first pathx))))
        extend-path (fn [pathx]
                      (let [path (first pathx)
                            ps (second pathx)]
                        (map #(list (conj path %) (conj ps %))
                                    (remove ps (knight-move (peek path))))))]
      (map first (loop [paths [(list [start] #{start})]]
                   (when (seq paths)
                     (or (seq (filter success? paths))
                         (recur (mapcat extend-path paths))))))))






;; instead of set of squares use bits in a long


(defn fri [[f r]]
  (+ (* (ifile f) 8) (dec r)))

(defn ifr [i]
  [(nth kfiles (quot i 8))
   (inc (rem i 8))])




;; not faster to bit-or instead of +

(defn bkm [b]
  (let [f (bit-shift-right b 3)
        r (bit-and b 7)]
    (into [] (keep (fn [[rdiff fdiff]]
                      (let [f2 (+ f fdiff)]
                        (when (zero? (bit-shift-right f2 3))
                          (let [r2 (+ r rdiff)]
                            (when (zero? (bit-shift-right r2 3))
                              (+ (bit-shift-left f2 3) r2)))))))
          [[-2 -1] [-1 -2] [-2 1] [-1 2] [1 2] [2 1] [1 -2] [2 -1]])))


;; keep position in one long rank in bits 543, file bits 210

(def bfile (reduce-kv (fn [m i kf] (assoc m kf (bit-shift-left i 3))) {} kfiles))

(defn frb [[f r]]
  (+ (bfile f) (dec r)))

(defn bfr [b]
  [(nth kfiles (bit-shift-right b 3))
   (inc (bit-and b 7))])

;; just for testing compatible with original knight-move
(defn frbkm [fr]
  (map bfr (bkm (frb fr))))



;; for finding short path -- converts once to bit rep, then back at end
(defn sp56 [start target]
  (let [st (fri start)
        targ (fri target)
        success? (fn [bpath] (= targ (peek (first bpath))))
        extend-path (fn [bpath]
                      (let [path (first bpath)
                            bits (second bpath)]
                        (map #(list (conj path %) (bit-set bits %))
                             (remove #(bit-test bits %) (bkm (peek path))))))]
    (->>
     (loop [bpaths [(list [st] (bit-set 0 st))]]
       (when (seq bpaths)
         (or (seq (filter success? bpaths))
             (recur (mapcat extend-path bpaths)))))
     (map first)
     (map #(map bfr %)))))

;; FASTEST better with transducers
(defn sp571 [start target]
  (let [st (frb start)
        targ (frb target)
        success? (fn [bpath] (= targ (peek (first bpath))))
        extend-path (fn [bpath]
                      (let [path (first bpath)
                            bits (second bpath)]
                        (into [] (comp (remove #(bit-test bits %))
                                       (map #(list (conj path %) (bit-set bits %))))
                              (bkm (peek path)))))]
    (into [] (comp (map first)
                   (map #(mapv bfr %)))
     (loop [bpaths [(list [st] (bit-set 0 st))]]
       (when (seq bpaths)
         (or (seq (filter success? bpaths))
             (recur (mapcat extend-path bpaths))))))))


(defn sp58 [start target]
  (let [st (frb start)
        targ (frb target)
        success? (fn [bpath] (= targ (peek (first bpath))))
        extend-path (fn [bpath]
                      (let [path (first bpath)
                            bits (second bpath)]
                        (into [] (comp (remove #(bit-test bits %))
                                       (map #(list (conj path %) (bit-set bits %))))
                              (bkm (peek path)))))]
    (into [] (comp (map first)
                   (map #(sequence (map bfr) %)))
     (loop [bpaths [(list [st] (bit-set 0 st))]]
       (when (seq bpaths)
         (or (seq (filter success? bpaths))
             (recur (mapcat extend-path bpaths))))))))

;; not better
(defn sp581 [start target]
  (let [st (frb start)
        targ (frb target)
        success? (fn [bpath] (= targ (peek (first bpath))))
        extend-path (fn [bpath]
                      (let [path (first bpath)
                            bits (second bpath)]
                        (into [] (comp (remove #(bit-test bits %))
                                       (map #(list (conj path %) (bit-set bits %))))
                              (bkm (peek path)))))]
    (into [] (comp (map first)
                   (map #(into [] (map bfr) %)))
     (loop [bpaths [(list [st] (bit-set 0 st))]]
       (when (seq bpaths)
         (or (seq (filter success? bpaths))
             (recur (mapcat extend-path bpaths))))))))



;; bpath is list path ints and one long of bits (i squares)
(defn sp55 [start target]
  (let [st (fri start)
        targ (fri target)
        success? (fn [bpath] (= targ (peek (first bpath))))
        kms (fn [sqi] (map fri (knight-move (ifr sqi))))
        extend-path (fn [bpath]
                      (let [path (first bpath)
                            bits (second bpath)]
                        (map #(list (conj path %) (bit-set bits %))
                             (remove #(bit-test bits %) (kms (peek path))))))]
    (->>
     (loop [bpaths [(list [st] (bit-set 0 st))]]
       (when (seq bpaths)
         (or (seq (filter success? bpaths))
             (recur (mapcat extend-path bpaths)))))
     (map first)
     (map #(map ifr %)))))


;;; IDEAS keep a set of pop path to reuse for filter
;;; keep a tree [HEAD [C1 [C11 [C12 C123 C1234]] [C2 C22 C23] [C3 C33 C34]]]


;;; SLOWER
(defn sp4 [start target]
  (let [success? (fn [path] (= target (peek path)))
        extend-path (fn [path] (map #(conj path %)
                                    (remove (fn [p] (some #(= p %) (pop path)))
                                            (knight-move (peek path)))))]
    (loop [paths [[start]]]
      (when (seq paths)
        (or (seq (filter success? paths))
            (recur (mapcat extend-path paths)))))))




(defn shortest-paths-WORKS [start target]
  (loop [paths [[start]] depth 0]
    (when (< depth 10)
      ;;(println paths)
      (if-let [sols (seq (filter #(= target (peek %)) paths))]
        sols
        (recur (mapcat (fn [path] (map #(conj path %) (knight-move (peek path)))) paths)
               (inc depth))))))
      

(defn smoke-short [shortest-paths]
  (assert? = (shortest-paths [:H 6] [:F 2])
           '( [[:H 6] [:G 4] [:F 2]] )
           (sort-by vec (shortest-paths [:A 1] [:B 1]))
           '( [[:A 1] [:B 3] [:D 2] [:B 1]]   [[:A 1] [:C 2] [:A 3] [:B 1]] )))


(defn smoke-sh [shortest-paths]
  (and (smoke-short shortest-paths)
       (assert? =
           (count (shortest-paths [:A 1] [:H 8])) 108
           (count (shortest-paths [:H 8] [:A 1])) 108
           (count (shortest-paths [:H 1] [:A 8])) 108
           (count (shortest-paths [:A 8] [:H 1])) 108)))

           


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



;;; SEM: I would like to change the notation to a single kw, like :A1, instead of the
;;; vector.  This is for human convenience.

(defn kw? [kw]
  (and (keyword? kw) (= (count (name kw)) 2)))

(defn kwv [kw]
  (let [nm (name kw)]
    [(keyword (subs nm 0 1))
     (Long/parseLong (subs nm 1))]))

(defn vkw [[f r]]
  (keyword (str (name f) r)))


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
