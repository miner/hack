(ns eric.chessmv)

;; https://gist.github.com/ericnormand/655bca753bced9299363ee8d41df8b5e

;; Chess moves
;; 
;; Write a function that determines if a chess piece, on an empty board, can move from one
;; space to another in one move.
;; 
;; Assume that pawns are moving from the low to the high numbers.
;; You can ignore en passant, pawn's capture, castling, and pawn's two-square move on the
;; second rank.


;; ----------------------------------------------------------------------
;; Board positions FR, file A-H, rank 1-8

;; Algebraic Notation for chess
;; https://www.thechessdrum.net/chessacademy/thechessboard.html


;; nth is faster for char access than first/second

(defn file [cc]
  (case (nth cc 0)
    \A 0
    \B 1
    \C 2
    \D 3
    \E 4
    \F 5
    \G 6
    \H 7
    nil))

(defn rank [cc]
  (case (nth cc 1)
    \1 0
    \2 1 
    \3 2
    \4 3
    \5 4
    \6 5
    \7 6
    \8 7
    nil))

(defn abs [n]
  (if (neg? n) (- n) n))

;; check valid positions in common logic
;; specialize just on piece
;; calculate with the deltas:  delta-file delta-rank 
(defmulti dmove? (fn [piece df dr] piece))

;; most of the logic is common, just specialized on the deltas
(defn can-move? [piece a b]
  (when-not (= a b)
    (let [ra (rank a)
          rb (rank b)
          fa (file a)
          fb (file b)]
      (when (and ra rb fa fb)
        (dmove? piece (- fb fa) (- rb ra))))))


;; not necessary
(defmethod dmove? :default [piece df dr]
  (println "dmove default" piece)
  false)

;; ignoring capturing and double first move
(defmethod dmove? :pawn [p df dr]
  (and (= dr 1) (zero? df)))

;; ignoring castling
(defmethod dmove? :king [p df dr]
  (and (<= (abs dr) 1) (<= (abs df) 1)))

(defmethod dmove? :queen [p df dr]
  (or (zero? df)
      (zero? dr)
      (= (abs df) (abs dr))))

;; ignoring castling
(defmethod dmove? :rook [p df dr]
  (or (zero? df) (zero? dr)))

(defmethod dmove? :bishop [p df dr]
  (= (abs df) (abs dr)))

(defmethod dmove? :knight [p df dr]
  (let [ar (abs dr)
        af (abs df)]
    (and (= (min ar af) 1)
         (= (max ar af) 2))))

        



(defn smoke-cm [can-move?]
  (assert (can-move? :pawn "A2" "A3"))
  (assert (not (can-move? :pawn "A2" "B2")))
  (assert (can-move? :queen "H1" "A8"))
  (assert (not (can-move? :knight "A4" "A5")))
  (assert (can-move? :knight "A4" "C5"))
  (assert (not (can-move? :king "A8" "A9"))) ; (that's off the board)
  (assert (not (can-move? :rook "A4" "A4")))
  (assert (can-move? :rook "A4" "A8"))
  (assert (can-move? :rook "B5" "F5"))
  (assert (can-move? :bishop "H1" "A8"))
  (assert (not (can-move? :bishop "H2" "A8")))
  true)

 

  


;; slightly changed into one fn
(defn sw-can-move? [piece orig dest]
  (let [sq-coords (fn [sq]
                    (let [sq' (mapv #(- (int %1) %2) sq [65 49])]
                      (when (every? #(<= 0 % 7) sq')
                        sq')))
        orig-sq (sq-coords orig)
        dest-sq (sq-coords dest)]
    (if (and orig-sq dest-sq (not= orig dest))
      (let [[dx dy] (map - dest-sq orig-sq)
            same-rank? (zero? dy)
            same-file? (zero? dx)
            same-diagonal? (= (* dx dx) (* dy dy))]
        (case piece
          :pawn (and same-file? (= 1 dy) (pos? (get orig-sq 1)))
          :rook (or same-file? same-rank?)
          :bishop same-diagonal?
          :queen (or same-file? same-rank? same-diagonal?)
          :king (every? #(<= -1 % 1) [dx dy])
          :knight (= 5 (+ (* dx dx) (* dy dy)))
          false))
      false)))
