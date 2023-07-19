(ns miner.samefringe
  (:require [clojure.core.reducers :as r]))

;;; link from article about "Storing Data in Control Flow"
;;; https://research.swtch.com/pcdata


;; In his response, titled “Another samefringe”, McCarthy gives the following LISP solution:
;; 
;; (DE SAMEFRINGE (X Y)
;;        (OR (EQ X Y)
;;            (AND (NOT (ATOM X))
;;                 (NOT (ATOM Y))
;;                 (SAME (GOPHER X) (GOPHER Y)))))
;; 
;; (DE SAME (X Y)
;;        (AND (EQ (CAR X) (CAR Y))
;;             (SAMEFRINGE (CDR X) (CDR Y))))
;; 
;; (DE GOPHER (U)
;;        (COND ((ATOM (CAR U)) U)
;;              (T (GOPHER (CONS (CAAR U)
;;                               (CONS (CDAR U) (CDR U)))))))
;; He then explains:
;; 
;;     gopher digs up the first atom in an S-expression, piling up the cdr parts (with its hind
;;     legs) so that indexing through the atoms can be resumed. Because of shared structure,
;;     the number of new cells in use in each argument at any time (apart from those occupied
;;     by the original expression and assuming iterative execution) is the number of cars
;;     required to go from the top to the current atom – usually a small fraction of the size
;;     of the S-expression.
;; 
;; In modern terms, McCarthy’s GOPHER loops applying right tree rotations until the leftmost
;; node is at the top of the tree. SAMEFRINGE applies GOPHER to the two trees, compares the
;; tops, and then loops to consider the remainders.
;; 
;; After presenting a second, more elaborate solution, McCarthy remarks:
;; 
;;     I think all this shows that samefringe is not an example of the need for co-routines,
;;     and a new “simplest example” should be found. There is no merit in merely moving
;;     information from data structure to control structure, and it makes some kinds of
;;     modification harder.


;;; SEM comments: The original examples come from a time before laziness was implemented in
;;; Lisp.  In Clojure, flatten is lazy so it is actually a good solution.  However,
;;; clojure.core.reducers/flatten is much faster than the original built-in flatten so
;;; that is my favorite.  I have lots of other code here trying to understand JMC's
;;; solution (translated into Clojure) but it's all slow or stack-overflows in Clojure.


;;; JMC code
(defn gopher [u]
  (if-not (sequential? (first u))
    u
    (gopher (cons (ffirst u)
                  (concat (rest (first u)) (rest u))))))
;;; SEM changed second cons to concat above to make it work???


;;;; FIXME -- need to chame SAME and SAMEFRINGE per N -- should letfn them

(declare samefringe)

(defn same [x y]
  (and (= (first x) (first y))
       (samefringe (rest x) (rest y))))

(defn samefringe [x y]
  (or (= x y)
      (and (sequential? x)
           (sequential? y)
           (same (gopher x) (gopher y)))))


;;; based on JMC
(defn sf [x y]
  (letfn [(same [x y]
            (and (= (first x) (first y))
                 (sf (rest x) (rest y))))
          (gopher [u]
            (if-not (sequential? (first u))
              u
              (gopher (cons (ffirst u)
                            (concat (rest (first u)) (rest u))))))]
    (or (= x y)
        (and (sequential? x)
             (sequential? y)
             (same (gopher x) (gopher y))))))



;;; SEM hacking  but all StackOverflow -- except rflatten=
;;; I think I need to find the right way to accumulate and still be lazy

;; stack overflow on big
(defn sf1 [x y]
  (letfn [(same [x y]
            (and (= (first x) (first y))
                 (sf1 (rest x) (rest y))))
          (gopher [u]
            (if (sequential? (first u))
              (recur (concat (first u) (rest u)))
              u))]
    (or (= x y)
        (and (sequential? x)
             (sequential? y)
             (same (gopher x) (gopher y))))))

;; stack overflow on big
(defn sf2 [x y]
  (letfn [(same [x y]
            (and (= (first x) (first y))
                 (sf2 (rest x) (rest y))))
          (gopher [u]
            (if-not (sequential? (first u))
              u
              (lazy-seq (gopher (concat (first u) (rest u))))))]
    (or (= x y)
        (and (sequential? x)
             (sequential? y)
             (same (gopher x) (gopher y))))))




;;; slow no stack overflow
(defn sf3 [x y]
  (letfn [(same [x y]
            (and (= (first x) (first y))
                 (sf3 (rest x) (rest y))))
          (gopher [u]
            (if (sequential? (first u))
              (recur (into (vec (first u)) (rest u)))
              u))]
    (or (= x y)
        (and (sequential? x)
             (sequential? y)
             (same (gopher x) (gopher y))))))


;;; no good
(defn sf32 [x y]
  (letfn [(same [x y]
            (and (= (first x) (first y))
                 (sf32 (rest x) (rest y))))
          (gopher [u]
            (if (sequential? (first u))
              (recur (cons (ffirst u) (sequence cat [(rest (first u)) (rest u)])))
              u))]
    (or (= x y)
        (and (sequential? x)
             (sequential? y)
             (same (gopher x) (gopher y))))))


;;; about same as sf3
(defn sf33 [x y]
  (letfn [(same [x y]
            (and (= (first x) (first y))
                 (sf33 (rest x) (rest y))))
          (gopher [u]
            (if (sequential? (first u))
              (recur (into [(ffirst u)] cat [(rest (first u)) (rest u)]))
              u))]
    (or (= x y)
        (and (sequential? x)
             (sequential? y)
             (same (gopher x) (gopher y))))))

;;; horribly slow
(defn sf5 [x y]
  (or (= x y)
      (and (sequential? x)
           (sequential? y)
           (loop [xs x ys y]
             (cond (empty? xs) (empty? ys)
                   (empty? ys) (empty? xs)
                   :else (let [x1 (first xs)
                               y1 (first ys)]
                           (cond (sequential? x1) (recur (into (vec x1) (rest xs)) ys)
                                 (sequential? y1) (recur xs (into (vec y1) (rest ys)))
                                 (= x1 y1) (recur (rest xs) (rest ys))
                                 :else nil)))))))




;; pretty slow but no stack overflow, slower than sf3
(defn sf4 [x y]
  (letfn [(same [x y]
            (and (= (first x) (first y))
                 (sf4 (rest x) (rest y))))
          (gopher [u]
            (if (sequential? (first u))
              (recur (sequence cat [(first u) (rest u)]))
              u))]
    (or (= x y)
        (and (sequential? x)
             (sequential? y)
             (same (gopher x) (gopher y))))))

;;; horribly slow, but no stack overflow
(defn sf5 [x y]
  (letfn [(same [x y]
            (and (= (first x) (first y))
                 (sf5 (rest x) (rest y))))
          (gopher [nest extra]
            ;;(println "gopher" nest extra)
            (if (sequential? nest)
              (let [head (first nest)]
                (if (sequential? head)
                  (recur (first head) (sequence cat [(rest head) (rest nest) extra]))
                  (into (vec nest) extra)))
              (cons nest extra)))]
    (or (= x y)
        (and (sequential? x)
             (sequential? y)
             (same (gopher x nil) (gopher y nil))))))


;;; works but not fast  
(defn sf9 [x y]
  (let [flathead (fn [xs]
                   (if (empty? xs)
                     xs
                     (let [head (first xs)]
                       (if (sequential? head)
                         (recur (concat head (rest xs)))
                         xs))))]
    (or (= x y)
        (let [fx (flathead x)
              fy (flathead y)]
          (and (= (first fx) (first fy))
               (recur (rest fx) (rest fy)))))))


(defn sf8 [x y]
  (let [flathead (fn [xs]
                   (let [head (first xs)]
                     (if (sequential? head)
                       (if-let [tail (next xs)]
                         (recur (concat head tail))
                         head)
                       xs)))]
    (or (= x y)
        (let [fx (flathead x)
              fy (flathead y)]
          (and (= (first fx) (first fy))
               (recur (rest fx) (rest fy)))))))

(defn sf81 [x y]
  (let [flathead (fn [xs]
                   (let [head (first xs)]
                     (if (sequential? head)
                       (if-let [tail (next xs)]
                         (recur (into (vec head) tail))
                         head)
                       xs)))]
    (or (= x y)
        (let [fx (flathead x)
              fy (flathead y)]
          (and (= (first fx) (first fy))
               (recur (rest fx) (rest fy)))))))



;;; simple but article says don't do it!  But that was before laziness was understood.
(defn flatten= [x y]
  (or (= x y)
      (and (sequential? x) (sequential? y) (= (flatten x) (flatten y)))))

;;; but compare to built-in flatten.  This seems slower if you only need first, but faster
;;; for big structure.  Still r/flatten is much faster (on my hardware)
(defn stflat [xs]
  (sequence (mapcat (fn [x] (if (sequential? x) (stflat x) (list x)))) xs))

(defn xflatten= [x y]
  (or (= x y)
      (and (sequential? x) (sequential? y) (= (stflat x) (stflat y)))))

(def nested (doall (reverse (reductions conj [] (map vector (range 100))))))
(def bad (concat nested [:fail]))
(def flatted (doall (flatten nested)))

(defn test-samefr [samefringe]
  (assert (samefringe '(A (B (C (D)))) '((((A) B) C) D)))
  (assert (samefringe nested flatted))
  (assert (not (samefringe bad nested)))
  true)



;;;; c.c.reducers version is faster!
(defn rflatten [coll]
  (into [] (r/flatten coll)))

(defn rflatten= [x y]
  (or (= x y)
      (and (sequential? x) (sequential? y) (= (rflatten x) (rflatten y)))))


;;; From old article
;;;
;;; The "samefringe" problem of testing trees for equality of tips (even if the trees have a
;;; different internal structure) seems to be the main example used to illustrate the
;;; usefulness of coroutines in A] languages.  ...  but please don't flatten the trees!

;; (SAMEFRINGE T1 T2) = T iff (EQUAL (FLATTEN TI) (FLATTEN T2))

;; (SAMEFRINGE '(A (B (C (D)))) '((((A) B) C).D))

;;;;;; [copy-paste needs some correction below]
;;
;; The problem seems to have a very simple "iterative" solution
;; in ordinary LISP without any non-recursive or unorthodox control
;; structure, and the solution is:
;; 
;; (OE SAMEFRINGE (TI T2)
;;     (COND ((NULL TI) (NULL T2))
;;           (T ((Lamba (Tt T2)
;;                         (and (EQ (CAR TI) (CRR T2))
;;                              (SAMEFRINGE (CDR TI) (CDR T2))))
;;               (FRINGE TI) (FRINGE T2)))))
;; 
;; (DE FRINGE (TREE)
;;     (COND ((NULL TREE) NIL)
;;           ((NULL (CAR TREE)) (FRINGE (CDR TREE)))
;;           ((ATOM (CAR TREE)) (LIST (CAR TREE) (CDR TREE)))
;;           (T (APPLY (lambda (TI T2)
;;                               (COND (TI (LIST T1 (CONS T2 (CDR TREE))))
;;                               (T (FRINGE (CDR TREE)))))
;;              (FRINGE (CDR TREE))))))


      
            

