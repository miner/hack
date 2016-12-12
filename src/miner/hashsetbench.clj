(ns miner.hashsetbench
 (:import [java.util HashSet Collection Collections]
          [java.awt Point]))


;; From: Michael Gardner <gardnermj@gmail.com>
;; Subject: Re: Help me understand what part of this code is slow, and how to make it faster?
;; Date: November 17, 2016 at 12:41:55 AM EST

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn neighbors [^Point p]
 (let [x (.-x p), y (.-y p)]
   [(Point. x (inc y))
    (Point. x (dec y)) 
    (Point. (inc x) y)
    (Point. (dec x) y)]))

(defn nth-neighbors [^long n ^Point p]
 (loop [n n, s1 (doto (HashSet.) (.add p)), s2 (HashSet.)]
   (if (zero? n) s1
     (let [s0 (HashSet.)]
       (doseq [_ s1, p (neighbors _)]
         (when-not (or (.contains s1 p) (.contains s2 p))
           (.add s0 p)))
       (recur (dec n) s0 s1)))))

;; ----------------------------------------------------------------------

;; SEM: my theory, x/y could be two 32bits in one long
;; And use data.int-set if you can make x/y as two int-sets

;; lein [org.clojure/data.int-map "0.2.4"]
;; [clojure.data.int-map :as i]


;; gardnermj@gmail.com
;;; 11/21/16  15:45 by miner -- update with satisfactory time

;; SEM: hashCode looks wrong since it should be an int, not long
;; probably everything would be better with ints

#_ (import java.util.Collection)
#_ (import java.util.HashSet)

(deftype point5 [^long i ^long j]
  Object
  (equals [this that] (and (= (.i this) (.i ^point5 that))
                           (= (.j this) (.j ^point5 that))))
  (hashCode [this] (+ (.i this) (* 4000 (.j this)))))

(defn iter-neighbors5 [f ^point5 p]
  (let [i ^long (.i p)
        j ^long (.j p)]
    (f (->point5 (dec i) j))
    (f (->point5 (inc i) j))
    (f (->point5 i (dec j)))
    (f (->point5 i (inc j)))))

(defn nth5 [n p]
  (loop [n ^long n
         s1 (HashSet. ^Collection [p])
         s2 (HashSet.)]
    (if (zero? n)
      s1
      (let [s0 (HashSet.)]
        (letfn [(add [p]
                     (when (not (or (.contains s1 p) (.contains s2 p)))
                       (.add s0 p)))]
               (doseq [p s1] (iter-neighbors5 add p))
               (recur (dec n) s0 s1))))))

;;; 11/21/16  15:46 by miner -- my guess



;; SEM: equals need an instance check
;; http://stackoverflow.com/questions/27581/what-issues-should-be-considered-when-overriding-equals-and-hashcode-in-java


(deftype point8 [^long i ^long j]
  Object
  (toString [this] (str [(.i this) (.j this)]))

  ;; must check class of `that`
  (equals [this that] (or (identical? this that)
                          (and (instance? point8 that)
                               (= (.i this) (.i ^point8 that))
                               (= (.j this) (.j ^point8 that)))))

  ;; seems like speed is highly dependent on cheap hashCode
  #_ (hashCode [this] (+ (.i this) (* 4000 (.j this))))
  ;; pretty good
  (hashCode [this] (unchecked-add (.i this) (unchecked-multiply 31 (.j this))))
  
  ;; clever bits that weren't faster
  #_ (hashCode [this] (bit-xor (.i this) (bit-shift-left (.j this) 16)))

  #_ (hashCode [this] (+ (.i this) (Long/rotateLeft (.j this) (int 16)))))

(defn iter-neighbors8 [f ^point8 p]
  (let [i ^long (.i p)
        j ^long (.j p)]
    (f (->point8 (unchecked-dec i) j))
    (f (->point8 (unchecked-inc i) j))
    (f (->point8 i (unchecked-dec j)))
    (f (->point8 i (unchecked-inc j)))))

(defn nth8 [n p]
  (loop [n ^long n
         s1 (HashSet. (Collections/singletonList p))
         s2 (HashSet.)]
    (if (zero? n)
      s1
      (let [s0 (HashSet.)]
        (letfn [(add [p]
                     (when (not (or (.contains s1 p) (.contains s2 p)))
                       (.add s0 p)))]
               (doseq [p s1] (iter-neighbors8 add p))
               (recur (dec n) s0 s1))))))


;;; 11/22/16  08:45 by miner -- michal.marczyk@gmail.com came up with a faster version.
;; SEM but note that he's using the unsafe `equals` -- so I changed that to be fair.

(deftype point [^long i ^long j]
  Object
  ;; SEM: must check class of `that`
  (equals [this that] (or (identical? this that)
                          (and (instance? point that)
                               (= (.i this) (.i ^point that))
                               (= (.j this) (.j ^point that)))))

  ;; original version -- unsafe
  #_ (equals [this that]
    (and (= (.i this) (.i ^point that))
         (= (.j this) (.j ^point that))))
  (hashCode [this]
    (+ (.i this) (* 4000 (.j this)))))

(defn nth-shell [^long n p]
  (loop [n  n
         s1 (doto (HashSet.) (.add p))
         s2 (HashSet.)]
    (if (zero? n)
      s1
      (let [s0 (HashSet. (* 4 (.size s1)))
            i1 (.iterator s1)]
        (dotimes [_ (.size s1)]
          (let [^point p (.next i1)
                i ^long (.i p)
                j ^long (.j p)
                p1 (point. (dec i) j)
                p2 (point. (inc i) j)
                p3 (point. i (dec j))
                p4 (point. i (inc j))]
            (if-not (or (.contains s1 p1) (.contains s2 p1))
              (.add s0 p1))
            (if-not (or (.contains s1 p2) (.contains s2 p2))
              (.add s0 p2))
            (if-not (or (.contains s1 p3) (.contains s2 p3))
              (.add s0 p3))
            (if-not (or (.contains s1 p4) (.contains s2 p4))
              (.add s0 p4))))
        (recur (dec n) s0 s1)))))

;; Also, to check that this still outputs the same neighbours the original impl (nth*) does:

#_ (defn persistent-shell [shell]
  (persistent!
    (reduce (fn [out ^point p]
              (conj! out [(.-i p) (.-j p)]))
      (transient #{})
      shell)))

;; SEM: his example didn't work for me
