(ns miner.unroll
  (:require [clojure.template :as tem]
            [clojure.tools.macro :as mac]
            [backtick :as bt]))



#_
(defn const-seq? [coll]
  (and (seqable? coll)
       (or (= (first coll) 'range)
           (every? number? coll))))


(defn constify [coll]
  (or (and (seqable? coll)
           (every? number? (rest coll))
           (cond (empty? coll) coll
                 (number? (first coll)) coll
                 (= (first coll) 'range) (apply range (rest coll))
                 (= (first coll) `range) (apply range (rest coll))
                 ;; support repeat maybe
                 ;; probably not iterate or map
                 :else nil))
      (throw (ex-info "Expected a constant seq expr" {:bad coll}))))

(defmacro unroll
  ([bindings form] `(unroll do ~bindings ~form))
  ([sumf bindings form]
   (assert (and (vector? bindings) (even? (count bindings))))
   ;;   (assert (every? const-seq? (take-nth 2 (rest bindings))))
   (assert (symbol? sumf))
   (let [params# (vec (take-nth 2 bindings))
         intlvs# (apply map vector (map constify (take-nth 2 (rest bindings))))]
     `(~sumf ~@(map (fn [args] (clojure.template/apply-template params# form args))
                    intlvs#)))))



;; hand-written, smart about not needing shift 0
(defn enc6b [v]
  (bit-or (bit-shift-left (nth v 0 0) 40)
          (bit-shift-left (nth v 1 0) 32)
          (bit-shift-left (nth v 2 0) 24)
          (bit-shift-left (nth v 3 0) 16)
          (bit-shift-left (nth v 4 0) 8)
          (nth v 5 0)))

;; 6 bytes [0..255] big-endian into one long
(defn encode-6bytes [v]
  (unroll bit-or
          [i (range 6) sh [40 32 34 16 8 0]]
          (bit-shift-left (nth v i 0) sh)))



;;; UNFINISHED idea to use backtick

(defmacro unroll2
  ([bindings form] `(unroll2 do ~bindings ~form))
  ([sumf bindings form]
   (assert (and (vector? bindings) (even? (count bindings))))
   ;;   (assert (every? const-seq? (take-nth 2 (rest bindings))))
   (assert (symbol? sumf))
   (let [params# (vec (take-nth 2 bindings))
         uform# (clojure.template/apply-template params# form (mapv #(list 'unquote %) params#))
         intlvs# (apply map vector (map constify (take-nth 2 (rest bindings))))]
     `(~sumf ~@(map (fn [args] (:UNFINISHED uform#))
                    intlvs#)))))


;;; think about symbol-macrolet
;;; issue is read-time for backquote



;; works but not as clean
(defmacro unroll-SAVE
  ([bindings form] `(unroll-SAVE do ~bindings ~form))
  ([sumf bindings form]
   (assert (and (vector? bindings) (even? (count bindings))))
   ;; (assert (every? const-seq? (take-nth 2 (rest bindings))))
   (assert (symbol? sumf))
   (let [vrs# (vec (take-nth 2 bindings))
         intlvs# (apply map vector (map constify (take-nth 2 (rest bindings))))]
     `(~sumf ~@(map (fn [args#]
                      (clojure.template/apply-template vrs# form args#))
                    intlvs#)))))
