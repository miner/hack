(ns miner.defkey)



;; My preliminary thoughts...
;; I think we want declared keywords to avoid typos, unexpected keys.
;; Could be a ns declaration addition.  By default, kws don't need ns declarations.  But
;; they wanted something for easier aliasing.

;; typical alias
(alias 'x 'a.b.x)

;; But that requires, a.b.x to already exist!

;; This works, but the doc doesn't seem to guarantee it.  Could there be an issue with
;; pre-existing ns, or maybe with creating ns later???

(alias 'x (create-ns 'a.b.x))

;; I want a skinny keyword namespace that warns or throws for an undeclared kw
;;
;; ::x/a
;;
;; Could linter simply warn on unspecked kw?
;; Could meta on ns declare kw list, more like symbol options.
;;
;; Using =sym def is OK but not self-evaluating so gets tricky buried in quoted nesting.



;; pseudo macro

#_
(defmacro defkey
  ([key] `(def ^:const =(name key) ~key))
  ([sym key] `(def ^:const sym ~key))
  )


;; could also compile into a let scope
;; (defmacro with-keys [=bar =foo] ...)
#_
(let [=bar ::bar
      =foo ::foo]
  ;; ...
  )



;; https://twitter.com/cemerick/status/885347293884559360


(def ^:const =foo ::foo)

(def ^:const =bar :un.known.ns/bar)




;;; ----------------------------------------------------------------------
;;; UNRELATED STUFF

;; https://github.com/ztellman/gloss
;; This is considered the best way to get bytes from a long
(defn long->byte-array [^long n]
  (-> (ByteBuffer/allocate 8) (.putLong n) .array))

(defn lbsz [^long n]
  (seq (long->byte-array n)))

;; kind of cheating by not using byte-array
(defn lbs [n]
  (map #(unchecked-byte (bit-shift-right n %)) (range 56 -1 -8)))

(defn lba [n]
  (byte-array (map #(unchecked-byte (bit-shift-right n %)) (range 56 -1 -8))))
