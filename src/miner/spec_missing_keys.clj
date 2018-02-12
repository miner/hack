;;; SEM borrowed code
(ns miner.missing-keys
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))


;; Stuart Halloway
;; https://gist.github.com/stuarthalloway/f4c4297d344651c99827769e1c3d34e9

;; I think it would be a mistake to introduce temporal coupling to prevent typos.

;; The example program below lets you identify "missing" keys specs at
;; the time and place of your choosing, and then handle them as you
;; deem appropriate, without imposing those decisions on other
;; users of spec.


(defn keyspec-form?
  "Returns true if x is the s/form of a keys spec"
  [x]
  (and (sequential? x) (= 'clojure.spec.alpha/keys (first x))))

(defn keyspec-form-keys
  "Returns set of keys mentioned in s/form of a keys spec"
  [x]
  (into
   #{}
   (comp (filter vector?)
         cat)
   x))

(defn keyspec-keys
  "Given an s/registry, returns a map from keys spec names to
the set of qualified keys referenced by the spec."
  [registry]
  (into
   {}
   (comp (map (fn [[k v]] [k (s/form v)]))
         (filter (fn [[_ v]] (keyspec-form? v)))
         (map (fn [[k v]] [k (keyspec-form-keys v)])))
   registry))

(defn possible-keyspec-typos
  "Given a spec registry, returns a map from keys specs to
keynames missing from the registry. Useful for e.g. finding
typos when you believe you have specified all keys"
  [registry]
  (let [ksks (keyspec-keys registry)
        rks (into #{} (keys registry))]
    (into
     {}
     (comp (map (fn [[n ks]]
                  [n (set/difference ks rks)]))
           (filter (fn [[_ ks]] (seq ks))))
     (keyspec-keys registry))))

(comment
  (s/def ::foo int?)
  (s/def ::bar (s/keys :req-un [::foo ::quux]))
  ;; this will show you that ::quux is missing
  (possible-keyspec-typos (s/registry))

  )
