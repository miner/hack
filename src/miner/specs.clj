(ns miner.specs
  (:require [clojure.spec :as s]
            [clojure.walk :as walk]))

;; Clojure mailing list reported bug on spec using partial.  Real issue was the proto was
;; updated later by extend-type so (partial satisfies? Proto) predicate was out-dated.  I
;; thought maybe the spec description should be annotated to indicate that form might be out
;; of date.  But not all vars.  Most are just fn names that would never change.  Seems
;; strange to chase down just protocols.  Bad idea after all.


(defn protosym? [x]
  (when (symbol? x)
    (when-let [v (resolve x)]
      (when (var? v)
        (when-let [proto (var-get v)]
          (and (associative? proto) (= v (:var proto)) (class? (:on-interface proto))))))))

(defn qual? [x]
  (and (symbol? x) (namespace x)))

(defn unqual [sym]
  (let [simple   (symbol (name sym))]
  (if (protosym? sym)
    (list 'do (hash sym) simple)
    simple)))

;; hacked c/spec.clj  abbrev

(defn abb [form]
  (cond
   (seq? form)
   (walk/postwalk (fn [form]
                    (cond (qual? form) (unqual form)

                     (and (seq? form) (= 'fn (first form)) (= '[%] (second form)))
                     (last form)
                     
                     :else form))
                  form)

      (qual? form) (unqual form)

   :else form))



(comment

(require '[clojure.spec :as s])
(defprotocol Game)

(defrecord Foo [])

(s/def ::game1 (partial satisfies? Game))

(s/def ::game2 #(satisfies? Game %))

;; end comment
)
