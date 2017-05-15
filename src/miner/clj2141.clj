(ns miner.clj2141
  (:require [criterium.core :as cr]))

;; http://dev.clojure.org/jira/browse/CLJ-2141


;; Non-binary predicates.


;; Traditional Clojure predicates are named with a "?" suffix and return either true or
;; false.  The new 1.9 alpha predicates sometimes return nil.

;; Call me a Boolean Pedant, but I don't like it.

;; I'm surprised by the boolean binarism in the Clojure community.

;; What's wrong with nil-bending predicates?

;; Boolean creative predicates are the new thing.  Let a little nil in your life.

;; Real life is not strictly false.  Booleans are fluid.  Predicates should reflect that.

;; Let's put the questioning back in the question mark.  Predicates can return true, false,
;; or nil.  There's a spectrum of falseyness.

;; The boolean spectrum is better with nil.  Non-binary predicates for the win.

;; Nil is the non-binary falsey spirit.

;; Even with the wide acceptance of transients and transducers, there's still a lot of
;; resistance to nil-bending and a real distaste for mutable state.


;; The Clojure community unfortunately reinforces the traditional norms of the Boolean type as
;; a binary separation of true and false values.  This division is artificial and amounts to
;; erasure of diversity in several ways.
;; 
;; Much of it is very clearly socially constructed.  For example, the association of the
;; question mark suffix with strictly true/false predicates is merely a naming convention.
;; Functions can -- and in fact do -- return a wide variety of values, no matter how some
;; people label them.
;; 
;; The division of values into truthy and falsey has no scientific or grammatical basis.
;; Most individual values have a mix of bits that may be associated with both sides of the
;; binary dichotomy.
;; 
;; Boolean identities exist on a continuum, with false and nil generally accepted as falsey
;; and everything else as truthy.  (Although those terms have no acceptable scientific
;; definition, we think you know what we mean -- which sums up the problem pretty nicely.)
;; Naturally, false? is strictly true for false.  Of course, some?  is strictly false for
;; nil, yet true for false.
;; 
;; We value freedom of boolean expression.  Each predicate decides for itself what it will
;; return.  If it's feeling nil today, it should say so with pride.  Well behaved programs
;; shouldn't have a problem with predicate choices: true, false or nil.

;; cis-predicates?


;; benchmarking qualified-keyword? in Clj 1.9 alpha

(def sample (interleave (map #(keyword (str "k" %)) (range 1000))
                        (range 1000)
                        (map #(keyword (str "x/k" %)) (range 1000))))



(defn qualified-keyword-some? {:static true} [x]
  (and (keyword? x) (some? (namespace x))))

(defn qualified-keyword-not-nil?  {:static true} [x]
  (and (keyword? x) (not (nil? (namespace x)))))


(defn smoke [f]
  (take 100 (filter f sample)))

(defn qb [f]
  (println (str f))
  (cr/quick-bench (count (filter f sample)))
  (println))

(defn bench [f]
  (println (str f))
  (cr/bench (count (filter f sample)))
  (println))

(defn quick-all []
  (qb qualified-keyword?)
  (qb qualified-keyword-some?)
  (qb qualified-keyword-not-nil?))

(defn bench-all []
  (bench qualified-keyword?)
  (bench qualified-keyword-some?)
  (bench qualified-keyword-not-nil?))



