(ns miner.erule)

;; https://gist.github.com/ericnormand/2318405631f158d6f2ceaaff3161c6c4

;; Boolean combinators.  Note that rule-and and rule-or are monoids and so they follow the
;; monoid pattern.  For a bonus, write the functions rule-if (that implements the logical if
;; arrow operator) and rule-iff (that implements the logical if and only if double-arrow
;; operator). Not that rule-if is different from the programming if since it returns true if
;; the condition is false.


;;; My idea is basically the same as poste by NPException

(defn rule-and
  ([] (constantly true))
  ([rule] rule)
  ([rule1 rule2] (fn [& args] (and (apply rule1 args) (apply rule2 args))))
  ([rule1 rule2 & rules] (apply rule-and (rule-and rule1 rule2) rules)))

(defn rule-or
  ([] (constantly false))
  ([rule] rule)
  ([rule1 rule2] (fn [& args] (or (apply rule1 args) (apply rule2 args))))
  ([rule1 rule2 & rules] (apply rule-or (rule-or rule1 rule2) rules)))

(defn rule-not [rule] (complement rule))


(defn rule-if [rule-p rule-q]
  (rule-or (rule-not rule-p) rule-q))

(defn rule-iff [rule-p rule-q]
  (rule-and (rule-if rule-p rule-q)
            (rule-if rule-q rule-p)))









;;; I think this is the same
(defn riff [rp rq]
  (rule-or (rule-and rp rq)
           (rule-and (rule-not rp) (rule-not rq))))

(comment

A  B   IF   IFF
T  T   T    T
T  F   F    F
F  T   T    F
F  F   T    T

)


(defmacro mkrule [andor]
  `(fn rule-name#
     ([] (constantly (~andor)))
     ([rule#] rule#)
     ([rule1# rule2#] (fn [& args#] (~andor (apply rule1# args#) (apply rule2# args#))))
     ([rule1# rule2# & rules#] (apply rule-name#
                                      (rule-name# rule1# rule2#)
                                      rules#))))

(def rule-and (mkrule and))

(def rule-or (mkrule or))




