(ns miner.iteration)

;; CLJ-2555 proposed by Ghadi

;; SEM: (snarky) rebirth of OOP!  Methods as keyed functions.

;; Serious comments:  unlike (iterate f init) where init appears first in result.
;;
;; Suggestion: instead of ":initk" use ":seed" -- more suggestive that (step! seed) is
;; called for first result.
;;
;; Suggestion: avoid term "continuation" as this is not like a Scheme computational continuation.
;;
;; Suggestion: instead of "k" terminology, use "state".  Alerts user that it could be
;; ephemeral or even mutable! Similarly, :statef instead of :kf.
;;
;; Suggestion: reverse sense of :some? and use :terminate? or :end? or something else for
;;short-circuiting default nil?


(comment

(take 10 (iterate inc 0))
(0 1 2 3 4 5 6 7 8 9)

(take 10 (iteration inc :initk 0))
(1 2 3 4 5 6 7 8 9 10)

(take 10 (iteration inc :initk -1))
(0 1 2 3 4 5 6 7 8 9)

(map str (take 10 (iterate #(.plusDays % 1) (java.time.LocalDate/now))))


(take 10 (iteration (fnil inc 0) :vf -))
(-1 -2 -3 -4 -5 -6 -7 -8 -9 -10)

)


(defn iteration
  "creates a seqable/reducible given step!,
   a function of some (opaque continuation data) k

   step! - fn of k/nil to (opaque) 'ret'

   :some? - fn of ret -> truthy, indicating there is a value
           will not call vf/kf nor continue when false
   :vf - fn of ret -> v, the values produced by the iteration
   :kf - fn of ret -> next-k or nil (will not continue)
   :initk - the first value passed to step!

   vf, kf default to identity, some? defaults to some?, initk defaults to nil

   it is presumed that step! with non-initk is unreproducible/non-idempotent
   if step! with initk is unreproducible, it is on the consumer to not consume twice"
  {:added "1.11"}
  [step! & {:keys [vf kf some? initk]
            :or {vf identity
                 kf identity
                 some? some?
                 initk nil}}]
  (reify
   clojure.lang.Seqable
   (seq [_]
        ((fn next [ret]
           (when (some? ret)
             (cons (vf ret)
                   (when-some [k (kf ret)]
                     (lazy-seq (next (step! k)))))))
         (step! initk)))
   clojure.lang.IReduceInit
   (reduce [_ rf init]
           (loop [acc init
                  ret (step! initk)]
             (if (some? ret)
               (let [acc (rf acc (vf ret))]
                 (if (reduced? acc)
                   @acc
                   (if-some [k (kf ret)]
                     (recur acc (step! k))
                     acc)))
               acc)))))

;; SEM: how about reduce-kv support

(defn iter
  "Creates a seqable/reducible given step!,
   a function of some (opaque) state

   step! - fn of state to (next) state

   :terminate? - fn of state -> truthy, indicating the iteration should short-circuit,
           returnin its current accumulation;  will not call vf/sf nor continue when truthy.
   :vf - fn of state -> v, the values produced by the iteration
   :sf - fn of state -> next-state or nil (will not continue)
   :state - the first value passed to step!

   vf, sf default to identity, terminate? defaults to nil?, state defaults to nil

   It is presumed that result of step! is unreproducible/non-idempotent so the
   consumer should be wary of caching the internal state.  The vf function should produce
   immutable values."

  {:added "1.11"}
  [step! & {:keys [vf sf some? state]
            :or {vf identity
                 sf identity
                 terminate? nil?
                 state nil}}]
  (reify
    
   clojure.lang.Seqable
   (seq [_]
        ((fn next [ret]
           (when-not (terminate? ret)
             (cons (vf ret)
                   (when-some [state (sf ret)]
                     (lazy-seq (next (step! state)))))))
         (step! state)))
    
   clojure.lang.IReduceInit
   (reduce [_ rf init]
           (loop [acc init
                  ret (step! state)]
             (if-not (terminate? ret)
               (let [acc (rf acc (vf ret))]
                 (if (reduced? acc)
                   @acc
                   (if-some [k (sf ret)]
                     (recur acc (step! k))
                     acc)))
               acc)))

    ;; SEM added
    clojure.lang.IKVReduce
    (kvreduce [_ rf3 init]
           (loop [acc init
                  i 0
                  ret (step! state)]
             (if-not (termnate? ret)
               (let [acc (rf3 acc i (vf ret))]
                 (if (reduced? acc)
                   @acc
                   (if-some [state (sf ret)]
                     (recur acc (unchecked-inc i) (step! state))
                     acc)))
               acc)))))

