(ns miner.maxsubarray
  (:require [clojure.edn :as edn]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.rfs :as xrf]
            [clojure.java.io :as io]
            [clojure.test.check :as ct]
            [clojure.test.check.generators :as cg]
            [clojure.core.reducers :as r]
            [clojure.core.async :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))


;; programmer challenge
;; https://www.hackerrank.com/challenges/maxsubarray

;; I did a lot of work on the wrong concept before I figured out the correct path.
;; This problem can have very large input (100K ints) so it's not practical to use Clojure
;; sequences.  I had some semi-clever ideas that worked for small samples, but timed out for
;; large tests.  Also, I missed some basic things such as it's possible that all the items
;; are negative.  (I was implicitly assuming some positive value.)  I was looking for
;; partitions of pos vs. neg so that they could be recombined.  But that approach assumes
;; you can manipulate the whole collection at once.  Not practical for a big list over 100K
;; elements.  Finally, I had to step back and figure out a step-wise approach that kept the
;; appropriate running totals for each input and discarded history.
;;
;; I still have the feeling that there should be a better way to separate the i/o and
;; concentrate on the logic.  It feels wrong calling (read) from the middle of a nice
;; function (making it impure).  Perhaps, there's a way to use async to essentially stream
;; the values from a file????
;;
;; I notice that my loop bindings are essentially the state -- with several interrelated
;; values.  How could this be abstracted so that you could reduce/transduce over the
;; inputs.  The inputs would have to stream into the calculation.  The result would have to
;; be multiple values (map or vector) corresponding to the loop bindings.


(def sample-input [2
                   4
                   1 2 3 4
                   6
                   2 -1 2 3 4 -5])


;; makes slightly faster fn than using generic comp
(defn comp1
  "Like comp but specialized for single-arg functions"
  ([] identity)
  ([f] f)
  ([f g] (fn [x] (f (g x))))
  ([f g h] (fn [x] (f (g (h x)))))
  ([f g h & more] (apply comp1 (comp1 f g h) more)))

;; Not sure about rf-comp
;; maybe better to use regular comp and push the x onto the state first
;; kind of strange to pass the x arg to each one separately
;; Transducers already do something like this???

;; like comp but specialized for 2-arg `reducing` fn
(defn rf-comp
  ([] conj)
  ([f] f)
  ([f g] (fn [r x] (f (g r x) x)))
  ([f g h] (fn [r x] (f (g (h r x) x) x)))
  ([f g h & more] (apply rf-comp (rf-comp f g h) more)))


;; SEM: Need to try clojure.core/reducers for speed
;; but order is important so reducers can't parallelize

;; really big input takes too long if you create vectors and partitions
;; could be 100K longs in a case
;; need to process more link FSA
;; stepwise calculations, one item at a time


(defn read-case-WORKS []
  (let [cnt (edn/read)]
    (loop [i cnt
           running Long/MIN_VALUE
           contig Long/MIN_VALUE
           anysub Long/MIN_VALUE ]
      (if (zero? i)
        [contig anysub]
        (let [x (edn/read)
              running (if (neg? running) x (+ running x))
              anysub (cond (neg? anysub) (max anysub x)
                           (neg? x) anysub
                           :else (+ anysub x))
              contig (max contig running)]
          (recur (dec i) running contig anysub))))))
             

(defn run-maxsub-WORKS []
  (dotimes [_ (edn/read)]
    (let [[contiguous total] (read-case-WORKS)]
      (println contiguous total))))



;;;; Playing with other ideas
;; seems like async and transduce ought to work for this

;; state: contig anysub running -- "car"
;; final result: contig anysub -- (pop car)

;; Can we take that apart?
;;  running depends on running and x
;;  anysub depends on anysub and x
;;  contig depends on contig and running


;; But see step-combo for a decomplected way.
;;
;; more convenient to order the args this way so we can pop for final result
(defn step-red [[contig anysub running] x]
  (let [running (if (neg? running) x (+ running x))
        anysub (cond
                   (neg? anysub) (max anysub x)
                   (neg? x) anysub
                   :else (+ anysub x))
        contig (max contig running)]
    [contig anysub running]))



(defn step-running [running x]
  (if (neg? running) x (+ running x)))

(defn step-anysub [anysub x]
  (cond (neg? anysub) (max anysub x)
      (neg? x) anysub
      :else (+ anysub x)))

(defn step-contig [contig running]
  (max contig running))

(defn step-combo [[contig anysub running] x]
  (let [running (step-running running x)
        anysub (step-anysub anysub x)
        contig (step-contig contig running)]
    [contig anysub running]))




;; could re-consider step-contig as dependent on x and state [contig running]

(defn step-running-contig [[running contig] x]
  (let [running (if (neg? running) x (+ running x))
        contig (max contig running)]
    [running contig]))

(defn step-arc [[anysub rc] x]
  (let [rc (step-running-contig rc x)
        anysub (step-anysub anysub x)]
    [anysub rc]))

;; a bit slower, probably due to extra destructuring
(defn arc-case [cnt lazyreads]
  (let [[anysub rc] (reduce step-arc
                                    ;; init: [anysub [running contig]]
                                    [Long/MIN_VALUE [Long/MIN_VALUE Long/MIN_VALUE]]
                                    lazyreads)]
    [(peek rc) anysub]))


;; slower to run reduce twice, but simpler to understand
(defn r2-case [cnt lazyreads]
  (vector
   (peek (reduce step-running-contig [Long/MIN_VALUE Long/MIN_VALUE] lazyreads))
   (reduce step-anysub Long/MIN_VALUE lazyreads)))





;; But how to comp that automatically???
;; Use a map for the state -- keys compose

(defn sm-running [mp x]
  (update mp :running step-running x))

(defn sm-anysub [mp x]
  (update mp :anysub step-anysub x))

(defn sm-contig [mp _]
  (update mp :contig max (:running mp)))




(defn sm1-running [mp]
  (update mp :running step-running (:x mp)))

(defn sm1-anysub [mp]
  (update mp :anysub step-anysub (:x mp)))

(defn sm1-contig [mp]
  (update mp :contig max (:running mp)))

(def sm1-step (fn [m x] ((comp1 sm1-anysub sm1-contig sm1-running) (assoc m :x x))))

(defn sm1-case [cnt lazyreads]
  ((juxt :contig :anysub)
        (reduce sm1-step
                {:contig Long/MIN_VALUE :anysub Long/MIN_VALUE :running Long/MIN_VALUE}
                lazyreads)))


;; using transients
(defn tm1-running [mp]
  (assoc! mp :running (step-running (:running mp) (:x mp))))

(defn tm1-anysub [mp]
  (assoc! mp :anysub (step-anysub (:anysub mp) (:x mp))))

(defn tm1-contig [mp]
  (assoc! mp :contig (max (:contig mp) (:running mp))))

(def tm1-step (fn [m x] ((comp1 tm1-anysub tm1-contig tm1-running) (assoc! m :x x))))



(defn tm1-case [cnt lazyreads]
  ((juxt :contig :anysub)
        (reduce tm1-step
                (transient {:contig Long/MIN_VALUE :anysub Long/MIN_VALUE :running Long/MIN_VALUE})
                lazyreads)))




;; m2-step is pretty fast, must be easy to optimize when all together vs. comp1
(defn m2-step [{:keys [contig anysub running]} x]
  (let [running (if (neg? running) x (+ running x))
        anysub (cond
                   (neg? anysub) (max anysub x)
                   (neg? x) anysub
                   :else (+ anysub x))
        contig (max contig running)]
    {:contig contig :anysub anysub :running running}))


(defn m2-case [cnt lazyreads]
  ((juxt :contig :anysub)
        (reduce m2-step
                {:contig Long/MIN_VALUE :anysub Long/MIN_VALUE :running Long/MIN_VALUE}
                lazyreads)))


;; RECORD is actually a bit faster than the generic map.  About same as vector approach
;; red-case.  But it was slower with the standard map coding of m2-step.  Had to try to be
;; more literal with the field access, rather than destructing the record as a map to get it
;; to run faster.

;; make a record to handle state
(defrecord CAR [^long contig ^long anysub ^long running])


(defn rec-step [^CAR car x]
  (let [running (if (neg? (:running car)) x (+ (:running car) x))
        anysub (cond
                   (neg? (:anysub car)) (max (:anysub car) x)
                   (neg? x) (:anysub car)
                   :else (+ (:anysub car) x))
        contig (max (:contig car) running)]
    (->CAR contig anysub running)))


(defn rec-case [cnt lazyreads]
  ((juxt :contig :anysub)
        (reduce rec-step
                (->CAR Long/MIN_VALUE Long/MIN_VALUE Long/MIN_VALUE)
                lazyreads)))

(defn rec1-step [^CAR car x]
  (let [{:keys [contig anysub running]} car
        running (if (neg? running) x (+ running x))
        anysub (cond
                   (neg? anysub) (max anysub x)
                   (neg? x) anysub
                   :else (+ anysub x))
        contig (max contig running)]
    (->CAR contig anysub running)))

(defn rec1-case [cnt lazyreads]
  ((juxt :contig :anysub)
        (reduce rec1-step
                (->CAR Long/MIN_VALUE Long/MIN_VALUE Long/MIN_VALUE)
                lazyreads)))

;; Faster to explicitly grab fields than to use destructuring.
;; Clojure destructuring does not optimize the single symbol source case.  See the macroxpand of
;; the `{:keys ...} foo` -- does extra work to allow a seq of kw/values, uses generic get access.

(defn rec2-step [^CAR car x]
  (let [contig (:contig car)
        anysub (:anysub car)
        running (:running car)
        running (if (neg? running) x (+ running x))
        anysub (cond
                   (neg? anysub) (max anysub x)
                   (neg? x) anysub
                   :else (+ anysub x))
        contig (max contig running)]
    (->CAR contig anysub running)))

(defn rec2-case [cnt lazyreads]
  ((juxt :contig :anysub)
        (reduce rec2-step
                (->CAR Long/MIN_VALUE Long/MIN_VALUE Long/MIN_VALUE)
                lazyreads)))

;; slightly slower to use generic get rather than kw access
(defn rec3-step [^CAR car x]
  (let [contig (get car :contig)
        anysub (get car :anysub )
        running (get car :running)
        running (if (neg? running) x (+ running x))
        anysub (cond
                   (neg? anysub) (max anysub x)
                   (neg? x) anysub
                   :else (+ anysub x))
        contig (max contig running)]
    (->CAR contig anysub running)))

(defn rec3-case [cnt lazyreads]
  ((juxt :contig :anysub)
        (reduce rec3-step
                (->CAR Long/MIN_VALUE Long/MIN_VALUE Long/MIN_VALUE)
                lazyreads)))

;; just slightly faster to use field access
(defn rec4-step [^CAR car x]
  (let [contig (.contig car)
        anysub (.anysub car)
        running (.running car)
        running (if (neg? running) x (+ running x))
        anysub (cond
                   (neg? anysub) (max anysub x)
                   (neg? x) anysub
                   :else (+ anysub x))
        contig (max contig running)]
    (->CAR contig anysub running)))


(defn rec4-case [cnt lazyreads]
  ((juxt :contig :anysub)
        (reduce rec4-step
                (->CAR Long/MIN_VALUE Long/MIN_VALUE Long/MIN_VALUE)
                lazyreads)))




(def sm-combo (rf-comp sm-contig sm-anysub sm-running))

(defn sm-case [cnt lazyreads]
  ((juxt :contig :anysub)
        (reduce sm-combo
                {:contig Long/MIN_VALUE :anysub Long/MIN_VALUE :running Long/MIN_VALUE}
                lazyreads)))



(defn step-combo [[contig anysub running] x]
  (let [running (step-running running x)
        anysub (step-anysub anysub x)
        contig (step-contig contig running)]
    [contig anysub running]))


;; step-case looks better organized for vector state
;; But map-state is easier to read and maintain, although slower.

(defn step-case [cnt lazyreads]
  (pop (reduce step-combo
          ;; init: contig anysub running 
          [Long/MIN_VALUE Long/MIN_VALUE Long/MIN_VALUE]
          lazyreads)))




(defn assoc-when [a k v] (if v (assoc a k v) a))


(defn sm-running [mp x]
  (update mp :running step-running x))

(defn sm-anysub [mp x]
  (update mp :anysub step-anysub x))

(defn sm-contig [mp _]
  (update mp :contig max (:running mp)))



(defn av-running [car x]
  (update car 2 step-running x))

(defn av-anysub [car x]
  (update car 1 step-anysub x))

(defn av-contig [car _]
  (update car 0 max (peek car)))

(def av-step (rf-comp av-contig av-anysub av-running))

(defn av-case [cnt lazyreads]
  (pop (reduce av-step [Long/MIN_VALUE Long/MIN_VALUE Long/MIN_VALUE] lazyreads)))



(defn av1-running [carx]
  (update carx 2 step-running (peek carx)))

(defn av1-anysub [carx]
  (update carx 1 step-anysub (peek carx)))

(defn av1-contig [carx]
  (update carx 0 max (carx 2)))

(def av1-step (fn [av x] ((comp1 av1-contig av1-anysub av1-running) (conj av x))))

(defn av1-case [cnt lazyreads]
  (take 2 (reduce av1-step [Long/MIN_VALUE Long/MIN_VALUE Long/MIN_VALUE] lazyreads)))



(defn read-case [in cnt]
    (loop [i cnt
           car [Long/MIN_VALUE Long/MIN_VALUE Long/MIN_VALUE ]]
      (if (zero? i)
        (pop car)
        (let [x (edn/read in)
              car (step-red car x)]
          (recur (dec i) car)))))


;; filename = msa-test0.txt or msa-test1.txt
(defn run-max [filename]
  (with-open [in (java.io.PushbackReader. (io/reader (io/resource filename)))]
      (dotimes [_ (edn/read in)]
        (let [[contiguous total] (read-case in (edn/read in))]
          (println contiguous total)))))


;; Doesn't seem to be any advantage to the transducers here.  Need the whole state to
;; process in order so all the action is in the reducing-fn, not the transducer.

;; trans and red cases are basically the same, and perform the same
;; important that the seqs are lazy since they might be 100K items

(defn trans-case [cnt lazyreads]
  (transduce conj
             (completing step-red pop)
             ;; init: contig anysub running 
             [Long/MIN_VALUE Long/MIN_VALUE Long/MIN_VALUE]
             lazyreads))


(defn red-case [cnt lazyreads]
  (pop (reduce step-red
          ;; init: contig anysub running 
          [Long/MIN_VALUE Long/MIN_VALUE Long/MIN_VALUE]
          lazyreads)))

;; all the i/o and control is in here
;; lazy reads
;; the case-fn are either trans-case or red-case (not much diff)

(defn run-cases
  ([] (run-cases red-case))
  ([case-fn] (run-cases case-fn "msa-test1.txt"))
  ([case-fn filename]
   (println (str case-fn) "-" filename)
   (with-open [in (java.io.PushbackReader. (io/reader (io/resource filename)))]
     (dotimes [_ (edn/read in)]
       (let [cnt (edn/read in)
             [contiguous total] (case-fn cnt (repeatedly cnt #(edn/read in)))]
         (println contiguous total))))))


;; But for the big picture, the reduce and trans solutions are depending on the lazy results
;; of repeatedly read-ing, which has some overhead compared to the faster, run-max that
;; reads explicitly.  However the reduce solution looks nice, and the logic is detangled
;; from the i/o and control code.




;; trans is slightly faster than reader, much faster than go
(defn test-reader [filename]
  (with-open [in (java.io.PushbackReader. (io/reader (io/resource filename)))]
    (reduce + 0 (take-while identity (repeatedly #(edn/read {:eof nil} in))))))

(defn test-trans [filename]
  (with-open [in (java.io.PushbackReader. (io/reader (io/resource filename)))]
    (transduce (take-while identity) + 0 (repeatedly #(edn/read {:eof nil} in)))))




(defn test-go [filename]
  (let [cin (chan)
        rdr (go
              (with-open [fstream (java.io.PushbackReader. (io/reader (io/resource filename)))]
                (loop []
                  (if-let [x (edn/read {:eof false} fstream)]
                    (do (>! cin x)
                        (recur))
                    (close! cin)))))
        prntr (go (loop [sum 0]
                    (if-let [x (<! cin)]
                      (recur (+ sum x))
                      sum))) ]

    (<!! rdr)
    (<!! prntr)))


           
    


;;; converting data to EDN should be faster than basic text
;; msa-test1.edn is in Clojure vector notation

(defn read-msa [filename]
  (with-open [fstream (java.io.PushbackReader. (io/reader (io/resource filename)))]
    (edn/read fstream)))

(comment
  (def msa10 (read-msa "msa-test1.edn"))
  )

;; nearly as fast as run-max
(defn run-edn
  ([] (run-edn "msa-test1.edn"))
  ([filename]
   (doseq [[contig anysub] (map #(red-case (count %) %) (read-msa filename))]
     (println contig anysub))))

(defn run-edn-cases
  ([] (run-edn-cases red-case))
  ([case-fn]
   (doseq [[contig anysub] (map #(case-fn (count %) %) (read-msa "msa-test1.edn"))]
     (println contig anysub))))

(defn test-edn-cases
  ([] (test-edn-cases red-case))
  ([case-fn]
   (mapcat #(case-fn (count %) %) (read-msa "msa-test1.edn"))))



;; New idea;  how easy is it to "merge" mutliple calculations across the reduce.  Doing
;; anysub and contig right now.  What if you wanted more state?  Like contig with starting
;; index.
;;
;; Can you generalize this with transducers.  Want to comp some independent things, but they
;; seem to share inputs ("running" in red-case).  Maybe running isn't needed.  Or maybe
;; red-case should be decomposed.  Calc running first then pipe through to multiple anysub
;; and contig (+ index???)


;; SEM: maybe it's better to simply run two reductions rather than try to combine rfs

