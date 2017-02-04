(ns miner.maxsubarray
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
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
              anysub (cond
                         (neg? anysub) (max anysub x)
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

;; more convenient to order the args this way so we can pop for final result
(defn step-red [[contig anysub running] x]
  (let [running (if (neg? running) x (+ running x))
        anysub (cond
                   (neg? anysub) (max anysub x)
                   (neg? x) anysub
                   :else (+ anysub x))
        contig (max contig running)]
    [contig anysub running]))


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



