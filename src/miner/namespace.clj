
;;; example from Clojure Programming

(ns examples.ns
  (:refer-clojure :exclude [next replace remove])
  (:require (clojure [string :as string]
                     [set :as set])
            [clojure.java.shell :as sh])
  (:use (clojure zip xml))
  (:import java.util.Date
           java.text.SimpleDateFormat
           (java.util.concurrent Executors LinkedBlockingQueue)))

;; require is assumed
;; implicit    [clojure.core :refer :all]
;; same effective initial namespace attributes are merged (like a map)
(ns examples.ns
  ;; implicit  [clojure.core :refer :all]
  clojure.core :exclude [next replace remove]
  (clojure string :as string
           set :as set)
  clojure.java.shell :as sh
  (clojure zip :refer :all xml :refer :all)
  (:import java.util.Date
           java.text.SimpleDateFormat
           (java.util.concurrent Executors LinkedBlockingQueue)))


(parens mean distribute leading term across rest elements)
:as :refer :only :exclude bind to previous ns-element

