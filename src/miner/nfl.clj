(ns miner.nfl
  (:require [clojure.data.csv :as csv])
  (:require [clojure.java.io :as io]))

;;; A scorigami is a final score that has never happened before in NFL history.  Jon Bois
;;; popularized the concept.

;;; https://en.wikipedia.org/wiki/Scorigami

;;; Unpdated chart on the web:
;;; https://nflscorigami.com/

;;; Note: winner score first.  No distinction of home/away.

(defn winner-score [x]
  (parse-long (nth x 2)))

(defn loser-score [x]
  (parse-long (nth x 3)))

(def extract-score (juxt winner-score loser-score))
  
;;; NFL data source
;; https://www.pro-football-reference.com/boxscores/game-scores.htm

;;; 09/10/24  14:38 by miner -- files downloaded for testing.

(def nfl-scores-file "nfl-scores.csv")
;; this file is only most recent example of that score, not all games.
;; ordered by highest frequency ("count") column

(def nfl-missing-scores-file "nfl-missing-scores.csv")
;; I assume there's some implied max score.  Apparently, 70.


;;; doall to force while file is open
;;; skip header line
(def nfl-data (with-open [reader (io/reader (io/resource nfl-scores-file))]
                (doall (rest (csv/read-csv reader)))))

;; pd is the point difference, not useful
;; count might be useful
(def nfl-keys [:rk :score :ptsw :ptsl :pttot :pd :count :xxx :last-game])

(def nfl-scores (into (sorted-set) (map extract-score) nfl-data))


(def example-data
  [["1" "20-17" "20" "17" "37" "3" "291" "all games"
    "Miami Dolphins vs. Jacksonville Jaguars September 8 2024"]
   ["2" "27-24" "27" "24" "51" "3" "234" "all games"
    "Kansas City Chiefs vs. Buffalo Bills January 21 2024"]
   ["1085" "38-12" "38" "12" "50" "26" "1" "all games"
    "Washington Redskins vs. New York Giants October 11 1987"]])



  
(def nfl-missing-data (with-open [reader (io/reader (io/resource nfl-missing-scores-file))]
                        (doall (rest (csv/read-csv reader)))))

(def nfl-missing-scores (into (sorted-set) (map extract-score) nfl-missing-data))



               
