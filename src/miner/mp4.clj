(ns miner.mp4
  (:require [me.raynes.fs :as fs]
            [clojure.string :as str]
            [clojure.java.io :as io]))


(def ^:dynamic *miro-dir* (io/file "/Volumes/Seagate/Miro"))

(defn video? [f]
  (case (fs/extension f)
    (".mp4" ".mkv" ".avi") true
    false))

(defn episode? [str]
  (boolean (or (re-matches #"[Ss]?\d+[Eex]\d+" str)
               (re-matches #"\d\d\d" str))))

(defn year? [str]
  (boolean (re-matches #"(19|20)\d\d" str)))

(defn num-char? [ch]
  (<= (long \0) (long ch) (long \9)))

(defn zero-char? [ch]
  (= \0 ch))

(defn clean-episode [str]
  (str/join (drop-while zero-char? (filter num-char? str))))

(defn clean-name [basename]
  (let [segments (str/split basename #"[.]")
        ep (first (filter episode? segments))]
    (when ep
      (str (str/join "." (remove year? (take-while (complement episode?) segments)))
           "."
           (clean-episode ep)))))

(defn clean-file-path [dest f]
  (when (video? f)
    (let [base (fs/base-name f)
          show (clean-name base)]
      (io/file dest (str show (fs/extension f))))))

(defn clean-up-regex [dest regex]
  (doseq [mp4 (fs/find-files dest regex)]
    (let [clean (clean-file-path dest mp4)]
      (if-not (fs/exists? clean)
        ;;(println "will rename\n  " (str mp4) "\n  " (str clean))
        (fs/rename mp4 clean)
        (println "Error: file already exists " clean)))))

(defn delete-subdirs [dest]
  (doseq [dir (drop 1 (fs/find-files* dest fs/directory?))]
    (fs/delete-dir dir)))

(defn clean-up [dest]
  (clean-up-regex dest #".*[.]mp4")
  (clean-up-regex dest #".*[.]mkv")
  (clean-up-regex dest #".*[.]avi")
  (delete-subdirs dest))




        

