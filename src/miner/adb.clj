(ns miner.adb
  (:require [clojure.java.io :as io]))


;; inspired by Apropos Cast 1/18/19
;; https://gist.github.com/apropos-cast/abf8b64452ae0ae56cafa333c1eaf4e8

;; SEM -- I'm thinking an agent would be good to serialize the file writes.  We would
;; usually not want to wait for the file I/O before we get our db atom updated.  This lets
;; the write happen asynchronously on the agent's thread.  The risk is that a write to disk
;; might fail somewhat later even though the atom is updated.  If you crash, you might get
;; an older state when you restart.


;; just an experiment, not a real thing.  Needs serious multi-threaded testing.  Using Refs
;; with dosync would be the right way to do transactions with multiple resources.


;; SEM private
(defn db-atom [data filename]
  (let [db (atom data)
        durability (agent 0)
        write-db (fn [state dbdata]
                   (spit filename (pr-str dbdata))
                   (inc state))]
    (add-watch db ::saver (fn [_ _ old data]
                            (when (not= old data)
                              (send-off durability write-db data))))))

  

(defn drop-db! [filename]
  (io/delete-file filename true))


(defn load-db [filename]
  (db-atom (clojure.edn/read-string (slurp filename)) filename))


(defn create-db!
  ([] (create-db! "dbsave.edn"))

  ([filename] (create-db! filename {}))

  ([filename data]
   (when (.exists (io/file filename))
     (throw (ex-info (str "File " filename " already exists") {:filename filename})))
   (spit filename (pr-str data))
   (db-atom data filename) ))


