(ns miner.datomic
  (:require [datomic.client.api :as d]))

;;; 08/17/23  16:24 by miner -- Datomic Local, new release
;;; https://blog.datomic.com/2023/08/datomic-local-is-released.html

;;; Datomic Local documentation
;;; https://docs.datomic.com/cloud/datomic-local.html


;;; My old code used to work with Datomic Free.  Looks like some things are changing with
;;; datomic-local
;;;[com.datomic/datomic-free "0.9.5697"  :exclusions [joda-time]]



;;; Playing around with Datomic Local

;;; sample data already loaded from ~/cljproj/datomic-data/datomic-samples/

;; for REPL convenience
#_
(require '[datomic.client.api :as d])

(def client (d/client {:server-type :datomic-local
                       :system "datomic-samples"}))


;; if you don't need durability, you can just use in-memorty
#_
(def client (d/client {:server-type :datomic-local
                       :storage-dir :mem
                       :system "ci"}))
#_
(d/list-databases client {})


(def conn (d/connect client {:db-name "movies"}))

(def db (d/db conn))

(def all-titles-q '[:find ?movie-title 
                    :where [_ :movie/title ?movie-title]])

(def all-movies-q '[:find ?e 
                    :where [?e :movie/title]])

#_
(d/q all-titles-q db)

#_
(d/q all-movies-q db)







;;; OLDER STUFF BELOW
#_
@(d/transact conn [{:db/doc "Hello world"}])

#_
(def movie-schema [{:db/ident :movie/title
                    :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one
                    :db/doc "The title of the movie"}

                   {:db/ident :movie/genre
                    :db/valueType :db.type/string
                    :db/cardinality :db.cardinality/one
                    :db/doc "The genre of the movie"}

                   {:db/ident :movie/release-year
                    :db/valueType :db.type/long
                    :db/cardinality :db.cardinality/one
                    :db/doc "The year the movie was released in theaters"}])


#_
@(d/transact conn movie-schema)

#_
(def first-movies [{:movie/title "The Goonies"
                    :movie/genre "action/adventure"
                    :movie/release-year 1985}
                   {:movie/title "Commando"
                    :movie/genre "action/adventure"
                    :movie/release-year 1985}
                   {:movie/title "Repo Man"
                    :movie/genre "punk dystopia"
                    :movie/release-year 1984}])


#_
@(d/transact conn first-movies)
