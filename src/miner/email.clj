(ns miner.email
  (:require [miner.strgen :as sg]
            [clojure.test.check.generators :as gen]))

;;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-329-tool-cljdoc/
;;;
;;; Eric Normand writes:
;;;
;;; "The challenge this week is to create a test.check generator for email addresses. This is
;;; both an implementation challenge and a design challenge. How completely does your
;;; generator cover the space of emails? How can it use the size parameter? Does the
;;; generator capture useful edge cases? What resources (for instance, documentation) can
;;; you use to make sure you are generating correct emails?"


(def simple-user (sg/string-generator #"[A-Z]{3,}"))

(def simple-domain (sg/string-generator #"[a-z]{3,}[.](com|org|net)"))

(def simple-email (gen/let [u simple-user
                            d simple-domain]
                    (str u "@" d)))


(def email (sg/string-generator #"[A-Z0-9a-z._%+-]+@([A-Za-z0-9-]+[.])+[A-Za-z]{2,64}"))

