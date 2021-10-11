(ns miner.exor)

;; https://gist.github.com/ericnormand/b21bea7143211980f84a87fc1b5e463a

;;; The XOR Cypher is simple: To encrypt, you do a bitwise XOR of the message with a key. To
;;; decrypt it, you XOR it again with the same key.  Write a function encrypt that does an
;;; XOR encryption of a message string given a key string. If the message is longer than the
;;; key, repeat the key.  Note: This is an exercise in JVM byte manipulation.


(defn encrypt-xor [key message]
  (apply str (map (comp char bit-xor) (cycle (map byte key)) (map byte message))))

(defn utf-bytes [^String s]
  (.getBytes s "UTF-8"))

(defn encxor [key message]
  (apply str (map (comp char bit-xor) (cycle (utf-bytes key)) (utf-bytes message))))


;;; Not sure why UTF-16 didn't work.  Maybe some overflow in the bit-xor?
(def ^java.nio.charset.Charset utf8 (java.nio.charset.Charset/forName "UTF-8"))

(defn enc8 [key message]
  (let [utf-bytes (fn [^String s] (.getBytes s utf8))]
    (String. (byte-array (map bit-xor (cycle (utf-bytes key)) (utf-bytes message))))))

;;; ISSUE:  will UTF bytes necessarily be legal after bit-xor?  Some runs might not be!
;;; US-ASCII is guaranteed to work because everything fits in one byte.  The ascii subset of
;;; UTF-8 is also guaranteed to work.   So you should test some unicode chars!

(def ^java.nio.charset.Charset ascii (java.nio.charset.Charset/forName "US-ASCII"))

(defn enca [key message]
  (let [ascii-bytes (fn [^String s] (.getBytes s ascii))]
    (String. (byte-array (map bit-xor (cycle (ascii-bytes key)) (ascii-bytes message))))))

(defn smoke-xor [encrypt-xor]
  (let [test= (fn [s] (assert (= (encrypt-xor "foobar" (encrypt-xor "foobar" s)) s)))]
    (test= "foobar")
    (test= "")
    (test= "This is your penultimate test")
    (test= "This is your final test and it's a bit longer than anything else we have tried
  so far but not nearly as long as a real message might be so I hope you don't think it's
  too easy to encode or decode."))
  true)
