(ns miner.crc
  (:require [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [digest :as dig])  
  (:import (java.io FileInputStream BufferedInputStream)
           java.nio.ByteBuffer
           java.nio.channels.FileChannel
           (java.util.zip CheckedInputStream Checksum Adler32 CRC32)))

(set! *warn-on-reflection* true)

(def ^:const bsize (* 4 1024))


;; rough results on lore
;; md5  100 ms
;; sha1 200 ms
;; crc   13.5 ms
;; adler  5.7 ms


;; a test file that's 9.5 MB
(def lore (io/as-file "/Users/miner/Downloads/lisp-lore-guide-to-lispm.pdf"))

;; a small test file, 160 KB
(def mcc (io/as-file "/Users/miner/Downloads/lisp-history-mccarthy.ps"))

(def aaa (Adler32.))
(def ccc (CRC32.))
(def bbb (byte-array bsize))


(defn sha1 [file-or-url]
  ;; missing file should return nil
  (let [file (io/as-file file-or-url)]
    (when (fs/readable? file)
      (dig/sha-1 file))))

(defn md5 [file-or-url]
  ;; missing file should return nil
  (let [file (io/as-file file-or-url)]
    (when (fs/readable? file)
      (dig/md5 file))))

;            (java.util.zip CheckedInputStream Checksum Adler32 CRC32)))
;; if you pass in a chksum object, you should .reset it before re-using unless you want
;; a running total checksum
;; default Adler32
(defn checksum
  ([file] (checksum file (byte-array bsize)))
  ([file buffer] (checksum file buffer (Adler32.)))
  ([file buffer ^Checksum chksum]
     (let [file (io/as-file file)]
       (when (fs/readable? file)
         (with-open [input (FileInputStream. file)
                     cis (CheckedInputStream. input chksum)]
          ;; read whole file to calculate checksum
          (while (not (neg? (.read cis buffer))))
          (.getValue (.getChecksum cis)))))))

(defn adler32
  ([file] (checksum file (byte-array bsize) (Adler32.)))
  ([file buffer] (checksum file buffer (Adler32.))))

(defn crc32
  ([file] (checksum file (byte-array bsize) (CRC32.)))
  ([file buffer] (checksum file buffer (CRC32.))))




;; very slow
(defn crc-slow [file]
  (let [file (io/as-file file)]
    (when (fs/readable? file)
      (let [crc ^CRC (CRC32.)]
        (with-open [input (io/input-stream file)]
          (loop [v ^int (.read input)]
            (if (== v -1)
              (.getValue crc)
              (do (.update crc v)
                  (recur ^int (.read input))))))))))



;; Maybe bad examples from the web
;; http://examples.javacodegeeks.com/core-java/security/calculate-the-crc-sum-of-a-file/

;; nio version but it's a bit slower than the regular checksum
;; look for reflection
(defn nchecksum
  ([file] (nchecksum file (byte-array bsize)))
  ([file buffer] (nchecksum file buffer (CRC32.)))
  ([file buffer ^Checksum chksum]
     (let [file (io/as-file file)]
       (when (fs/readable? file)
         (with-open [input (FileInputStream. file)
                     chan (.getChannel input)]
           (let [bb (ByteBuffer/wrap buffer)]
             (loop [cnt ^int (.read chan bb)]
               (if (== cnt -1)
                 (.getValue chksum)
                 (do (.update chksum buffer 0 cnt)
                     (.clear bb)
                     (recur ^int (.read chan bb)))))))))))

