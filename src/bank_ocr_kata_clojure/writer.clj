(ns bank-ocr-kata-clojure.writer
  (:require [clojure.java.io :as io])
  (:import (java.io Writer)))

(defn- write! [^Writer w ^String thing]
  (.write w thing))

(defn write-account-number-digits
  "Accepts a file path and sequence of account numbers potentially annotated with status, one per line."
  [file-path account-numbers-with-status]
  (with-open [w (io/writer file-path)]
    (doseq [^String an account-numbers-with-status]
      (write! w an)
      (write! w (System/lineSeparator)))))
