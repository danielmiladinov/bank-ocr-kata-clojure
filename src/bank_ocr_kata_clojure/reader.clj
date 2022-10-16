(ns bank-ocr-kata-clojure.reader
  (:require [clojure.java.io :as io]))

(defn read-account-number-glyphs
  "Accepts a file path and returns a sequence the account number lines it was able to read"
  [file-path]
  (with-open [reader (io/reader file-path)]
    (->> (into [] (line-seq reader))
         (partition 4)
         (map (comp (partial reduce str)
                    (partial interpose "\n"))))))
