(ns bank-ocr-kata-clojure.tools
  (:require [clojure.string :as s]))

(defn strip-margin
  ([string]
   (strip-margin string "\\|"))
  ([string delimiter] (->> string
                           s/split-lines
                           (map s/triml)
                           (map #(.replaceFirst % delimiter ""))
                           (s/join "\n"))))
