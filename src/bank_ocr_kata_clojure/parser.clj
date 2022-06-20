(ns bank-ocr-kata-clojure.parser
  (:require [bank-ocr-kata-clojure.tools :as t]
            [clojure.set :as set]
            [clojure.string :as s]))

(def zero
  (t/strip-margin "| _ 
                   || |
                   ||_|
                   "))

(def one
  (t/strip-margin "|   
                   |  |
                   |  |
                   "))

(def two
  (t/strip-margin "| _ 
                   | _|
                   ||_ 
                   "))

(def three
  (t/strip-margin "| _ 
                   | _|
                   | _|
                   "))

(def four
  (t/strip-margin "|   
                   ||_|
                   |  |
                   "))

(def five
  (t/strip-margin "| _ 
                   ||_ 
                   | _|
                   "))

(def six
  (t/strip-margin "| _ 
                   ||_ 
                   ||_|
                   "))

(def seven
  (t/strip-margin "| _ 
                   |  |
                   |  |
                   "))

(def eight
  (t/strip-margin "| _ 
                   ||_|
                   ||_|
                   "))

(def nine
  (t/strip-margin "| _ 
                   ||_|
                   | _|
                   "))

(def to-char {zero  \0
              one   \1
              two   \2
              three \3
              four  \4
              five  \5
              six   \6
              seven \7
              eight \8
              nine  \9})

(def to-glyph (set/map-invert to-char))

(defn extract-char [chars pos]
  (let [extraction (apply str (concat (get-in chars [0 pos])
                                 [\newline]
                                 (get-in chars [1 pos])
                                 [\newline]
                                 (get-in chars [2 pos])
                                 [\newline]))]
     
    (get to-char extraction \?)))

(defn to-digits [glyph]
  (let [glyph-lines (->> (s/split-lines glyph)
                         (map (comp vec (partial partition 3)))
                         vec)]
    (.toString
      (reduce (fn [sb pos]
                (doto sb (.append (extract-char glyph-lines pos))))
              (StringBuffer.)
              (range 9)))))

