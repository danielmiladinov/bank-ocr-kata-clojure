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

(defn lines-of [glyph]
  (->> (s/split-lines glyph)
       (map (comp vec (partial partition 3)))
       vec))

(defn glyphs-of [lines]
  (str (->> lines
            (map (comp (partial apply str)
                       flatten))
            (interpose \newline)
            (apply str))
       \newline))

(defn length-of [glyphs]
  (quot (->> (s/split-lines glyphs)
             (map count)
             (apply max))
        3))

(defn glyph-at [glyphs pos]
  (let [lines (lines-of glyphs)]
    (apply str (concat (get-in lines [0 pos])
                       [\newline]
                       (get-in lines [1 pos])
                       [\newline]
                       (get-in lines [2 pos])
                       [\newline]))))

(defn extract-char [glyphs pos]
  (get to-char (glyph-at glyphs pos) \?))

(defn to-digits [glyph]
  (->> (range 9)
       (reduce (fn [sb pos]
                 (doto sb (.append (extract-char glyph pos))))
               (StringBuffer.))
       (.toString)))

(defn to-glyphs [digits]
  (->> digits
       str
       (map (comp lines-of to-glyph))
       (reduce (fn [[acc-line-1
                     acc-line-2
                     acc-line-3]
                    [glyph-line-1
                     glyph-line-2
                     glyph-line-3]]
                 [(concat acc-line-1 glyph-line-1)
                  (concat acc-line-2 glyph-line-2)
                  (concat acc-line-3 glyph-line-3)])
               [[] [] []])
       glyphs-of))

(defn replace-in-glyphs [glyphs pos replacement]
  (let [glyphs-lines      (lines-of glyphs)
        replacement-lines (lines-of replacement)
        replaced-lines    (-> glyphs-lines
                              (assoc-in [0 pos] (get-in replacement-lines [0 0]))
                              (assoc-in [1 pos] (get-in replacement-lines [1 0]))
                              (assoc-in [2 pos] (get-in replacement-lines [2 0])))]
    (glyphs-of replaced-lines)))
