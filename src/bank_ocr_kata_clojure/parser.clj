(ns bank-ocr-kata-clojure.parser
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def zero
  [" _ "
   "| |"
   "|_|"])

(def one
  ["   "
   "  |"
   "  |"])

(def two
  [" _ "
   " _|"
   "|_ "])

(def three
  [" _ "
   " _|"
   " _|"])

(def four
  ["   "
   "|_|"
   "  |"])

(def five
  [" _ "
   "|_ "
   " _|"])

(def six
  [" _ "
   "|_ "
   "|_|"])

(def seven
  [" _ "
   "  |"
   "  |"])

(def eight
  [" _ "
   "|_|"
   "|_|"])

(def nine
  [" _ "
   "|_|"
   " _|"])

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

(defn glyphs-of [lines]
  (str (->> lines
            (map (comp (partial apply str)
                       flatten))
            (interpose \newline)
            (apply str))
       \newline))

(defn length-of [glyphs]
  (quot (->> (str/split-lines glyphs)
             (map count)
             (apply max))
        3))

(defn glyph-at [glyphs pos]
  (let [start (* pos 3)
        end   (+ start 3)]
    (->> (str/split-lines glyphs)
         (filter (comp (partial <= 3) count))
         (mapv #(apply str (-> % vec (subvec start end)))))))

(defn extract-char [glyphs pos]
  (get to-char (glyph-at glyphs pos) \?))

(defn to-digits [glyphs]
  (->> (length-of glyphs)
       range
       (reduce (fn [sb pos]
                 (doto sb (.append (extract-char glyphs pos))))
               (StringBuffer.))
       (.toString)))

(defn to-glyphs [digits]
  (->> digits
       str
       (map to-glyph)
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

(defn printable [glyph-lines]
  (str (str/join \newline glyph-lines)
       \newline))

(defn- updated-glyph-line [glyph-pos glyph-line replacement-line]
  (let [[char-0 char-1 char-2] (vec replacement-line)
        char-pos (* 3 glyph-pos)]
    (-> glyph-line
        (assoc (+ 0 char-pos) char-0)
        (assoc (+ 1 char-pos) char-1)
        (assoc (+ 2 char-pos) char-2))))

(defn replace-in-glyphs [glyphs pos replacement]
  (let [[g-line-0 g-line-1 g-line-2] (mapv vec (str/split-lines glyphs))
        [r-line-0 r-line-1 r-line-2] (mapv vec (str/split-lines replacement))
        replaced-lines (->> [(updated-glyph-line pos g-line-0 r-line-0)
                             (updated-glyph-line pos g-line-1 r-line-1)
                             (updated-glyph-line pos g-line-2 r-line-2)]
                            (map str/join)
                            printable)]
    replaced-lines))
