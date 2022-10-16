(ns bank-ocr-kata-clojure.guesser
  (:require [bank-ocr-kata-clojure.parser :as p]
            [clojure.string :as s]))

(defn- change-char-at [pos new-char glyph]
  (apply str (assoc (vec glyph) pos new-char)))

(defn- chg-tp-md [new-char] (fn [glyph] (change-char-at 1 new-char glyph)))
(defn- chg-md-lt [new-char] (fn [glyph] (change-char-at 4 new-char glyph)))
(defn- chg-md-md [new-char] (fn [glyph] (change-char-at 5 new-char glyph)))
(defn- chg-md-rt [new-char] (fn [glyph] (change-char-at 6 new-char glyph)))
(defn- chg-bt-lt [new-char] (fn [glyph] (change-char-at 8 new-char glyph)))
(defn- chg-bt-md [new-char] (fn [glyph] (change-char-at 9 new-char glyph)))
(defn- chg-bt-rt [new-char] (fn [glyph] (change-char-at 10 new-char glyph)))

(defn removals [glyph]
  (->> glyph
       ((juxt (chg-tp-md \space)
              (chg-md-lt \space)
              (chg-md-md \space)
              (chg-md-rt \space)
              (chg-bt-lt \space)
              (chg-bt-md \space)
              (chg-bt-rt \space)))
       set))

(defn additions [glyph]
  (->> glyph
       ((juxt (chg-tp-md \_)
              (chg-md-lt \|)
              (chg-md-md \_)
              (chg-md-rt \|)
              (chg-bt-lt \|)
              (chg-bt-md \_)
              (chg-bt-rt \|)))
       set))

(defn alternatives [glyphs]
  (reduce (fn [alts pos]
            (let [glyph   (p/glyph-at glyphs pos)
                  added   (additions glyph)
                  removed (removals glyph)
                  replace (partial p/replace-in-glyphs glyphs pos)]
              (-> alts
                  (into (map replace added))
                  (into (map replace removed)))))
          #{}
          (range (p/length-of glyphs))))

(defn with-guessing [parser validator]
  (fn parse-and-validate-and-guess [glyphs]
    (let [parsed (parser glyphs)
          valid? (validator parsed)]
      (if valid?
        parsed
        (let [alternatives (alternatives glyphs)
              parsed-alts  (map parser alternatives)
              guesses      (filterv validator parsed-alts)]
          (case (count guesses)
            0 (str parsed " ILL")
            1 (first guesses)
            (str parsed " AMB ['" (s/join "', '" (sort guesses)) "']")))))))
