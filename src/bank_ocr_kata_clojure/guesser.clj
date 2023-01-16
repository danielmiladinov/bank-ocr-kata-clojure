(ns bank-ocr-kata-clojure.guesser
  (:require [bank-ocr-kata-clojure.parser :as p]
            [clojure.string :as str]))

(defn- change-char-at [row col new-char glyph]
  (assoc glyph row (apply str (assoc (vec (nth glyph row)) col new-char))))

(defn- chg-tp-md [new-char] (fn [glyph] (change-char-at 0 1 new-char glyph)))
(defn- chg-md-lt [new-char] (fn [glyph] (change-char-at 1 0 new-char glyph)))
(defn- chg-md-md [new-char] (fn [glyph] (change-char-at 1 1 new-char glyph)))
(defn- chg-md-rt [new-char] (fn [glyph] (change-char-at 1 2 new-char glyph)))
(defn- chg-bt-lt [new-char] (fn [glyph] (change-char-at 2 0 new-char glyph)))
(defn- chg-bt-md [new-char] (fn [glyph] (change-char-at 2 1 new-char glyph)))
(defn- chg-bt-rt [new-char] (fn [glyph] (change-char-at 2 2 new-char glyph)))

(defn removals [glyph]
  (->> glyph
       ((juxt (chg-tp-md \space)
              (chg-md-lt \space)
              (chg-md-md \space)
              (chg-md-rt \space)
              (chg-bt-lt \space)
              (chg-bt-md \space)
              (chg-bt-rt \space)))
       (map (comp #(str % \newline) (partial str/join \newline)))
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
       (map (comp #(str % \newline) (partial str/join \newline)))
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
            (str parsed " AMB ['" (str/join "', '" (sort guesses)) "']")))))))
