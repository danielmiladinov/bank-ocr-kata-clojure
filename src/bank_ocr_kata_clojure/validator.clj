(ns bank-ocr-kata-clojure.validator)

(defn- to-int [^Character c]
  (try
    (Integer/valueOf (.toString c))
    (catch NumberFormatException _
      0)))

(defn legible? [^String account-number]
  (every? #(Character/isDigit ^Character %)
          account-number))

(defn valid? [^String account-number]
  (and (string? account-number)
       (= 9 (count account-number))
       (= 0 (let [[d9 d8 d7 d6 d5 d4 d3 d2 d1] account-number]
              (mod (+ (to-int d1)
                      (* 2 (to-int d2))
                      (* 3 (to-int d3))
                      (* 4 (to-int d4))
                      (* 5 (to-int d5))
                      (* 6 (to-int d6))
                      (* 7 (to-int d7))
                      (* 8 (to-int d8))
                      (* 9 (to-int d9)))
                   11)))))

(defn validate
  "Accepts a parsed account number for input, possibly appending a status if there was a problem with the number.
   If the account number has a wrong checksum, or was illegible,
   it will be followed by an additional status string:
     - “ERR”: wrong checksum
     - “ILL”: illegible number"
  [^String account-number]
  (cond
    ((complement legible?) account-number) (str account-number " ILL")
    ((complement valid?) account-number) (str account-number " ERR")
    :else account-number))
