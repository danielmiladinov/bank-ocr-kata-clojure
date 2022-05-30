(ns bank-ocr-kata-clojure.story-01-test
  "You work for a bank, which has recently purchased an ingenious machine
  to assist in reading letters and faxes sent in by branch offices.

  The machine scans the paper documents, and produces a file with a number of entries
  which each look like this:

      _  _     _  _  _  _  _
    | _| _||_||_ |_   ||_||_|
    ||_  _|  | _||_|  ||_| _|

  Each entry is 4 lines long, and each line has 27 characters.
  The first 3 lines of each entry contain an account number written using pipes and underscores,
  and the fourth line is blank.

  Each account number should have 9 digits, all of which should be in the range 0-9.
  A normal file contains around 500 entries.

  Your first task is to write a program that can take this file and parse it into actual account numbers."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as s]
            [bank-ocr-kata-clojure.reader :as r]
            [bank-ocr-kata-clojure.parser :as p]
            [bank-ocr-kata-clojure.tools :as t]))

(deftest to-digits
  (is (= "000000000"
         (p/to-digits (t/strip-margin
                        "| _  _  _  _  _  _  _  _  _ 
                         || || || || || || || || || |
                         ||_||_||_||_||_||_||_||_||_|
                         "))))
  (is (= "111111111"
         (p/to-digits (t/strip-margin
                        "|                           
                         |  |  |  |  |  |  |  |  |  |
                         |  |  |  |  |  |  |  |  |  |
                         "))))
  (is (= "222222222"
         (p/to-digits (t/strip-margin
                        "| _  _  _  _  _  _  _  _  _ 
                         | _| _| _| _| _| _| _| _| _|
                         ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
                         "))))
  (is (= "333333333"
         (p/to-digits (t/strip-margin
                        "| _  _  _  _  _  _  _  _  _ 
                         | _| _| _| _| _| _| _| _| _|
                         | _| _| _| _| _| _| _| _| _|
                         "))))
  (is (= "444444444"
         (p/to-digits (t/strip-margin
                        "|                           
                         ||_||_||_||_||_||_||_||_||_|
                         |  |  |  |  |  |  |  |  |  |
                         "))))
  (is (= "555555555"
         (p/to-digits (t/strip-margin
                        "| _  _  _  _  _  _  _  _  _ 
                         ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
                         | _| _| _| _| _| _| _| _| _|
                         "))))
  (is (= "666666666"
         (p/to-digits (t/strip-margin
                        "| _  _  _  _  _  _  _  _  _ 
                         ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
                         ||_||_||_||_||_||_||_||_||_|
                         "))))
  (is (= "777777777"
         (p/to-digits (t/strip-margin
                        "| _  _  _  _  _  _  _  _  _ 
                         |  |  |  |  |  |  |  |  |  |
                         |  |  |  |  |  |  |  |  |  |
                         "))))
  (is (= "888888888"
         (p/to-digits (t/strip-margin
                        "| _  _  _  _  _  _  _  _  _ 
                         ||_||_||_||_||_||_||_||_||_|
                         ||_||_||_||_||_||_||_||_||_|
                         "))))
  (is (= "999999999"
         (p/to-digits (t/strip-margin
                        "| _  _  _  _  _  _  _  _  _ 
                         ||_||_||_||_||_||_||_||_||_|
                         | _| _| _| _| _| _| _| _| _|
                         "))))
  (is (= "123456789"
         (p/to-digits (t/strip-margin
                        "|    _  _     _  _  _  _  _ 
                         |  | _| _||_||_ |_   ||_||_|
                         |  ||_  _|  | _||_|  ||_| _|
                         ")))))

(deftest read-account-number-lines
  (testing "read one account number from a single file"
    (is (= [(t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               || || || || || || || || || |
               ||_||_||_||_||_||_||_||_||_|
               ")]
           (r/read-account-number-glyphs "test-resources/reader/000000000.txt")))
    (is (= [(t/strip-margin
              "|                           
               |  |  |  |  |  |  |  |  |  |
               |  |  |  |  |  |  |  |  |  |
               ")]
           (r/read-account-number-glyphs "test-resources/reader/111111111.txt")))
    (is (= [(t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               | _| _| _| _| _| _| _| _| _|
               ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
               ")]
           (r/read-account-number-glyphs "test-resources/reader/222222222.txt")))
    (is (= [(t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               | _| _| _| _| _| _| _| _| _|
               | _| _| _| _| _| _| _| _| _|
               ")]
           (r/read-account-number-glyphs "test-resources/reader/333333333.txt")))
    (is (= [(t/strip-margin
              "|                           
               ||_||_||_||_||_||_||_||_||_|
               |  |  |  |  |  |  |  |  |  |
               ")]
           (r/read-account-number-glyphs "test-resources/reader/444444444.txt")))
    (is (= [(t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
               | _| _| _| _| _| _| _| _| _|
               ")]
           (r/read-account-number-glyphs "test-resources/reader/555555555.txt")))
    (is (= [(t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
               ||_||_||_||_||_||_||_||_||_|
               ")]
           (r/read-account-number-glyphs "test-resources/reader/666666666.txt")))
    (is (= [(t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               |  |  |  |  |  |  |  |  |  |
               |  |  |  |  |  |  |  |  |  |
               ")]
           (r/read-account-number-glyphs "test-resources/reader/777777777.txt")))
    (is (= [(t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               ||_||_||_||_||_||_||_||_||_|
               ||_||_||_||_||_||_||_||_||_|
               ")]
           (r/read-account-number-glyphs "test-resources/reader/888888888.txt")))
    (is (= [(t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               ||_||_||_||_||_||_||_||_||_|
               | _| _| _| _| _| _| _| _| _|
               ")]
           (r/read-account-number-glyphs "test-resources/reader/999999999.txt")))
    (is (= [(t/strip-margin
              "|    _  _     _  _  _  _  _ 
               |  | _| _||_||_ |_   ||_||_|
               |  ||_  _|  | _||_|  ||_| _|
               ")]
           (r/read-account-number-glyphs "test-resources/reader/123456789.txt"))))

  (testing "read multiple account numbers from a single file"
    (is (= [(t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               || || || || || || || || || |
               ||_||_||_||_||_||_||_||_||_|
               ")
            (t/strip-margin
              "|                           
               |  |  |  |  |  |  |  |  |  |
               |  |  |  |  |  |  |  |  |  |
               ")
            (t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               | _| _| _| _| _| _| _| _| _|
               ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
               ")
            (t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               | _| _| _| _| _| _| _| _| _|
               | _| _| _| _| _| _| _| _| _|
               ")
            (t/strip-margin
              "|                           
               ||_||_||_||_||_||_||_||_||_|
               |  |  |  |  |  |  |  |  |  |
               ")
            (t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
               | _| _| _| _| _| _| _| _| _|
               ")
            (t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
               ||_||_||_||_||_||_||_||_||_|
               ")
            (t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               |  |  |  |  |  |  |  |  |  |
               |  |  |  |  |  |  |  |  |  |
               ")
            (t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               ||_||_||_||_||_||_||_||_||_|
               ||_||_||_||_||_||_||_||_||_|
               ")
            (t/strip-margin
              "| _  _  _  _  _  _  _  _  _ 
               ||_||_||_||_||_||_||_||_||_|
               | _| _| _| _| _| _| _| _| _|
               ")]
           (r/read-account-number-glyphs "test-resources/reader/multiple-lines.txt")))))

(deftest parse-500-account-numbers
  (let [expected (->> (slurp "test-resources/story-01/500-accounts-numbers.txt")
                      (s/split-lines)
                      vec)
        actual   (->> (r/read-account-number-glyphs "test-resources/story-01/500-accounts-glyphs.txt")
                      (map p/to-digits)
                      vec)]
    (is (= expected
           actual))))
