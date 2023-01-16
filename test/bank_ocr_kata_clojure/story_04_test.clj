(ns bank-ocr-kata-clojure.story-04-test
  "It turns out that often when a number comes back as ERR or ILL it is because the scanner has failed to pick up 
  on one pipe or underscore for one of the figures.
  For example:
  
  ```text
      _  _  _  _  _  _     _ 
  |_||_|| || ||_   |  |  ||_ 
    | _||_||_||_|  |  |  | _|
  ```
  
  The 9 could be an 8 if the scanner had missed one |. Or the 0 could be an 8. Or the 1 could be a 7.
  The 5 could be a 9 or 6. So your next task is to look at numbers that have come back as ERR or ILL, 
  and try to guess what they should be, by adding or removing just one pipe or underscore.
  If there is only one possible number with a valid checksum, then use that. 
  If there are several options, the status should be AMB. 
  If you still can’t work out what it should be, the status should be reported ILL."
  (:require
    [bank-ocr-kata-clojure.core :as c]
    [bank-ocr-kata-clojure.guesser :as g]
    [bank-ocr-kata-clojure.parser :as p]
    [bank-ocr-kata-clojure.tools :as t]
    [bank-ocr-kata-clojure.validator :as v]
    [clojure.string :as str]
    [clojure.test :refer [deftest is testing]]))

(deftest removals
  (testing "zero"
    (let [expected-removals
          #{(t/strip-margin "|   
                             || |
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             |  |
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             || |
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             ||  
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             || |
                             | _|
                             ")

            (t/strip-margin "| _ 
                             || |
                             || |
                             ")

            (t/strip-margin "| _ 
                             || |
                             ||_ 
                             ")}]
      (is (= expected-removals
             (g/removals p/zero)))))

  (testing "one"
    (let [expected-removals
          #{(t/strip-margin "|   
                             |  |
                             |  |
                             ")

            (t/strip-margin "|   
                             |   
                             |  |
                             ")

            (t/strip-margin "|   
                             |  |
                             |   
                             ")}]
      (is (= expected-removals
             (g/removals p/one)))))

  (testing "two"
    (let [expected-removals
          #{(t/strip-margin "|   
                             | _|
                             ||_ 
                             ")

            (t/strip-margin "| _ 
                             |  |
                             ||_ 
                             ")

            (t/strip-margin "| _ 
                             | _ 
                             ||_ 
                             ")

            (t/strip-margin "| _ 
                             | _|
                             | _ 
                             ")

            (t/strip-margin "| _ 
                             | _|
                             ||  
                             ")

            (t/strip-margin "| _ 
                             | _|
                             ||_ 
                             ")}]
      (is (= expected-removals
             (g/removals p/two)))))

  (testing "three"
    (let [expected-removals
          #{(t/strip-margin "|   
                             | _|
                             | _|
                             ")

            (t/strip-margin "| _ 
                             | _|
                             | _|
                             ")

            (t/strip-margin "| _ 
                             |  |
                             | _|
                             ")

            (t/strip-margin "| _ 
                             | _ 
                             | _|
                             ")

            (t/strip-margin "| _ 
                             | _|
                             |  |
                             ")

            (t/strip-margin "| _ 
                             | _|
                             | _ 
                             ")}]
      (is (= expected-removals
             (g/removals p/three)))))

  (testing "four"
    (let [expected-removals
          #{(t/strip-margin "|   
                             ||_|
                             |  |
                             ")

            (t/strip-margin "|   
                             | _|
                             |  |
                             ")

            (t/strip-margin "|   
                             || |
                             |  |
                             ")

            (t/strip-margin "|   
                             ||_ 
                             |  |
                             ")

            (t/strip-margin "|   
                             ||_|
                             |   
                             ")}]
      (is (= expected-removals
             (g/removals p/four)))))

  (testing "five"
    (let [expected-removals
          #{(t/strip-margin "|   
                             ||_ 
                             | _|
                             ")

            (t/strip-margin "| _ 
                             | _ 
                             | _|
                             ")

            (t/strip-margin "| _ 
                             ||  
                             | _|
                             ")

            (t/strip-margin "| _ 
                             ||_ 
                             | _|
                             ")

            (t/strip-margin "| _ 
                             ||_ 
                             |  |
                             ")

            (t/strip-margin "| _ 
                             ||_ 
                             | _ 
                             ")}]
      (is (= expected-removals
             (g/removals p/five)))))

  (testing "six"
    (let [expected-removals
          #{(t/strip-margin "|   
                             ||_ 
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             | _ 
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             ||  
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             ||_ 
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             ||_ 
                             | _|
                             ")

            (t/strip-margin "| _ 
                             ||_ 
                             || |
                             ")

            (t/strip-margin "| _ 
                             ||_ 
                             ||_ 
                             ")}]
      (is (= expected-removals
             (g/removals p/six)))))

  (testing "seven"
    (let [expected-removals
          #{(t/strip-margin "|   
                             |  |
                             |  |
                             ")

            (t/strip-margin "| _ 
                             |  |
                             |  |
                             ")

            (t/strip-margin "| _ 
                             |   
                             |  |
                             ")

            (t/strip-margin "| _ 
                             |  |
                             |   
                             ")}]
      (is (= expected-removals
             (g/removals p/seven)))))

  (testing "eight"
    (let [expected-removals
          #{(t/strip-margin "|   
                             ||_|
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             | _|
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             || |
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             ||_ 
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             | _|
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             || |
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             ||_ 
                             ")}]
      (is (= expected-removals
             (g/removals p/eight)))))

  (testing "nine"
    (let [expected-removals
          #{(t/strip-margin "|   
                             ||_|
                             | _|
                             ")

            (t/strip-margin "| _ 
                             | _|
                             | _|
                             ")

            (t/strip-margin "| _ 
                             || |
                             | _|
                             ")

            (t/strip-margin "| _ 
                             ||_ 
                             | _|
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             | _|
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             |  |
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             | _ 
                             ")}]
      (is (= expected-removals
             (g/removals p/nine))))))

(deftest additions
  (testing "zero"
    (let [expected-additions
          #{(t/strip-margin "| _ 
                             || |
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             ||_|
                             ")}]
      (is (= expected-additions
             (g/additions p/zero)))))

  (testing "one"
    (let [expected-additions
          #{(t/strip-margin "| _ 
                             |  |
                             |  |
                             ")

            (t/strip-margin "|   
                             || |
                             |  |
                             ")

            (t/strip-margin "|   
                             | _|
                             |  |
                             ")

            (t/strip-margin "|   
                             |  |
                             |  |
                             ")

            (t/strip-margin "|   
                             |  |
                             || |
                             ")

            (t/strip-margin "|   
                             |  |
                             | _|
                             ")}]
      (is (= expected-additions
             (g/additions p/one)))))

  (testing "two"
    (let [expected-additions
          #{(t/strip-margin "| _ 
                             | _|
                             ||_ 
                             ")

            (t/strip-margin "| _ 
                             | _|
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             ||_ 
                             ")}]
      (is (= expected-additions
             (g/additions p/two)))))

  (testing "three"
    (let [expected-additions
          #{(t/strip-margin "| _ 
                             | _|
                             | _|
                             ")

            (t/strip-margin "| _ 
                             | _|
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             | _|
                             ")}]
      (is (= expected-additions
             (g/additions p/three)))))

  (testing "four"
    (let [expected-additions
          #{(t/strip-margin "|   
                             ||_|
                             |  |
                             ")

            (t/strip-margin "|   
                             ||_|
                             | _|
                             ")

            (t/strip-margin "|   
                             ||_|
                             || |
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             |  |
                             ")}]
      (is (= expected-additions
             (g/additions p/four)))))

  (testing "five"
    (let [expected-additions
          #{(t/strip-margin "| _ 
                             ||_ 
                             | _|
                             ")

            (t/strip-margin "| _ 
                             ||_ 
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             | _|
                             ")}]
      (is (= expected-additions
             (g/additions p/five)))))

  (testing "six"
    (let [expected-additions
          #{(t/strip-margin "| _ 
                             ||_ 
                             ||_|
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             ||_|
                             ")}]
      (is (= expected-additions
             (g/additions p/six)))))

  (testing "seven"
    (let [expected-additions
          #{(t/strip-margin "| _ 
                             |  |
                             |  |
                             ")

            (t/strip-margin "| _ 
                             |  |
                             | _|
                             ")

            (t/strip-margin "| _ 
                             |  |
                             || |
                             ")

            (t/strip-margin "| _ 
                             | _|
                             |  |
                             ")

            (t/strip-margin "| _ 
                             || |
                             |  |
                             ")}]
      (is (= expected-additions
             (g/additions p/seven)))))

  (testing "eight"
    (let [expected-additions
          #{(t/strip-margin "| _ 
                             ||_|
                             ||_|
                             ")}]
      (is (= expected-additions
             (g/additions p/eight)))))

  (testing "nine"
    (let [expected-additions
          #{(t/strip-margin "| _ 
                             ||_|
                             | _|
                             ")

            (t/strip-margin "| _ 
                             ||_|
                             ||_|
                             ")}]
      (is (= expected-additions
             (g/additions p/nine))))))

(deftest to-glyphs
  (testing "single digit ints"
    (is (= (p/printable p/zero) (p/to-glyphs 0)))
    (is (= (p/printable p/one) (p/to-glyphs 1)))
    (is (= (p/printable p/two) (p/to-glyphs 2)))
    (is (= (p/printable p/three) (p/to-glyphs 3)))
    (is (= (p/printable p/four) (p/to-glyphs 4)))
    (is (= (p/printable p/five) (p/to-glyphs 5)))
    (is (= (p/printable p/six) (p/to-glyphs 6)))
    (is (= (p/printable p/seven) (p/to-glyphs 7)))
    (is (= (p/printable p/eight) (p/to-glyphs 8)))
    (is (= (p/printable p/nine) (p/to-glyphs 9))))
  (testing "single digits"
    (is (= (p/printable p/zero) (p/to-glyphs "0")))
    (is (= (p/printable p/one) (p/to-glyphs "1")))
    (is (= (p/printable p/two) (p/to-glyphs "2")))
    (is (= (p/printable p/three) (p/to-glyphs "3")))
    (is (= (p/printable p/four) (p/to-glyphs "4")))
    (is (= (p/printable p/five) (p/to-glyphs "5")))
    (is (= (p/printable p/six) (p/to-glyphs "6")))
    (is (= (p/printable p/seven) (p/to-glyphs "7")))
    (is (= (p/printable p/eight) (p/to-glyphs "8")))
    (is (= (p/printable p/nine) (p/to-glyphs "9"))))
  (testing "multiple digits"
    (is (= (t/strip-margin "|    _ 
                            |  | _|
                            |  ||_ 
                            ")
           (p/to-glyphs "12")))
    (is (= (t/strip-margin "|    _    
                            |  | _||_|
                            |  ||_   |
                            ")
           (p/to-glyphs "124")))
    (is (= (t/strip-margin "|    _     _ 
                            |  | _||_||_|
                            |  ||_   ||_|
                            ")
           (p/to-glyphs "1248")))
    (is (= (t/strip-margin "|    _     _     _ 
                            |  | _||_||_|  ||_ 
                            |  ||_   ||_|  ||_|
                            ")
           (p/to-glyphs "124816")))
    (is (= (t/strip-margin "|    _     _     _  _  _ 
                            |  | _||_||_|  ||_  _| _|
                            |  ||_   ||_|  ||_| _||_ 
                            ")
           (p/to-glyphs "12481632")))
    (is (= (t/strip-margin "|    _     _     _  _  _  _    
                            |  | _||_||_|  ||_  _| _||_ |_|
                            |  ||_   ||_|  ||_| _||_ |_|  |
                            ")
           (p/to-glyphs "1248163264")))
    (is (= (t/strip-margin "|    _     _     _  _  _  _        _  _ 
                            |  | _||_||_|  ||_  _| _||_ |_|  | _||_|
                            |  ||_   ||_|  ||_| _||_ |_|  |  ||_ |_|
                            ")
           (p/to-glyphs "1248163264128")))))

(deftest replace-in-glyphs
  (testing "position 0"
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "023456789")
          actual   (p/replace-in-glyphs input 0 (str/join \newline p/zero))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "223456789")
          actual   (p/replace-in-glyphs input 0 (str/join \newline p/two))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "323456789")
          actual   (p/replace-in-glyphs input 0 (str/join \newline p/three))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "423456789")
          actual   (p/replace-in-glyphs input 0 (str/join \newline p/four))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "523456789")
          actual   (p/replace-in-glyphs input 0 (str/join \newline p/five))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "623456789")
          actual   (p/replace-in-glyphs input 0 (str/join \newline p/six))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "723456789")
          actual   (p/replace-in-glyphs input 0 (str/join \newline p/seven))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "823456789")
          actual   (p/replace-in-glyphs input 0 (str/join \newline p/eight))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "923456789")
          actual   (p/replace-in-glyphs input 0 (str/join \newline p/nine))]
      (is (= expected actual))))
  (testing "position 1"
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "103456789")
          actual   (p/replace-in-glyphs input 1 (str/join \newline p/zero))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "113456789")
          actual   (p/replace-in-glyphs input 1 (str/join \newline p/one))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "133456789")
          actual   (p/replace-in-glyphs input 1 (str/join \newline p/three))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "143456789")
          actual   (p/replace-in-glyphs input 1 (str/join \newline p/four))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "153456789")
          actual   (p/replace-in-glyphs input 1 (str/join \newline p/five))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "163456789")
          actual   (p/replace-in-glyphs input 1 (str/join \newline p/six))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "173456789")
          actual   (p/replace-in-glyphs input 1 (str/join \newline p/seven))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "183456789")
          actual   (p/replace-in-glyphs input 1 (str/join \newline p/eight))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "193456789")
          actual   (p/replace-in-glyphs input 1 (str/join \newline p/nine))]
      (is (= expected actual))))
  (testing "position 2"
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "120456789")
          actual   (p/replace-in-glyphs input 2 (str/join \newline p/zero))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "121456789")
          actual   (p/replace-in-glyphs input 2 (str/join \newline p/one))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "122456789")
          actual   (p/replace-in-glyphs input 2 (str/join \newline p/two))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "124456789")
          actual   (p/replace-in-glyphs input 2 (str/join \newline p/four))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "125456789")
          actual   (p/replace-in-glyphs input 2 (str/join \newline p/five))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "126456789")
          actual   (p/replace-in-glyphs input 2 (str/join \newline p/six))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "127456789")
          actual   (p/replace-in-glyphs input 2 (str/join \newline p/seven))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "128456789")
          actual   (p/replace-in-glyphs input 2 (str/join \newline p/eight))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "129456789")
          actual   (p/replace-in-glyphs input 2 (str/join \newline p/nine))]
      (is (= expected actual))))
  (testing "position 3"
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123056789")
          actual   (p/replace-in-glyphs input 3 (str/join \newline p/zero))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123156789")
          actual   (p/replace-in-glyphs input 3 (str/join \newline p/one))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123256789")
          actual   (p/replace-in-glyphs input 3 (str/join \newline p/two))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123356789")
          actual   (p/replace-in-glyphs input 3 (str/join \newline p/three))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123556789")
          actual   (p/replace-in-glyphs input 3 (str/join \newline p/five))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123656789")
          actual   (p/replace-in-glyphs input 3 (str/join \newline p/six))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123756789")
          actual   (p/replace-in-glyphs input 3 (str/join \newline p/seven))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123856789")
          actual   (p/replace-in-glyphs input 3 (str/join \newline p/eight))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123956789")
          actual   (p/replace-in-glyphs input 3 (str/join \newline p/nine))]
      (is (= expected actual))))
  (testing "position 4"
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123406789")
          actual   (p/replace-in-glyphs input 4 (str/join \newline p/zero))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123416789")
          actual   (p/replace-in-glyphs input 4 (str/join \newline p/one))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123426789")
          actual   (p/replace-in-glyphs input 4 (str/join \newline p/two))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123436789")
          actual   (p/replace-in-glyphs input 4 (str/join \newline p/three))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123446789")
          actual   (p/replace-in-glyphs input 4 (str/join \newline p/four))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123466789")
          actual   (p/replace-in-glyphs input 4 (str/join \newline p/six))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123476789")
          actual   (p/replace-in-glyphs input 4 (str/join \newline p/seven))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123486789")
          actual   (p/replace-in-glyphs input 4 (str/join \newline p/eight))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123496789")
          actual   (p/replace-in-glyphs input 4 (str/join \newline p/nine))]
      (is (= expected actual))))
  (testing "position 5"
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123450789")
          actual   (p/replace-in-glyphs input 5 (str/join \newline p/zero))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123451789")
          actual   (p/replace-in-glyphs input 5 (str/join \newline p/one))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123452789")
          actual   (p/replace-in-glyphs input 5 (str/join \newline p/two))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123453789")
          actual   (p/replace-in-glyphs input 5 (str/join \newline p/three))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123454789")
          actual   (p/replace-in-glyphs input 5 (str/join \newline p/four))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123455789")
          actual   (p/replace-in-glyphs input 5 (str/join \newline p/five))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123457789")
          actual   (p/replace-in-glyphs input 5 (str/join \newline p/seven))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123458789")
          actual   (p/replace-in-glyphs input 5 (str/join \newline p/eight))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123459789")
          actual   (p/replace-in-glyphs input 5 (str/join \newline p/nine))]
      (is (= expected actual))))
  (testing "position 6"
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456089")
          actual   (p/replace-in-glyphs input 6 (str/join \newline p/zero))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456189")
          actual   (p/replace-in-glyphs input 6 (str/join \newline p/one))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456289")
          actual   (p/replace-in-glyphs input 6 (str/join \newline p/two))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456389")
          actual   (p/replace-in-glyphs input 6 (str/join \newline p/three))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456489")
          actual   (p/replace-in-glyphs input 6 (str/join \newline p/four))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456589")
          actual   (p/replace-in-glyphs input 6 (str/join \newline p/five))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456689")
          actual   (p/replace-in-glyphs input 6 (str/join \newline p/six))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456889")
          actual   (p/replace-in-glyphs input 6 (str/join \newline p/eight))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456989")
          actual   (p/replace-in-glyphs input 6 (str/join \newline p/nine))]
      (is (= expected actual))))
  (testing "position 7"
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456709")
          actual   (p/replace-in-glyphs input 7 (str/join \newline p/zero))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456719")
          actual   (p/replace-in-glyphs input 7 (str/join \newline p/one))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456729")
          actual   (p/replace-in-glyphs input 7 (str/join \newline p/two))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456739")
          actual   (p/replace-in-glyphs input 7 (str/join \newline p/three))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456749")
          actual   (p/replace-in-glyphs input 7 (str/join \newline p/four))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456759")
          actual   (p/replace-in-glyphs input 7 (str/join \newline p/five))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456769")
          actual   (p/replace-in-glyphs input 7 (str/join \newline p/six))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456779")
          actual   (p/replace-in-glyphs input 7 (str/join \newline p/seven))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456799")
          actual   (p/replace-in-glyphs input 7 (str/join \newline p/nine))]
      (is (= expected actual))))
  (testing "position 8"
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456780")
          actual   (p/replace-in-glyphs input 8 (str/join \newline p/zero))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456781")
          actual   (p/replace-in-glyphs input 8 (str/join \newline p/one))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456782")
          actual   (p/replace-in-glyphs input 8 (str/join \newline p/two))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456783")
          actual   (p/replace-in-glyphs input 8 (str/join \newline p/three))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456784")
          actual   (p/replace-in-glyphs input 8 (str/join \newline p/four))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456785")
          actual   (p/replace-in-glyphs input 8 (str/join \newline p/five))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456786")
          actual   (p/replace-in-glyphs input 8 (str/join \newline p/six))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456787")
          actual   (p/replace-in-glyphs input 8 (str/join \newline p/seven))]
      (is (= expected actual)))
    (let [input    (p/to-glyphs "123456789")
          expected (p/to-glyphs "123456788")
          actual   (p/replace-in-glyphs input 8 (str/join \newline p/eight))]
      (is (= expected actual)))))

(deftest guess-alternatives-for-incorrect-account-numbers
  (let [parse-validate-and-guess (g/with-guessing p/to-digits
                                                  (every-pred v/valid?
                                                              v/legible?))]
    (testing "711111111"
      (let [glyphs (t/strip-margin "|                           
                                    |  |  |  |  |  |  |  |  |  |
                                    |  |  |  |  |  |  |  |  |  |
                                    ")]
        (is (= "711111111"
               (parse-validate-and-guess glyphs)))))

    (testing "777777177"
      (let [glyphs (t/strip-margin "| _  _  _  _  _  _  _  _  _ 
                                    |  |  |  |  |  |  |  |  |  |
                                    |  |  |  |  |  |  |  |  |  |
                                    ")]
        (is (= "777777177"
               (parse-validate-and-guess glyphs)))))

    (testing "200800000"
      (let [glyphs (t/strip-margin "| _  _  _  _  _  _  _  _  _ 
                                    | _|| || || || || || || || |
                                    ||_ |_||_||_||_||_||_||_||_|
                                    ")]
        (is (= "200800000"
               (parse-validate-and-guess glyphs)))))

    (testing "333393333"
      (let [glyphs (t/strip-margin "| _  _  _  _  _  _  _  _  _ 
                                    | _| _| _| _| _| _| _| _| _|
                                    | _| _| _| _| _| _| _| _| _|
                                    ")]
        (is (= "333393333"
               (parse-validate-and-guess glyphs)))))

    (testing "888888888 AMB ['888886888', '888888880', '888888988']"
      (let [glyphs (t/strip-margin "| _  _  _  _  _  _  _  _  _ 
                                    ||_||_||_||_||_||_||_||_||_|
                                    ||_||_||_||_||_||_||_||_||_|
                                    ")]
        (is (= "888888888 AMB ['888886888', '888888880', '888888988']"
               (parse-validate-and-guess glyphs)))))

    (testing "555555555 AMB ['555655555', '559555555']"
      (let [glyphs (t/strip-margin "| _  _  _  _  _  _  _  _  _ 
                                    ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
                                    | _| _| _| _| _| _| _| _| _|
                                    ")]
        (is (= "555555555 AMB ['555655555', '559555555']"
               (parse-validate-and-guess glyphs)))))

    (testing "666666666 AMB ['666566666', '686666666']"
      (let [glyphs (t/strip-margin "| _  _  _  _  _  _  _  _  _ 
                                    ||_ |_ |_ |_ |_ |_ |_ |_ |_ 
                                    ||_||_||_||_||_||_||_||_||_|
                                    ")]
        (is (= "666666666 AMB ['666566666', '686666666']"
               (parse-validate-and-guess glyphs)))))

    (testing "999999999 AMB ['899999999', '993999999', '999959999']"
      (let [glyphs (t/strip-margin "| _  _  _  _  _  _  _  _  _ 
                                    ||_||_||_||_||_||_||_||_||_|
                                    | _| _| _| _| _| _| _| _| _|
                                    ")]
        (is (= "999999999 AMB ['899999999', '993999999', '999959999']"
               (parse-validate-and-guess glyphs)))))

    (testing "490067715 AMB ['490067115', '490067719', '490867715']"
      (let [glyphs (t/strip-margin "|    _  _  _  _  _  _     _ 
                                    ||_||_|| || ||_   |  |  ||_ 
                                    |  | _||_||_||_|  |  |  | _|
                                    ")]
        (is (= "490067715 AMB ['490067115', '490067719', '490867715']"
               (parse-validate-and-guess glyphs)))))

    (testing "123456789"
      (let [glyphs (t/strip-margin "|    _  _     _  _  _  _  _ 
                                    | _| _| _||_||_ |_   ||_||_|
                                    |  ||_  _|  | _||_|  ||_| _|
                                    ")]
        (is (= "123456789"
               (parse-validate-and-guess glyphs)))))

    (testing "000000051"
      (let [glyphs (t/strip-margin "| _     _  _  _  _  _  _    
                                    || || || || || || || ||_   |
                                    ||_||_||_||_||_||_||_| _|  |
                                    ")]
        (is (= "000000051"
               (parse-validate-and-guess glyphs)))))

    (testing "490867715"
      (let [glyphs (t/strip-margin "|    _  _  _  _  _  _     _ 
                                    ||_||_|| ||_||_   |  |  | _ 
                                    |  | _||_||_||_|  |  |  | _|
                                    ")]
        (is (= "490867715"
               (parse-validate-and-guess glyphs)))))))

(deftest transform-input-into-output
  (let [source          "test-resources/story-04/actual-input.txt"
        destination     "test-resources/story-04/actual-output.txt"
        expected-output "test-resources/story-04/expected-output.txt"]

    (c/transform {:src source
                  :dst destination})

    (is (= (slurp expected-output)
           (slurp destination)))))
