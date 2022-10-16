(ns bank-ocr-kata-clojure.story-03-test
  "Your boss is keen to see your results.
  He asks you to write out a file of your findings,
  one for each input file, in this format:

  ```text
  457508000
  664371495 ERR
  86110??36 ILL
  ```

  i.e., the file has one account number per row.
  If some characters are illegible, they are replaced by a ?.
  In the case of a wrong checksum, or illegible number,
  this is noted in a second column indicating status."
  (:require [bank-ocr-kata-clojure.core :as c]
            [bank-ocr-kata-clojure.parser :as p]
            [bank-ocr-kata-clojure.tools :as t]
            [bank-ocr-kata-clojure.validator :as v]
            [clojure.string :as s]
            [clojure.test :refer [deftest is testing]]))

(deftest parse-and-validate-single-account-numbers
  (let [parse-and-validate (comp v/validate
                                 p/to-digits)]
    (testing "000000051"
      (let [glyph (t/strip-margin "| _  _  _  _  _  _  _  _    
                                   || || || || || || || ||_   |
                                   ||_||_||_||_||_||_||_| _|  |
                                   ")]
        (is (= "000000051"
               (parse-and-validate glyph)))))

    (testing "49006771? ILL"
      (let [glyph (t/strip-margin "|    _  _  _  _  _  _     _ 
                                   ||_||_|| || ||_   |  |  | _ 
                                   |  | _||_||_||_|  |  |  | _|
                                   ")]
        (is (= "49006771? ILL"
               (parse-and-validate glyph)))))

    (testing "123456780 ERR"
      (let [glyph (t/strip-margin "|    _  _     _  _  _  _  _ 
                                   |  | _| _||_||_ |_   ||_|| |
                                   |  ||_  _|  | _||_|  ||_||_|
                                   ")]
        (is (= "123456780 ERR"
               (parse-and-validate glyph)))))

    (testing "1234?678? ILL"
      (let [glyph (t/strip-margin "|    _  _     _  _  _  _  _ 
                                   |  | _| _||_| _ |_   ||_||_|
                                   |  ||_  _|  | _||_|  ||_| _ 
                                   ")]
        (is (= "1234?678? ILL"
               (parse-and-validate glyph)))))))

(deftest transform-input-into-output
  (let [src             "test-resources/story-03/actual-input.txt"
        dst             "test-resources/story-03/actual-output.txt"
        ;; Changes made in story 4 updated the actual output: "664371495 ERR" gets autocorrected to "664371485"
        expected-output (->> ["457508000"
                              "664371485"
                              "86110??36 ILL\n"]
                             (s/join \newline))]

    (c/transform {:src src
                  :dst dst})

    (is (= expected-output
           (slurp dst)))))
