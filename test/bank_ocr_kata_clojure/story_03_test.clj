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
  (:require [bank-ocr-kata-clojure.core :as core]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [bank-ocr-kata-clojure.validator :as validator]
            [bank-ocr-kata-clojure.parser :as parser]
            [bank-ocr-kata-clojure.tools :as tools]))

(deftest parse-and-validate-single-account-numbers
  (let [parse-and-validate (comp validator/validate
                                 parser/to-digits)]
    (testing "000000051"
      (let [glyph (tools/strip-margin "| _  _  _  _  _  _  _  _    
                                       || || || || || || || ||_   |
                                       ||_||_||_||_||_||_||_| _|  |
                                       ")]
        (is (= "000000051"
               (parse-and-validate glyph)))))
    
    (testing "49006771? ILL"
      (let [glyph (tools/strip-margin "|    _  _  _  _  _  _     _ 
                                       ||_||_|| || ||_   |  |  | _ 
                                       |  | _||_||_||_|  |  |  | _|
                                       ")]
        (is (= "49006771? ILL"
               (parse-and-validate glyph)))))

    (testing "123456780 ERR"
      (let [glyph (tools/strip-margin "|    _  _     _  _  _  _  _ 
                                       |  | _| _||_||_ |_   ||_|| |
                                       |  ||_  _|  | _||_|  ||_||_|
                                       ")]
        (is (= "123456780 ERR"
               (parse-and-validate glyph)))))
    
    (testing "1234?678? ILL"
      (let [glyph (tools/strip-margin "|    _  _     _  _  _  _  _ 
                                       |  | _| _||_| _ |_   ||_||_|
                                       |  ||_  _|  | _||_|  ||_| _ 
                                       ")]
        (is (= "1234?678? ILL"
               (parse-and-validate glyph)))))))

(deftest transform-input-into-output
  (let [src             "test-resources/story-03/actual-input.txt"
        dst             "test-resources/story-03/actual-output.txt"
        expected-output (->> ["457508000"
                              "664371495 ERR"
                              "86110??36 ILL\n"]
                             (str/join \newline))]

    (core/transform {:src src
                     :dst dst})

    (is (= expected-output
           (slurp dst)))))
