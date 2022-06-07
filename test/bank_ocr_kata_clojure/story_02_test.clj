(ns bank-ocr-kata-clojure.story-02-test
  "Having done that (finished story 1), you quickly realize that the ingenious machine is not in fact infallible.
  Sometimes it goes wrong in its scanning. The next step therefore is to validate that the numbers you read are
  in fact valid account numbers. A valid account number has a valid checksum. This can be calculated as follows:

  ```text
  account number:  3  4  5  8  8  2  8  6  5
  position names:  d9 d8 d7 d6 d5 d4 d3 d2 d1

  checksum calculation:
  (d1+2*d2+3*d3+...+9*d9) mod 11 = 0
  ```"
  (:require [bank-ocr-kata-clojure.validator :as v]
            [clojure.test :refer [deftest is testing]]))

(deftest account-number-valid?
  (testing "positive cases"
    (is (true? (v/valid? "345882865")))
    (is (true? (v/valid? "457508000"))))

  (testing "negative cases"
    (is (false? (v/valid? "664371495")))
    (is (false? (v/valid? "64371495")))
    (is (false? (v/valid? "6437149567"))))

  (testing "not strings"
    (is (false? #_:clj-kondo/ignore (v/valid? [1 2 3 4 5 6 7 8 9])))
    (is (false? #_:clj-kondo/ignore (v/valid? (Object.))))
    (is (false? #_:clj-kondo/ignore (v/valid? 123456789)))
    (is (false? #_:clj-kondo/ignore (v/valid? #{'fee 'fie 'foe 'fum})))
    (is (false? #_:clj-kondo/ignore (v/valid? true)))
    (is (false? #_:clj-kondo/ignore (v/valid? {:foo :bar})))))
