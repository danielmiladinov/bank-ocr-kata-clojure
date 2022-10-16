(defproject bank-ocr-kata-clojure "0.1.0-SNAPSHOT"
  :description "An implementation of the Bank OCR Kata in Clojure (https://codingdojo.org/kata/BankOCR/)"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.cli "1.0.206"]]
  :source-paths ["src"]
  :test-paths ["test"]
  :resource-paths ["resources"]
  :main ^:skip-aot bank-ocr-kata-clojure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot      :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev     {:resource-paths ["test-resources"]
                       :source-paths   ["dev-resources"]
                       :main           dev}})

