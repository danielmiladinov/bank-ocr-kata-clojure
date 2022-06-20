(ns bank-ocr-kata-clojure.core
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [clojure.string :as str]
            [bank-ocr-kata-clojure.writer :as writer]
            [bank-ocr-kata-clojure.reader :as reader]
            [bank-ocr-kata-clojure.parser :as parser]
            [bank-ocr-kata-clojure.validator :as validator])
  (:gen-class))

(defn can-read? [file-path]
  (.canRead (io/file file-path)))

(defn can-write? [file-path]
  (let [file (io/file file-path)]
    (if (.exists file)
      (.canWrite file)
      (-> file
          (.getParentFile)
          (.canWrite)))))

(def cli-options
  [["-s" "--src INPUT-FILE-CONTAINING-FAXED-ACCOUNT-NUMBERS" "Source file containing faxed account numbers"
    :validate [can-read?]]
   ["-d" "--dst OUTPUT-FILE-TO-CONTAIN-DECODED-ACCOUNT-NUMBERS" "Output file to contain decoded account numbers"
    :validate [can-write?]]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["This is my bank-ocr-kata, written in Clojure."
        ""
        "Options:"
        options-summary
        ""
        "Usage: "
        "java -jar bank-ocr-kata-clojure-0.1.0-standalone.jar \\"
        "  --src INPUT-FILE-CONTAINING-FAXED-ACCOUNT-NUMBERS \\"
        "  --dst OUTPUT-FILE-TO-CONTAIN-DECODED-ACCOUNT-NUMBERS \\"
        ""]
       (str/join \newline)))

(defn error-msg [errors]
  (str "Please ensure that:\n"
       "  --src specifies a readable file, and that\n"
       "  --dst specifies a writable file.\n"
       "The following errors occurred while parsing your command:\n"
       (str/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with an error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message (usage summary) :ok? true}

      errors
      {:exit-message (error-msg errors)}

      :else
      {:ok? true :options options})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn transform [{:keys [src dst]}]
  (->> (reader/read-account-number-glyphs src)
       (map (comp validator/validate
                  parser/to-digits))
       (writer/write-account-number-digits dst)))

(defn -main
  [& args]
  (let [{:keys [options ok? exit-message]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 -1) exit-message)
      (transform options))))
