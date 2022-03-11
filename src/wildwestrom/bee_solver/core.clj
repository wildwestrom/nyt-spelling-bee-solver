(ns wildwestrom.bee-solver.core
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [failjure.core :as f]
            [wildwestrom.bee-solver.solver :refer [solver]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aggregate Wordlists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def wordlist
  (->> "resources/"
       io/file
       file-seq
       (filter #(not (.isDirectory %)))
       (map (fn [file] (set (line-seq (io/reader file)))))
       (reduce set/union)
       sort))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn validate-not-blank [letter]
  (if-not (empty? letter)
    letter
    (f/fail "Must contain a value")))

(defn validate-length-is-one [letter]
  (if (= 1 (count letter))
    letter
    (f/fail "String must have only one character.")))

(defn validate-is-alpha [letter]
  (if (some? (re-find #"[A-z]" letter))
    letter
    (f/fail "Every char must be alphanumeric.")))

(defn validate-char [input]
  (f/ok-> input
          validate-not-blank
          validate-length-is-one
          validate-is-alpha))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-str [state]
  (let [result     (validate-char (str (read)))
        handle-err (fn [e]
                     (println (f/message e) "\nTry again: ")
                     (get-str state))]
    (f/attempt handle-err
               (if (f/failed? result)
                 result
                 (do (swap! state conj result)
                     (if (apply distinct? @state)
                       (do (println @state)
                           result)
                       (do (swap! state #(drop 1 %))
                           (f/fail "No duplicate letters allowed."))))))))

(defn format-list [word-seq]
  (println (str/join "\n" word-seq)))

(defn -main [& args]
  (let [letters-state   (atom '())
        get-str*        #(get-str letters-state)
        required-letter (do (println "What is the required letter?")
                            (get-str*))
        other-letters   (do (println "What are the other letters?")
                            (take 6 (repeatedly get-str*)))]
    (println)
    (format-list (solver wordlist required-letter other-letters))))
