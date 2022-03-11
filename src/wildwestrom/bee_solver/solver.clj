(ns wildwestrom.bee-solver.solver
  (:require [clojure.string :as str]))

(defn first-pass-filter [dict]
  (->> dict
       (filter #(<= 4 (count %)))
       (filter #(when (nil? (re-find #"[-\s\W]" %)) true))))

(defn find-req-letter [letter dict]
  (let [pattern (re-pattern (str "(?i:(?=.*" letter "))"))]
    (filter #(when-not (nil? (re-find pattern %)) true)
            dict)))

(defn find-matching-words [req-letter other-letters dict]
  (let [pattern (re-pattern (str "(?i:\\b[" req-letter (str/join other-letters) "]+\\b)"))]
    (filter #(when-not (nil? (re-find pattern %)) true)
            dict)))

(defn solver [dict req-letter other-letters]
  (->> dict
       (first-pass-filter)
       (find-req-letter req-letter)
       (find-matching-words req-letter other-letters)))
