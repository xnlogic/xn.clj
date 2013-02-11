(ns xn.import
  (:require [xn.client :as xn]
            [clojure-csv.core :as csv]
            [clojure.string :as s]))

(defn api-url-regex []
  (let [url (s/replace @xn/*url-root #"/v1$" "")]
    (re-pattern (str "^(" url "|http://localhost:\\d+)(/v1)?"))))

(def file-lines (comp s/split-lines slurp))

(def parse-csv csv/parse-csv)

(defn csv [filename]
  (-> filename slurp csv/parse-csv))

(defn has [f] (partial some f))

(defn files [dir]
  (->> dir clojure.java.io/file
    .listFiles
    (map #(.getPath %))))

(defn csv-files [dir]
  (filter #(re-find #"\.csv$" %) (files dir)))

(defn page-url? [s]
  (re-find (api-url-regex) s))

(defn relative-url [s]
  (s/replace s (api-url-regex) ""))

(defn match-url [pattern]
  (fn [s]
    (let [url (relative-url s)]
      (when (re-find pattern url)
        url))))

(defn model-url-matcher [model]
  (match-url (re-pattern (str #"^/model/" model #"/\d+/?$"))))

(defn record-url-matcher [& parts]
  (match-url (re-pattern (str #"^/is/" (s/join "," parts) #"/\d+/?$"))))
