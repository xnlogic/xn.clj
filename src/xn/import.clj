(ns xn.import
  (:require [xn.client :as xn]
            [xn.tools :refer [merge-with-rules]]
            [clojure.data.json :as json]
            [clojure-csv.core :as csv]
            [clojure.string :as s]
            [xn.repl :refer [prident]]))

(defn json-file [filename]
  (json/read-str (slurp filename) :key-fn keyword))

(defn api-url-regex []
  (let [url (s/replace @xn/*url-root #"/v1$" "")]
    (re-pattern (str "^(" url "|http://localhost:\\d+)(/v1)?"))))

(def file-lines (comp s/split-lines slurp))

(defn json-lines [filename]
  (->> filename file-lines
    (map #(json/read-str % :key-fn keyword))))

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


; create-unique -> {keyvalue id}
(defn create-unique [{:keys [model key ignore errors]} records]
  (->> (if (map? records) [records] records)
    (filter key)
    (map (fn [body]
           [(body key)
            (let [result (xn/execute {:method :put
                                      :url (str "/model/"
                                                (name (if (fn? model)
                                                        (model body)
                                                        model)))
                                      :query {:unique (name key)}
                                      :body (apply dissoc body ignore)
                                      :throw-exceptions false
                                      })]
              (if (vector? result)
                (first result)
                (do
                  (clojure.pprint/pprint result)
                  (when errors (swap! errors assoc (body key) result))
                  nil)))]))
    (into {})))

(defn extract-records
  ([fields records]
   (extract-records {} {} fields records))
  ([clean-rules merge-rules fields records]
   (let [default-rule (fn [v] (cond
                                (string? v) (let [v (.trim v)] (when-not (= "" v) v))
                                (and (number? v) (zero? v)) nil
                                :else v))
         fields (if (map? fields)
                  (filter (fn [[from to]] to) fields)
                  (into {} (map vector fields fields)))]
     (->> (or records [])
       (map (fn [r]
              (->> fields
                (map (fn [[from to]]
                       (let [v (r from)
                             v ((clean-rules from (:default clean-rules default-rule)) v)]
                         {to v})))
                (apply merge-with-rules merge-rules))))
       (filter #(not-every? nil? %))
       (set)))))

(defn set-one-rels [fields records]
  (reduce (fn [records [field rels]]
            (map (fn [r] (update-in r [field] #(rels %))) records))
          records
          fields))

(defn add-many-rels [fields records]
  (reduce (fn [records [field rels]]
            (map (fn [r] (update-in
                           r [field]
                           (fn [values]
                             {:add (map rels values)})))
                 records))
          records
          fields))

(defn key->id [part key]
  (->> (xn/make-request {:url (str "is/" (name part) "/properties/" (name key))
                         :method :get
                         :query {:limit :1000000}})
    (map reverse)
    (map vec)
    (into {})))

(defn external-ids-map [source-name & parts]
  (into {}
        (xn/get-path-properties
          ["is/data_source" nil
           "filter/name" nil
           "rel/external_records" :record_id
           "rel/record" :xnid
           (when-not (empty? parts) (str "is/" (s/join "," (map name parts)))) nil]
          {:query {:name source-name :limit :1000000}})))

(defn external-ids->xnids [source-name parts ids]
  (if (empty? ids)
    []
    (into {}
          (xn/get-path-properties
            ["is/data_source" nil
             "filter/name~1" nil
             "rel/external_records" :record_id
             "filter/name~2" nil
             "rel/record" :xnid
             (when-not (empty? parts) (str "is/" (s/join "," (map name parts)))) nil]
            {:query {:limit :1000000
                     "name~1" source-name
                     "name~2[value]" (s/join "," ids)}}))))
