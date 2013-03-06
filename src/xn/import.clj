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

(defn map-to-rels [add-or-set fns]
  (fn [records]
    {add-or-set (map (apply juxt fns) records)}))

(defn extract-rel-unique [add-or-set model-name field & fns]
  (map-to-rels add-or-set
         (fn [value] {:CREATE model-name, :UNIQUE field, field value})
         fns))

(defn extract-rel-non-unique [add-or-set model-name field & fns]
  (map-to-rels add-or-set
         (fn [value] {:CREATE model-name, field value})
         fns))

(defn extract [& {:keys [clean merge-rules fields template mappings filters]}]
   (let [default-rule (fn [v] (cond
                                (string? v) (let [v (.trim v)] (when-not (= "" v) v))
                                (and (number? v) (zero? v)) nil
                                :else v))
         fields (if (map? fields)
                  (filter (fn [[from to]] to) fields)
                  (into {} (map vector fields fields)))]
     (fn [records]
       (->> (cond (sequential? records) records
                  records [records]
                  :else [])
            (map (fn [r]
                   (->> fields
                        (map (fn [[from to]]
                               (let [v (r from)
                                     v ((clean from (:default clean default-rule)) v)]
                                 {to v})))
                        (apply merge-with-rules merge-rules))))
            (map (fn [r] (merge template r)))
            (filter #(not-every? nil? %))
            set
            (fn [extracted] (if mappings
                              (reduce (fn [data f] (map f data)) extracted mappings)
                              extracted))
            (fn [extracted] (if filters
                              (reduce (fn [data f] (filter f data)) extracted filters)))))))

(defn extract-records
  ([fields records]
   ((extract :fields fields) records))
  ([clean-rules merge-rules fields records]
   ((extract :clean clean-rules :merge merge-rules :fields fields) records)))

(defn extract-rel-records [add-or-set model-name uniques & extract-rules]
  (fn [records]
    {add-or-set (->> (if (sequential? records) records [records])
                     ((apply extract extract-rules))
                     (map #(merge {:CREATE model-name :UNIQUE uniques} %)))})) ; set model-name and unique

(defn external [data-source]
  (fn [id]
    {:CREATE :external_record :UNIQUE :name
     :name (str data-source "/" id)}))

(defn set-by-externals [data-source & fns]
  (map-to-rels :set (external data-source) fns))

(defn add-by-externals [data-source & fns]
  (map-to-rels :add (external data-source) fns))




; -------------------------------------------------------------------------
; hopefully these methods can be removed

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
