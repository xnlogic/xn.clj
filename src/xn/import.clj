(ns xn.import
  (:require [xn.client :as xn]
            [xn.tools :refer [merge-with-rules key-mapper vec-wrap]]
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

(defn create-one-unique [{:keys [model key ignore errors] :as opts} body]
  (when (and (body key) (map? (body key)))
    [(body key)
     (let [result (xn/execute {:method :put
                               :url (str "/model/"
                                         (name (if (fn? model)
                                                 (model body)
                                                 model)))
                               :query {:unique (name key)}
                               :body (apply dissoc body ignore)
                               :throw-exceptions false})]
       (if (vector? result)
         (first result)
         (do
           (clojure.pprint/pprint result)
           (when errors (swap! errors assoc (body key) result))
           nil)))]))

; create-unique -> {keyvalue id}
(defn create-unique [{:keys [key] :as opts} records]
  (->> records
       vec-wrap
       (map #(create-one-unique opts %))
       (into {})))

(defn map-to-rels [add-or-set fns]
  (fn [records]
    (when records
      {add-or-set (map (apply comp fns) (vec-wrap records))})))

(defn extract-rel-unique [add-or-set model-name field & fns]
  (map-to-rels add-or-set
               (cons (fn [value] {:CREATE model-name, :UNIQUE field, field value})
                     fns)))

(defn extract-rel-non-unique [add-or-set model-name field & fns]
  (map-to-rels add-or-set
               (cons (fn [value] {:CREATE model-name, field value})
                     fns)))

(defn extract-one [& {:keys [clean merge-rules fields template mappings filters post-merge import]
                      :or {clean {} merge-rules {} template {} mappings [] filters [] post-merge {}}}]
  (letfn [(default-rule [v]
            (cond
              (string? v)      (let [v (.trim v)] (when-not (= "" v) v))
              (and (number? v) (zero? v)) nil
              :else            v))
          (rename-and-merge [r]
            (->> fields
                 (map (fn [[from to]] {to (r from)}))
                 (apply merge-with-rules merge-rules)))
          (apply-template [r] (merge template r))
          (not-blank [r]
            (when (not-every? nil? r) r))
          (apply-mappings [r]
            (reduce (fn [data f] (f data)) record mappings))
          (apply-filters [r]
            (when (every? #(% r) filters) r))]
    (let [clean-fields (key-mapper clean default-rule)
          finalize-fields (key-mapper post-merge identity)])
    (fn [record]
      (some-> record
              clean-fields
              rename-and-merge
              apply-template
              not-blank
              apply-mappings
              finalize-fields
              apply-filters))))

(defn extract [& {:keys [fields import] :as opts}]
  (let [default-rule
        fields (if (map? fields)
                 (filter (fn [[from to]] to) fields)
                 (into {} (map vector fields fields)))
        extractor (apply extract-one-record opts)]
    (fn [records]
      (->> records
           (map extractor)
           set))))

; TODO: remove references to extract-records
(defn extract-records
  ([fields records]
   ((extract :fields fields) records))
  ([clean-rules merge-rules fields records]
   ((extract :clean clean-rules :merge merge-rules :fields fields) records)))

(defn extract-rel-records [add-or-set model-name uniques & extract-rules]
  (fn [records]
    {add-or-set (->> records
                     vec-wrap
                     ((apply extract extract-rules))
                     (map #(merge {:CREATE model-name :UNIQUE uniques} %)))})) ; set model-name and unique

(defn external [data-source]
  (fn [id]
    (when id
      {:CREATE :external_record :UNIQUE :name
       :name (str data-source "/" id)})))

(defn set-by-externals [data-source & fns]
  (map-to-rels :set [(external data-source) fns]))

(defn add-by-externals [data-source & fns]
  (map-to-rels :add [(external data-source) fns]))




; -------------------------------------------------------------------------
; These should not be needed very often. Much clearer to build up a nested
; object that defines or finds all of the related elements in one request.

(defn set-one-rels
  "Use this when you already know the id of the related record"
  [fields records]
  (reduce (fn [records [field rels]]
            (map (fn [r] (update-in r [field] #(rels %))) records))
          records
          fields))

(defn add-many-rels
  "Use this when you already know the id of the related record"
  [fields records]
  (reduce (fn [records [field rels]]
            (map (fn [r] (update-in
                           r [field]
                           (fn [values]
                             {:add (map rels values)})))
                 records))
          records
          fields))

; -------------------------------------------------------------------------
; Built these before I added the ability to post nested elements or URLs like
; /is/asset/external_id/Source/ID,ID2/properties/name which make having a map
; of external ids to xnids or ids much less necessary.

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
