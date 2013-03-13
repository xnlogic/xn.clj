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

(defn create-one [{:keys [model ignore errors options] :as opts} body]
  (when (map? body)
    (let [result (xn/execute (merge
                               {:method :put
                                :url (str "/model/"
                                          (name (if (fn? model)
                                                  (model body)
                                                  model)))
                                :body (apply dissoc body ignore)
                                :throw-exceptions false}
                               options))]
      (if (vector? result)
        (first result)
        (do
          (clojure.pprint/pprint result)
          (when errors (swap! errors assoc (body key) result))
          nil)))))

(defn create-one-unique [{:keys [model key ignore errors] :as opts} body]
  {:pre [key]}
  (when (and (map? body) (body key))
    [(body key)
     (create-one (assoc opts :options {:query {:unique (name key)}})
                 body)]))

; create-unique -> {keyvalue id}
(defn create-unique [opts records]
  (->> records
       vec-wrap
       (map #(create-one-unique opts %))
       (into {})))

(defn create [opts records]
  (->> records
       vec-wrap
       (map #(create-one opts %))
       set))

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

(defn- extract-one* [{:keys [pre row clean merge-rules fields template mappings
                             filters post-merge import]
                      :or {clean {} merge-rules {} template {} mappings []
                           filters [] post-merge {} pre []}}]
  {:pre [(or fields row)]}
  (let [fields (or fields (remove nil? row))
        fields (if (map? fields)
                 (filter (fn [[from to]] to) fields)
                 (into {} (map vector fields fields)))]
    (letfn [(apply-pre-fns [data]
              (reduce (fn [data f] (f data)) data pre))
            (make-maps-from-rows [data]
              (if (and row (sequential? data))
                (map vector row data)
                data))
            (default-rule [v]
              (cond
                (string? v)      (let [v (.trim v)] (when-not (= "" v) v))
                (and (number? v)
                     (zero? v))  nil
                :else            v))
            (rename-and-merge [r]
              (->> fields
                   (mapcat (fn renamer [[from to]]
                             (cond
                               (keyword? to) [{to (r from)}]
                               (sequential? to) (mapcat #(renamer [from %]) to)
                               :else [])))
                   (apply merge-with-rules merge-rules)))
            (apply-template [r] (merge template r))
            (not-blank [r]
              (when (not-every? nil? r) r))
            (apply-mappings [r]
              (reduce (fn [data f] (f data)) r mappings))
            (apply-filters [r]
              (when (every? #(% r) filters) r))
            (add-to-import [source]
              (if import
                (fn [r]
                  (assoc r :import_records {:CREATE :import_record
                                            :data source
                                            :import {:set import}}))
                identity))]
      (let [clean-fields (key-mapper clean default-rule)
            finalize-fields (key-mapper post-merge identity)]
        (fn [record]
          (some-> record
                  apply-pre-fns
                  make-maps-from-rows
                  clean-fields
                  rename-and-merge
                  apply-template
                  not-blank
                  apply-mappings
                  finalize-fields
                  apply-filters
                  ((add-to-import record))))))))

(defn extract-one [& {:as opts}]
  (extract-one* opts))

(defn extract [& {:as opts :keys [create create-unique reader skip-rows]
                  :or {skip-rows 0}}]
  (fn [records & {:keys [filename notes run offset limit]
                  :or {offset skip-rows limit 99999999}}]
    {:pre [(or records (and filename reader))]}
    (let [records (or records (reader filename))
          import (when (or filename notes)
                      (create-one {:model :import :options {:throw-exceptions true}}
                                  {:filename filename :notes notes}))
          extractor (extract-one* (assoc opts :import import))]
      (let [results (->> records
                         vec-wrap
                         (drop offset)
                         (take limit)
                         (map extractor))]
        (if run
          (cond
            create
            (xn.import/create create results)
            create-unique
            (xn.import/create-unique create-unique results)
            :else
            results)
          results)))))

; TODO: remove references to extract-records
(defn extract-records
  ([fields records]
   ((extract :fields fields) records))
  ([clean-rules merge-rules fields records]
   ((extract :clean clean-rules :merge merge-rules :fields fields) records)))

(defn extract-rel-records [add-or-set model-name uniques & extract-rules]
  {:pre [add-or-set model-name]}
  (fn [records]
    {add-or-set (->> records
                     vec-wrap
                     ((apply extract extract-rules))
                     (map #(merge {:CREATE model-name}
                                  (when uniques {:UNIQUE uniques})
                                  %)))}))

(defn external [data-source]
  (fn [id]
    (when id
      {:CREATE :external_record :UNIQUE :name
       :name (str data-source "/" id)})))

(defn create-data-sources [& names]
  (create-unique {:model :data_source :key :name}
                 (map (fn [name] {:name name}) names)))

(defn external-name [data-source]
  (fn [id]
    (when id (str data-source "/" id))) )

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
            (map (fn [r] (update-in r [field] rels)) records))
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
           (when (seq parts) (str "is/" (s/join "," (map name parts)))) nil]
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
             (when (seq parts) (str "is/" (s/join "," (map name parts)))) nil]
            {:query {:limit :1000000
                     "name~1" source-name
                     "name~2[value]" (s/join "," ids)}}))))
