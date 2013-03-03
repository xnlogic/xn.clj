(ns lm.import.problems
  (:require [xn.client :as xn]
            [xn.import :as i :refer [extract-records create-unique set-one-rels
                                     add-many-rels]]
            [clojure.string :as s]
            [xn.tools :refer [vectorize]]
            [clojure.pprint :refer [pprint]]
            ))

(defn related-records [records]
  (extract-records
    {:id :external_id
     :name :name}
    records))

(defn add-rel
  (fn [values]
    {:add (map rels values)}))

(defn problem-records [records]
  (extract-records
    ; clean
    {:incidents (set-by-externals "Remedy" :id)
     :related_cis (set-by-externals "Remedy" :id)
     :rfcs (set-by-externals "Remedy" :id)
     :id (add-external "Remedy")
     :organization (add-rel {:model_name :customer
                             :UNIQUE :name}
                            :name)
     :support_group (add-rel {:model_name :support_group
                              :UNIQUE :name}
                             :name)}
    ; merge
    {}
    ; field mapping
    {:status              nil                  ;#{nil},
     :source_code         nil                  ;#{nil                               java.lang.String},
     :class               nil                  ;#{java.lang.String},
     :incidents           :incidents           ;#{clojure.lang.PersistentVector},
     :submitter           :submitter           ;#{java.lang.String},
     :support_group       :support_group       ;#{nil                               java.lang.String},
     :organization        :customer            ;#{java.lang.String},
     :short_description   :name                ;#{java.lang.String},
     :related_cis         :applies_to          ;#{clojure.lang.PersistentVector},
     :submit_date         :submission_date     ;#{java.lang.String},
     :id                  :external_records    ;#{java.lang.String},
     :description         :description         ;#{nil                               java.lang.String},
     :rfcs                :rfcs                ;#{clojure.lang.PersistentVector},
     :priority            :priority            ;#{java.lang.Long}
     }
    records))





(defn load! [raw]
  (let [records (problem-records raw)
        ]))


(comment
  ; quickly see the shape of the data
  (->> filename i/json-lines
       #_(mapcat :related_cis)
       #_related-records
       i/analyze-records
       pprint))

