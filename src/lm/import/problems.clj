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
    {:incidents (add-by-externals "Remedy" :id)
     :related_cis (add-by-externals "Remedy" :id)
     :rfcs (add-by-externals "Remedy" :id)
     :id (external "Remedy")
     :organization (extract-rel-unique :customer :name)
     :support_group (extract-rel-unique :support_group :name)}
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

