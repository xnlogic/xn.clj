(ns lm.import.device_itsm
  (:require [xn.client :as xn]
            [xn.import :as i :refer [extract-records create-unique set-one-rels add-many-rels]]
            [clojure.string :as s]
            [xn.tools :refer [vectorize]]))

(defn incident-records [records]
  (extract-records
    {:status :status
     :impact :impact
     :class nil
     :submitter :submitter
     :support_group :project
     :organization :customer
     :short_description :description
     :urgency :urgency
     :submit_date :submission_date
     :id :ciid
     :priority :priority}
    records))

(defn rfc-records [records]
  (extract-records
    {:status :status
     :class nil
     :submitter :submitter
     :submitter_email nil
     :organization :customer
     :service_category_tier_1 :sc
     :service_category_tier_2 :sc
     :service_category_tier_3 :sc
     :short_description :description
     :submitter_full_name nil
     :submit_date :submission_date
     :id :ciid
     :project :project}
    records))

(defn device-records [records]
  (extract-records
    {:incidents incident-records
     :rfcs rfc-records}
    {:sc vectorize}
    {:device_ciid :ciid ; look up by external record
     :device_name nil
     :device_class nil
     :incidents :incidents,
     :rfcs :rfcs}
    records))

