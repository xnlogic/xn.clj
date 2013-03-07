; remedy_incidents, rfcs, problems and releases (in that order) and then the remedy_devices_to_itsm file to stitch it all together...

(ns lm.import.device_itsm
  (:require [xn.client :as xn]
            [xn.import :as i :refer [extract create-unique extract-rel-unique
                                     external add-by-externals ]]
            [clojure.string :as s]
            [xn.tools :refer [vectorize]]))

(def incident-records
  (extract
    :clean {:organization (extract-rel-unique :set :customer :name)
            :support_group (extract-rel-unique :set :support_group :name)
            :id (external "Remedy")
            }
    :fields {:status :status
             :impact :impact
             :class nil
             :submitter :submitter
             :support_group :support_group
             :organization :customer
             :short_description :description
             :urgency :urgency
             :submit_date :submission_date
             :id :external_records
             :priority :priority}))

(def rfc-records
  (extract
    :clean {:organization (extract-rel-unique :set :customer :name)
            :id (external "Remedy")}
    :merge-rules {:sc vectorize}
    :fields {:status :status
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
             :id :external_records
             :project :project}))

(def device-records
  (extract
    :clean {:incidents incident-records
            :rfcs rfc-records
            :device_ciid (external "Remedy")}
    :fields {:device_ciid :external_records
             :device_name nil
             :device_class nil
             :incidents :incidents,
             :rfcs :rfcs}))

; Problems must be done with or *after* Incidents and RFCs
(def problem-records
  (extract
    :clean {:incidents (add-by-externals "Remedy" :id)
            :related_cis (add-by-externals "Remedy" :id)
            :rfcs (add-by-externals "Remedy" :id)
            :id (external "Remedy")
            :organization (extract-rel-unique :add :customer :name)
            :support_group (extract-rel-unique :add :support_group :name)}
    :fields {:status              nil                  ;#{nil},
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
             }))
