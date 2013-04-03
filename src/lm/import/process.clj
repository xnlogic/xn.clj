(ns lm.import.process
  (:use xn.import xn.repl)
  (:require [xn.client :refer [get-path-properties]]
            [clojure.string :as s]
            [xn.tools :refer [vectorize make-set lower-case fix-invalid-chars]]))

(def description-chars
  {\• "* "
   \… "..."
   ;\§ "(section)"
   \return ""
   \– "--" ; 8211
   \· "-"
   \ ""
   \” "\"" ; 8220
   \“ "\"" ; 8221
   \½ "1/2"
   \¿ "?"
   (char 8222) ","
   (char 8216) "'"
   (char 8217) "'"})

; File 01
(def dc-sites
  (extract
    :reader csv
    :skip-rows 1
    :run create-unique
    :run-opts {:model :datacenter :key :name}
    :row [nil nil :description :name]))

; File 02
(def subnets
  (extract
    :reader csv
    :skip-rows 1
    :parallel true
    :run create
    :run-opts {:model :subnet}
    :row [:subnet :description :mask]
    :fields (array-map
             :subnet :network_address
             :mask :network_address
             :description :description)
    :merge-rules {:network_address (fn [a b] (str a "/" b))}
    ; Make sure the api supports ip/mask notation ie 10.10.0.0/255.255.0.0
    :mappings [(fn [r] (assoc r :name (:network_address r)))]
    ))

; File 03
(def ip-subnet
  (extract
    :reader json-file
    :parallel true
    :run create-unique
    :run-opts {:model :subnet :key :name}
    :pre [:ip_subnet]
    :clean {:primary_key (external "Remedy")}
    :fields {:description :description
             :name :network_address
             :primary_key :external_records}
    :mappings [(fn [r] (assoc r :name (:network_address r)))]
    ))

; File 04 -- run first data-centers, then dc-zones (this updates & improves dc_subnets.clj)
(def data-centers
  (extract
    :reader json-file
    :run create
    :run-opts {:model :datacenter}
    :pre [#(assoc % :pods (count (:pods %)) )]
    :fields {:name :name
             :id [:EXTERNAL_ID :external_records]}
    :post-merge {:EXTERNAL_ID (external-name "NMDB-DC")
                 :external_records (external "NMDB-DC")}))

(defn pod-zone<-dc [dcs]
  (mapcat (fn [dc]
            (let [dc-name (:name dc) ]
              (map (fn [zone]
                     (-> zone
                         (assoc :parent_zone {:set {:name dc-name}})
                         (update-in [:pod] #(str dc-name " / " %))))
                   (apply concat (:pods dc)))))
          dcs))

; On this pass, ignore the zone relationships so that we can correctly
; create all zones that do have an external ID. After that we will find
; the correct parent zones to associate or create as needed (see
; dc-zones->parents)
(def dc-zones
  (extract
    :reader (fn [filename] (-> filename json-file pod-zone<-dc))
    :run create
    :run-opts {:model :zone}
    :fields {:name :name
             :id [:external_records :EXTERNAL_ID]
             :subnets :subnets
             :vlans :vlans}
    :clean {:subnets (extract-rel-records
                       :add :subnet nil
                       :fields {:name :name
                                :subnet :network_address
                                :id [:EXTERNAL_ID :external_records]
                                :direction :direction
                                :notes :description}
                       :post-merge {:EXTERNAL_ID (external-name "NMDB-Subnet")
                                    :external_records (external "NMDB-Subnet")})
            :vlans (extract-rel-records
                     :add :vlan nil
                     :fields {:primary_vlan :primary_vlan
                              :vlan :name
                              :id [:EXTERNAL_ID :external_id]
                              :direction :direction
                              :vlan_type :vlan_type
                              :notes :description}
                     :post-merge {:EXTERNAL_ID (external-name "NMDB-VLAN")
                                  :external_records (external "NMDB-VLAN")})}
    :post-merge {:external_records (external "NMDB-Zone")
                 :EXTERNAL_ID (external-name "NMDB-Zone")}))

(def dc-zones->parents
  (extract
    :reader (fn [filename] (-> filename json-file pod-zone<-dc))
    :run update
    :run-opts {:url #(str "/model/zone/external_id/" (:EXTERNAL_ID %))}
    :fields {:id :EXTERNAL_ID
             :parent_zone :parent_zone
             :pod :pod}
    :post-merge {:EXTERNAL_ID (external-name "NMDB-Zone")}
    :mappings [(fn [r]
                   (if (= "n/a" (lower-case (:name r))) ; no heirarchy
                     (-> r
                         (assoc :name (:pod r))
                         (dissoc :pod))
                     (let [dc (:parent_zone r)] ; build zone heirarchy
                       (-> r
                           (assoc :parent_zone
                                  {:add {:CREATE :zone :UNIQUE :name
                                         :name (:pod r)
                                         :parent_zone dc}})
                           (dissoc :pod)))))]))


; File 05 -- use devices.clj

; File 06
; NOTES:
; * look up a map of pod/zones from existing records and make associations
(def device-model->model (atom {}))
(def pod-zone->id (atom {}))

(defn get-pod-zone->ids []
  (reset! pod-zone->id
    (reduce (fn [m data] (assoc m (vec (butlast data)) (last data)))
            {}
            (concat (get-path-properties ["/is/datacenter,zone" :name "/rel/child_zones" [:name :_id]])
                    (get-path-properties ["/is/datacenter,zone" :name "/rel/child_zones" :name "/rel/child_zones" [:name :_id]])))))

(def nmdb-devices
  (extract
    :reader json-lines
    :run create
    :run-opts {:model #(:class %)
               :change-model true}
    :pre [#(assoc % :EXTERNAL_ID (:ciid %))]
    :fields (array-map
              :class nil
              :EXTERNAL_ID :EXTERNAL_ID
              :id :external_records
              :ciid :external_records
              :hostname :name
              :description :description
              :datacenter :zone
              :pod        :zone
              :zone       :zone
              :project :projects
              :interfaces :interfaces
              :device_model_type         :class
              :device_model      [:model :class])
    :clean {:interfaces (extract-rel-records :add :interface nil
                          :fields {:class nil
                                   :id :id
                                   :name :name
                                   :direction :direction
                                   :type nil  ; TODO not sure about this
                                   :speed :speed
                                   :duplex :duplex
                                   :media :cable_type
                                   :device nil }
                          :clean {:id (external "NMDB-IFace")
                                  :media (fn [v] ({"1000T" "CAT6"} v v))})
            :EXTERNAL_ID (external-name "Remedy")
            :ciid (external "Remedy")
            :id (external "NMDB-Device")
            :project (extract-rel-unique :add :project :name #(first (s/split % #" ")))
            :hostname lower-case }
    :merge-rules {:zone vectorize
                  :class vectorize
                  :external_records vectorize}
    :post-merge {:model (extract-rel-unique :set :device_model :name)
                 :class (fn [c] (@device-model->model c))
                 :zone (fn [[datacenter pod zone]]
                         (when (and datacenter pod zone)
                           (if (= "n/a" zone)
                             (@pod-zone->id [datacenter pod])
                             (@pod-zone->id [datacenter pod zone]))))}
    :filters [:class]
    ))

(def ifaces-with-ip
  (extract-rel-records :add :interface nil
    :row [:name :direction :ip_address]
    :clean {:ip_address (extract-rel-unique :set :ip_address :name)}))

; File 07
;  * should be looked up by the external record
(def hpsa-servers (extract
  :reader json-lines
  :run create
  :run-opts {:model #(:class %)
             :ignore #{:is_hypervisor :management_ip :primary_ip :class}}
  :pre [:sas_server]
  :fields {:server_id [:external_records :EXTERNAL_ID]
           :display_name :name
           :is_hypervisor :is_hypervisor
           :management_ip :management_ip
           :notes :description
           :primary_ip :primary_ip
           :reported_os :software
           :virtualization_type_id :virtualization_type_id}
  :clean {:display_name lower-case
          :reported_os (map-to-rels :add [(fn [n] {:CREATE :software :UNIQUE :name
                                                   :name n :type "Operating System" })])}
  :post-merge {:external_records (external "HPSA")
               :EXTERNAL_ID (external-name "HPSA")}
  :filters [:class]
  :mappings [(fn [r]
               (assoc r :class
                      (cond (= "Y" (:is_hypervisor r))  :vm_host
                            (:virtualization_type_id r) :vm
                            :else                       :server)))
             (fn [r] (assoc r :interfaces
                            (if (= (:management_ip r) (:primary_ip r))
                              (ifaces-with-ip [["eth0" "User-Facing" (:management_ip r)]])
                              (ifaces-with-ip [["eth0" "User-Facing" (:primary_ip r)]
                                               ["eth1" "Management" (:management_ip r)]]))))]))

; File 08
(def nmdb-ips
  (extract
    :reader json-file
    :parallel true
    :run create-unique
    :run-opts {:model :ip :key :name}
    :pre [:ip_address]
    :fields {:DNS :dns_entries
             :Description :description
             :IPSegment nil ; TODO: is this int actually a FK?
             :IsUsed nil
             :iFace nil
             :ip [:external_records :name]}
    :post-merge {:external_records (external "NMDB-IP")
                 :name (fn [v]
                         (try ((comp long read-string str) v)
                           (catch Exception e nil)))
                 :DNS (extract-rel-unique :add :dns_entry :name)}))

; File 09 is not imported


; File 10
(def solutions
  (extract
    ;Make fields an array-map to ensure that fields are processed for merge in the defined order
    :reader json-lines
    :run create
    :run-opts {:model :application}
    :fields (array-map
             :class nil
             :id [:EXTERNAL_ID :external_records]
             :name :name
             :description :description
             :recovery_strategy nil,
             :recovery_time_objectives :recovery_time_objective
             :public_facing nil,
             :data_sensitivity_rating :data_sensitivity,
             :critical_business_period nil
             :change_window nil
             :availability_supported :availability
             :availability_required :availability
             :num_of_users nil
             :infrastructure_system :runs_on_system)
    :merge-rules {:availability (fn [supported required] (or required supported))}
    :clean {:infrastructure_system
            (extract-rel-records
              :add :system nil
              :fields {:id [:EXTERNAL_ID :external_records]
                       :name :name
                       :description :description
                       :devices :network_devices
                       :ccl1 :ccl1}
              :post-merge {:EXTERNAL_ID (external-name "Remedy")
                           :external_records (external "Remedy")
                           :network_devices (add-by-externals "Remedy" :id)}
              :filters [#(= (:ccl1 %) "View")]) ; Note: There are a lot of 'System' records that are actually hardware. Try this search: \<InfrastructureSystem\>[^[}]\{-}ccl1":"Hardware/e
            :description (fix-invalid-chars description-chars)}
    :post-merge {:EXTERNAL_ID (external-name "Remedy")
                 :external_records (external "Remedy")}))

; HPSA records with non-unique IDs
;
; [ "HPSA/115160001",
;   "HPSA/105430001",
;   "HPSA/100510001",
;   "HPSA/116800001",
;   "HPSA/10001",
;   "HPSA/40001",
;   "HPSA/50001",
;   "HPSA/20001",
;   "HPSA/30001",
;   "HPSA/160001" ]

(def remedy-projects
  (extract
    :reader json-lines
    :run create-unique
    :run-opts {:model :project :key :name}
    :fields {:id :external_records
             :name :name
             :description :description
             :systems :systems,
             :device_cis :assets}
    :clean  {:id (external "Remedy")
             :name lower-case
             :description (fix-invalid-chars description-chars)
             :systems
             (extract-rel-records
               :add :system nil
               :fields {:id [:EXTERNAL_ID :external_records]
                        :name :name
                        :description :description
                       ;TODO: define properties or extract rel?
                       ;:supported_hours nil,
                       ;:change_windows nil
                        }
               :post-merge  {:EXTERNAL_ID (external-name "Remedy")
                             :external_records (external "Remedy")})
             :device_cis (add-by-externals "Remedy" :id)}))


; Problems must be done with or *after* Incidents and RFCs
(def remedy-problems
  (extract
    :reader json-lines
    :run create-unique
    :run-opts {:model :problem :key :name}
    :fields {:status              nil
             :source_code         nil
             :class               nil
             :incidents           :incidents
             :submitter           :submitter
             :support_group       :support_group
             :organization        :customer
             :short_description   :name
             :related_cis         :applies_to
             :submit_date         :submission_date
             :id                  :external_records
             :description         :description
             :rfcs                :rfcs
             :priority            :priority
             }
    :clean {:incidents (add-by-externals "Remedy" :id)
            :related_cis (add-by-externals "Remedy" :id)
            :rfcs (add-by-externals "Remedy" :id)
            :id (external "Remedy")
            :organization (extract-rel-unique :add :customer :name)
            :support_group (extract-rel-unique :add :support_group :name)
            :description (fix-invalid-chars description-chars)
            }
    ))

(def remedy-releases
  (extract
    :reader json-lines
    :run create-unique
    :run-opts {:model :release :key :name}
    :fields {:id [:name :external_records]
             :status nil,
             :organization :customer
             :submitter :submitter
             :submit_date :submission_date
             :support_group :support_group
             ;:go_live_date_proposed "2005-06-30T00:00:00-04:00",
             ;:go_live_date_actual "2006-06-30T00:00:00-04:00",
             :description :description
             :rfcs :rfcs}
    :clean {:organization (extract-rel-unique :set :customer :name)
            :description (fix-invalid-chars description-chars)}
    :post-merge {:external_records (external "Remedy")
                 :rfcs (add-by-externals "Remedy" :id)
                 :support_group (extract-rel-unique :set :support_group :name)}))

(def remedy-support-groups
  (extract
    :reader json-lines
    :run create
    :run-opts {:model :support_group}
    :fields {:status nil
             :class nil
             :name :name
             :organization :customer
             :ministry nil
             :support_group_type nil
             :support_organization nil
             :id [:EXTERNAL_ID :external_records]
             :support_group_role nil}
    :post-merge {:customer (extract-rel-unique :set :customer :name)
                 :external_records (external "Remedy")
                 :EXTERNAL_ID (external-name "Remedy")
                 }))

(def remedy-devices->itsm
  (extract
    :reader json-lines
    :run update
    :run-opts {:url #(str "/is/managed/external_id/" (:EXTERNAL_ID %))}
    :fields {:device_ciid :EXTERNAL_ID
             :incidents :incidents,
             :rfcs :rfcs}
    :clean {:incidents
            (extract-rel-records
              :add :incident nil
              :fields {:status :status
                       :impact :impact
                       :class nil
                       :submitter :submitter
                       :support_group :support_group
                       :organization :customer
                       :short_description :description
                       :urgency :urgency
                       :submit_date :submission_date
                       :id [:EXTERNAL_ID :external_records]
                       :priority :priority}
              :post-merge {:customer (extract-rel-unique :set :customer :name)
                           :description (fix-invalid-chars description-chars)
                           :support_group (extract-rel-unique :set :support_group :name)
                           :EXTERNAL_ID (external-name "Remedy")
                           :external_records (external "Remedy")})
            :rfcs
            (extract-rel-records
              :add :rfc :name
              :fields {:status :status
                       :class nil
                       :submitter :submitter
                       :submitter_email nil
                       :organization :customer
                       :short_description :description
                       :submitter_full_name nil
                       :submit_date :submission_date
                       :id [:EXTERNAL_ID :external_records]
                       :project :project}
              :post-merge {:customer (extract-rel-unique :set :customer :name)
                           :description (fix-invalid-chars description-chars)
                           :external_records (external "Remedy")
                           :EXTERNAL_ID (external-name "Remedy")})
            :device_ciid (external-name "Remedy")}
    ))

(def dc->subnets
  (extract
    :reader (fn [file]
              (->> file
                   csv
                   (drop 1)
                   (group-by first)
                   (map (fn [[dc subnets]] {:name dc :subnets (map last subnets)}))))
    :run create-unique
    :run-opts {:model :datacenter :key :name}
    :fields {:name :name
             :subnets :subnets}
    :clean {:subnets
            (extract-rel-unique :add :subnet :network_address)}))
