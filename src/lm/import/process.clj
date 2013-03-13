(ns lm.import.process
  (:use xn.import xn.repl)
  (:require [xn.client :as xn]
            [clojure.string :as s]
            [xn.tools :refer [vectorize make-set lower-case]]))

; File 01
(def dc-sites
  (extract
    :reader csv
    :skip-rows 1
    :create-unique {:model :datacenter :key :name}
    :row [nil nil :description :name]))

; File 02
(def subnets
  (extract
    :reader csv
    :skip-rows 1
    :create-unique {:model :subnet :key :name}
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
    :create-unique {:model :subnet :key :name}
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
    :create-unique {:model :datacenter :key :name}
    :pre [#(assoc % :pods (count (:pods %)) )]
    :fields {:name :name
             :id :external_records}
    :clean {:id (external "NMDB")}))

; FIXME: dc-zones is not associating to DCs??
(def dc-zones
  (extract
    :reader (fn [filename]
              (letfn [(pod-zone<-dc [dcs]
                        (mapcat (fn [dc]
                                  (let [dc-name (:name dc) ]
                                    (map (fn [zone]
                                           (-> zone
                                               (assoc :parent_zone {:set {:name dc-name}})
                                               (update-in [:pod] #(str dc-name " / " %))))
                                         (apply concat (:pods dc)))))
                                dcs))]
                (-> filename json-file pod-zone<-dc)))
    :create {:model :zone}
    :fields {:name :name
             :id :external_records
             :parent_zone :parent_zone
             :pod :pod
             :subnets :subnets
             :vlans :vlans}
    :clean {:id (external "NMDB")
            :subnets (extract-rel-records
                       :add :subnet :network_address
                       :fields {:name :name
                                :subnet :network_address
                                :id :external_records
                                :direction :direction
                                :notes :description}
                       :clean {:id (external "NMDB")})
            :vlans (extract-rel-records
                     :add :vlan :name
                     :fields {:primary_vlan :primary_vlan
                              :vlan :name
                              :id :external_id
                              :direction :direction
                              :vlan_type :vlan_type
                              :notes :description}
                     :clean {:id (external "NMDB")})}
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
; * these should be created unique based on the remedy external id... how can we do that?
; * look up a map of pod/zones from existing records and make associations
(def device-model->model (atom {}))
(def pod-zone->id (atom {}))

(defn get-pod-zone->ids []
  (reset! pod-zone->id
    (reduce (fn [m [dc pod id]] (assoc m [dc pod] id))
            {}
            (concat (get-path-properties ["/is/datacenter,zone" :name "/rel/child_zones" [:name :_id]])
                    (get-path-properties ["/is/datacenter,zone" :name "/rel/child_zones" :name "/rel/child_zones" [:name :_id]])))))

(def nmdb-devices
  (extract
    :reader json-lines
    :create-unique {:model #(:class %) :key :name}
    :fields (array-map
              :class nil
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
                          :clean {:id (external "NMDB")
                                  :media (fn [v] ({"1000T" "CAT6"} v v))})
            :id (external "NMDB")
            :ciid (external "Remedy")
            :project (extract-rel-unique :add :project :name #(first (s/split % #" ")))
            :hostname lower-case }
    :merge-rules {:zone vectorize
                  :class vectorize
                  :external_records vectorize}
    :post-merge {:model (extract-rel-unique :set :model :name)
                 :class @device-model->model
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
  :create {:model #(:class %)
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
  :post-merge {:external-records (external "HPSA")
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
(def nmdb-ips (extract
  :reader json-file
  :create-unique {:model :ip :key :name}
  :pre [:ip_address]
  :fields {:AssignedToGoNet nil
           :AssignedToiServ nil
           :DNS :dns_entries
           :Description :description
           :IPSegment nil
           :IsUsed nil
           :iFace nil
           :ip :name}
  :clean {:ip (comp int read-string str)
          :DNS (extract-rel-unique :add :dns_entry :name)}))

; File 09 is not imported

; File 10
(def solutions
  (extract
    ;Make fields an array-map to ensure that fields are processed for merge in the defined order
    :reader json-lines
    :create-unique {:model :application :key :name}
    :fields (array-map
             :class nil
             :id :external_records
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
    :clean {:id (external "Remedy")
            :infrastructure_system
            (extract-rel-records
              :add :system :name
              :fields {:id :external_records
                       :name :name
                       :description :description
                       :devices :network_devices}
              :clean {:id (external "Remedy")
                      :devices (map-to-rels
                                 :add
                                 [(fn [id] {:EXTERNAL_ID (str "Remedy/" id)})
                                  :id])})}))


