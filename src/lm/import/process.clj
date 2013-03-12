(ns lm.import.process
  (:require [xn.client :as xn]
            [xn.import :as i :refer [extract create-unique extract-rel-unique
                                     external external-name add-by-externals
                                     map-to-rels extract-rel-records]]
            [clojure.string :as s]
            [xn.tools :refer [vectorize make-set lower-case]]))


; File 01
(def dc-sites
  (extract
    :row [nil nil :description :name]))

; File 02
(def subnets
  (extract
    ;Subnet,SubnetName,SubnetMask,NetworkAddress,LocationID,Street1,City,State,Country,Domain,TftpServer,DNSServers,DefaultRouters,DHCPServer,DHCPOptionTemplate,
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
    :pre [:ip_subnet]
    :clean {:primary_key (external "Remedy")}
    :fields {:description :description
             :name :network_address
             :primary_key :external_records}
    :mappings [(fn [r] (assoc r :name (:network_address r)))]
    ))

; File 04 -- update then use dc_subnets.clj

; File 05 -- use devices.clj

; File 06
; NOTES:
; * these should be created unique based on the remedy external id... how can we do that?
; * look up a map of pod/zones from existing records and make associations
(def nmdb-devices
  (extract
    :fields {:class nil
             :device_model_type :CREATE
             :id :external_records
             :ciid :external_records
             :hostname :name
             :description :description
             :pod :zone
             :zone :zone
             :project :projects
             :datacenter nil
             :interfaces :interfaces}
    :clean {:interfaces (extract
                          :template {:CREATE :interface}
                          :fields {:class nil
                                   :id :id
                                   :name :name
                                   :direction :direction
                                   :type "NETwork",
                                   :speed :speed
                                   :duplex :duplex
                                   :media :cable_type
                                   :device nil }
                          :clean {:id (external "NMDB")
                                  :media (fn [v] ({"1000T" "CAT6"} v v))})
            :id (external "NMDB")
            :ciid (external "Remedy")
            :project (extract-rel-unique :add :project :name #(first (s/split % #" ")))
            }
    :merge-rules {:zone vectorize}
    ))

(def ifaces-with-ip
  (extract-rel-records :add :interface nil
    :row [:name :direction :ip_address]
    :clean {:ip_address (extract-rel-unique :set :ip_address :name)}))

; File 07
;  * should be looked up by the external record
(def hpsa-servers (extract
  :pre [:sas_server]
  :fields (partition 2 ; Allow server_id to be mapped to 2 different fields
            [:server_id :external-records
             :server_id :EXTERNAL_ID
             :display_name :name
             :is_hypervisor :is_hypervisor
             :management_ip :management_ip
             :notes :description
             :primary_ip :primary_ip
             :reported_os :software
             :virtualization_type_id :virtualization_type_id])
  :clean {:display_name lower-case
          :reported_os (map-to-rels :add (fn [n] {:CREATE :software :UNIQUE :name
                                                  :name n :type "Operating System" }))}
  :post-merge {:external-records (external "HPSA")
               :EXTERNAL_ID (external-name "HPSA")}
  :mappings [(fn [r] (cond (= "Y" (:is_hypervisor r)) :vm_host
                           (:virtualization_type_id r) :vm
                           :else :server))
             (fn [r] (assoc r :interfaces
                            (if (= (:management_ip r) (:primary_ip r))
                              (ifaces-with-ip [["eth0" "User-Facing" (:management_ip r)]])
                              (ifaces-with-ip [["eth0" "User-Facing" (:primary_ip r)]
                                               ["eth1" "Management" (:management_ip r)]]))))]))

; File 08
(def nmdb-ips (extract
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
                      :devices (map-to-rels :add (fn [device]
                                        ; TODO
                                        {:external_records (str "Remedy/" (:id device))}))})}))


