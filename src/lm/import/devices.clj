(ns lm.import.devices
  (:require [xn.client :as xn]
            [xn.import :as i :refer [extract external extract-rel-unique
                                     create-unique]]
            [clojure.string :as s]
            [xn.repl :refer [prident]]
            [xn.tools :refer [vectorize get-some lower-case]]))


(def class-name-map {"Remedy::Server"           :server,
                     "Remedy::MainframePrinter" :server,
                     "Remedy::Appliance"        :server,
                     "Remedy::VirtualServer"    :vm,
                     "Remedy::BladeChassis"     :blade_chassis,
                     "Remedy::StorageDevice"    :storage_device,
                     "Remedy::Mainframe"        :server,
                     "Remedy::SdeVirtualServer" :vm})

(def cc-map {["Hardware" "Network" "Front End Processor"]   :server,
             ["Hardware" "Network" "Router"]                :router,
             ["Hardware" "Network" "Firewall"]              :firewall,
             ["Hardware" "Network" "Load Balancer"]         :load_balancer,
             ["Hardware" "Network" "Virtual Load Balancer"] :virtual_load_balancer,
             ["Hardware" "Network" "Virtual Firewall"]      :firewall_context,
             ["Hardware" "Network" "Access Gateway"]        :firewall,
             ["Hardware" "Network" "Controller"]            :firewall,
             ["Hardware" "Network" "Switch Line Card"]      :line_card,
             ["Hardware" "Network" "Console"]               :remote_console,
             ["Hardware" "Network" "IDS/IPS"]               :server,
             ["Hardware" "Network" "Virtual Switch"]        :virtual_switch,
             ["Hardware" "Network" "Infrastructure"]        :server,
             ["Hardware" "Network" "NAM"]                   :nam_switch_module })

(def model-cleanup
  {[nil "C3750"                                                              ] [ "Cisco" "Catalyst 3750" :standalone_switch]
  [nil "CAT2900"                                                             ] [ "Cisco" "Catalyst 2900" :standalone_switch]
  [nil "CAT2924-XL"                                                          ] [ "Cisco" "Catalyst 2924-XL" :standalone_switch]
  [nil "CAT2950G"                                                            ] [ "Cisco" "Catalyst 2950G" :standalone_switch]
  [nil "CAT3000"                                                             ] [ "Cisco" "Catalyst 3000" :standalone_switch]
  [nil "CAT3100"                                                             ] [ "Cisco" "Catalyst 3100" :standalone_switch]
  [nil "CAT3200"                                                             ] [ "Cisco" "Catalyst 3200" :standalone_switch]
  [nil "CAT3750G"                                                            ] [ "Cisco" "Catalyst 3750G" :standalone_switch]
  [nil "CAT4000"                                                             ] [ "Cisco" "Catalyst 4000" :standalone_switch]
  [nil "CAT5000"                                                             ] [ "Cisco" "Catalyst 5000" :standalone_switch]
  [nil "CAT6506"                                                             ] [ "Cisco" "Catalyst 6506" :standalone_switch]
  [nil "CAT6509"                                                             ] [ "Cisco" "Catalyst 6509" :standalone_switch]
  [nil "CIGSM2950"                                                           ] [ "Cisco" "CIGSM2950" :blade_switch]
  [nil "cisco nexus 2248 fabric extender"                                    ] [ "Cisco" "Cisco Nexus 2248" :fabric_extender]
  [nil "console switch"                                                      ] [ "Raritan" "console switch" :remote_console]
  [nil "raritan dominion kxii kvm switch"                                    ] [ "Raritan" "Raritan dominion kxii KVM switch" :remote_console]
  ["cdw canada inc" "console switch"                                         ] [ "Raritan" "console switch" :remote_console]
  ["cisco" "2960"                                                            ] [ "Cisco" "2960" :standalone_switch]
  ["cisco" "2960G"                                                           ] [ "Cisco" "2960G" :standalone_switch]
  ["cisco" "3400"                                                            ] [ "Cisco" "3400" :standalone_switch]
  ["cisco" "3750"                                                            ] [ "Cisco" "3750" :standalone_switch]
  ["cisco" "6500"                                                            ] [ "Cisco" "6500" :switch_chassis]
  ["cisco" "6509 chassis"                                                    ] [ "Cisco" "6509 chassis" :switch_chassis]
  ["cisco" "C2950"                                                           ] [ "Cisco" "C2950" :standalone_switch]
  ["cisco" "C2960G"                                                          ] [ "Cisco" "C2960G" :standalone_switch]
  ["cisco" "C3750G"                                                          ] [ "Cisco" "C3750G" :standalone_switch]
  ["cisco" "C3750G 48TS"                                                     ] [ "Cisco" "C3750G 48TS" :standalone_switch]
  ["cisco" "CAT3750G"                                                        ] [ "Cisco" "CAT3750G" :standalone_switch]
  ["cisco" "CAT6504"                                                         ] [ "Cisco" "CAT6504" :switch_chassis]
  ["cisco" "CAT6509"                                                         ] [ "Cisco" "CAT6509" :switch_chassis]
  ["cisco" "CIGSM2950"                                                       ] [ "Cisco" "CIGSM2950" :standalone_switch]
  ["cisco" "CSS11503"                                                        ] [ "Cisco" "CSS11503" :lb_switch_module]
  ["cisco" "WS-C2950G-48-EI"                                                 ] [ "Cisco" "WS-C2950G-48-EI" :standalone_switch]
  ["cisco" "WS-C3750G-24TS-E"                                                ] [ "Cisco" "WS-C3750G-24TS-E" :standalone_switch]
  ["cisco" "cat6500 48 port"                                                 ] [ "Cisco" "cat6500 48 port" :switch_chassis]
  ["cisco" "catalyst 2950"                                                   ] [ "Cisco" "Catalyst 2950" :standalone_switch]
  ["cisco" "catalyst 3560x switch"                                           ] [ "Cisco" "Catalyst 3560x switch" :standalone_switch]
  ["cisco" "catalyst 3560x-24"                                               ] [ "Cisco" "Catalyst 3560x-24" :standalone_switch]
  ["cisco" "catalyst 3750"                                                   ] [ "Cisco" "Catalyst 3750" :standalone_switch]
  ["cisco" "catalyst 3750-24ts-s switch"                                     ] [ "Cisco" "Catalyst 3750-24ts-s switch" :standalone_switch]
  ["cisco" "catalyst 3750g-48ts switch"                                      ] [ "Cisco" "Catalyst 3750g-48ts switch" :standalone_switch]
  ["cisco" "catalyst 3750x"                                                  ] [ "Cisco" "Catalyst 3750x" :standalone_switch]
  ["cisco" "catalyst 6506-e switch chassis"                                  ] [ "Cisco" "Catalyst 6506-e switch chassis" :switch_chassis]
  ["cisco" "catalyst 6506e"                                                  ] [ "Cisco" "Catalyst 6506e" :switch_chassis]
  ["cisco" "catalyst 6509-v-e chassis"                                       ] [ "Cisco" "Catalyst 6509-v-e chassis" :switch_chassis]
  ["cisco" "catalyst switch"                                                 ] [ "Cisco" "Catalyst switch" :standalone_switch]
  ["cisco" "catalyst-3750_ios"                                               ] [ "Cisco" "Catalyst-3750_ios" :standalone_switch]
  ["cisco" "cisco 1841"                                                      ] [ "Cisco" "ISR 1841" :router]
  ["cisco" "cisco 2232 pp fex"                                               ] [ "Cisco" "2232 pp fex" :fabric_extender]
  ["cisco" "cisco 2248 tp fex"                                               ] [ "Cisco" "2248 tp fex" :fabric_extender]
  ["cisco" "cisco 2248 tp-e fex"                                             ] [ "Cisco" "2248 tp-e fex" :fabric_extender]
  ["cisco" "cisco 2800"                                                      ] [ "Cisco" "ISR 2800" :router]
  ["cisco" "cisco 3750"                                                      ] [ "Cisco" "3750" :standalone_switch]
  ["cisco" "cisco 6504"                                                      ] [ "Cisco" "6504" :switch_chassis]
  ["cisco" "cisco ace 4g 6504"                                               ] [ "Cisco" "Ace 4g 6504" :switch_chassis]
  ["cisco" "cisco ace 4g 6504 switch"                                        ] [ "Cisco" "Ace 4g 6504 switch" :switch_chassis]
  ["cisco" "cisco application control engine"                                ] [ "Cisco" "application control engine" :lb_switch_module]
  ["cisco" "cisco catalyst  3750x switch"                                    ] [ "Cisco" "Catalyst  3750x switch" :standalone_switch]
  ["cisco" "cisco catalyst 3560 switch w/24 ports"                           ] [ "Cisco" "Catalyst 3560 switch w/24 ports" :standalone_switch]
  ["cisco" "cisco catalyst 3560 switch w/48 ports"                           ] [ "Cisco" "Catalyst 3560 switch w/48 ports" :standalone_switch]
  ["cisco" "cisco catalyst 3560x switch"                                     ] [ "Cisco" "Catalyst 3560x switch" :standalone_switch]
  ["cisco" "cisco catalyst 3750"                                             ] [ "Cisco" "Catalyst 3750" :standalone_switch]
  ["cisco" "cisco catalyst 3750 switch"                                      ] [ "Cisco" "Catalyst 3750 switch" :standalone_switch]
  ["cisco" "cisco catalyst 3750 switch w/24 ports"                           ] [ "Cisco" "Catalyst 3750 switch w/24 ports" :standalone_switch]
  ["cisco" "cisco catalyst 3750 switch w/48 ports"                           ] [ "Cisco" "Catalyst 3750 switch w/48 ports" :standalone_switch]
  ["cisco" "cisco catalyst 3750x 48 port switch"                             ] [ "Cisco" "Catalyst 3750x 48 port switch" :standalone_switch]
  ["cisco" "cisco catalyst 3750x switch /w 48 ports"                         ] [ "Cisco" "Catalyst 3750x switch /w 48 ports" :standalone_switch]
  ["cisco" "cisco catalyst 3750x switch w/ 48 ports"                         ] [ "Cisco" "Catalyst 3750x switch w/ 48 ports" :standalone_switch]
  ["cisco" "cisco catalyst 3750x48 port switch"                              ] [ "Cisco" "Catalyst 3750x48 port switch" :standalone_switch]
  ["cisco" "cisco catalyst 3750x48 port switch]"                             ] [ "Cisco" "Catalyst 3750x48 port switch" :standalone_switch]
  ["cisco" "cisco catalyst 6506-e switch chassis"                            ] [ "Cisco" "Catalyst 6506-e switch chassis" :switch_chassis]
  ["cisco" "cisco catalyst 6509 switch"                                      ] [ "Cisco" "Catalyst 6509 switch" :switch_chassis]
  ["cisco" "cisco catalyst switch 3750x-24t-s"                               ] [ "Cisco" "Catalyst switch 3750x-24t-s" :standalone_switch]
  ["cisco" "cisco nexus 2232 fabric extender -gdc"                           ] [ "Cisco" "Nexus 2232 fabric extender -gdc" :fabric_extender]
  ["cisco" "cisco nexus 2248 fabric extender"                                ] [ "Cisco" "Nexus 2248 fabric extender" :fabric_extender]
  ["cisco" "cisco nexus 2248 fabric extender  w/ cabc13-c14 power cords-gdc" ] [ "Cisco" "Nexus 2248 fabric extender  w/ cabc13-c14 power cords-gdc" :fabric_extender]
  ["cisco" "cisco nexus 2248 fabric extender -gdc"                           ] [ "Cisco" "Nexus 2248 fabric extender -gdc" :fabric_extender]
  ["cisco" "cisco nexus 5020 switch"                                         ] [ "Cisco" "Nexus 5020 switch" :standalone_switch]
  ["cisco" "cisco nexus 5548 switch"                                         ] [ "Cisco" "Nexus 5548 switch" :standalone_switch]
  ["cisco" "cisco nexus 5548p switch"                                        ] [ "Cisco" "Nexus 5548p switch" :standalone_switch]
  ["cisco" "cisco nexus 5596 switch\n"                                       ] [ "Cisco" "Nexus 5596 switch\n" :standalone_switch]
  ["cisco" "cisco nexus 5596 switch w/ 3 n55-m16p cards-gdc"                 ] [ "Cisco" "Nexus 5596 switch w/ 3 n55-m16p cards-gdc" :standalone_switch]
  ["cisco" "cisco nexus 7009 series switch"                                  ] [ "Cisco" "Nexus 7009 series switch" :switch_chassis]
  ["cisco" "cisco nexus fabric extender"                                     ] [ "Cisco" "Nexus fabric extender" :fabric_extender]
  ["cisco" "cisco nexus fabric extender\n"                                   ] [ "Cisco" "Nexus fabric extender\n" :fabric_extender]
  ["cisco" "cisco ws-c2950t-48-si"                                           ] [ "Cisco" "ws-c2950t-48-si" :standalone_switch]
  ["cisco" "cisco ws-c3750g switch"                                          ] [ "Cisco" "ws-c3750g switch" :standalone_switch]
  ["cisco" "cisco ws-c3750g-24ts-e"                                          ] [ "Cisco" "ws-c3750g-24ts-e" :standalone_switch]
  ["cisco" "cisco ws-c3750g-48ts-e"                                          ] [ "Cisco" "ws-c3750g-48ts-e" :standalone_switch]
  ["cisco" "cisco ws-c6506"                                                  ] [ "Cisco" "ws-c6506" :switch_chassis]
  ["cisco" "cisco ws-c6509"                                                  ] [ "Cisco" "ws-c6509" :switch_chassis]
  ["cisco" "cisco ws-c6509-1300ac="                                          ] [ "Cisco" "ws-c6509-1300ac=" :switch_chassis]
  ["cisco" "cisco ws-c6509-e"                                                ] [ "Cisco" "ws-c6509-e" :switch_chassis]
  ["cisco" "cisco ws-c6513"                                                  ] [ "Cisco" "ws-c6513" :switch_chassis]
  ["cisco" "ibm cat6500"                                                     ] [ "Cisco" "IBM cat6500" :switch_chassis]
  ["cisco" "ibm cisco 6504"                                                  ] [ "Cisco" "IBM Cisco 6504" :switch_chassis]
  ["cisco" "ibm cisco cat3750"                                               ] [ "Cisco" "IBM Cisco cat3750" :standalone_switch]
  ["cisco" "ibm cisco cat6500"                                               ] [ "Cisco" "IBM Cisco cat6500" :switch_chassis]
  ["cisco" "ibm css11503-ac"                                                 ] [ "Cisco" "IBM css11503-ac" :lb_switch_module]
  ["cisco" "nexus"                                                           ] [ "Cisco" "Nexus" :standalone_switch]
  ["cisco" "nexus 2248 fabric extender"                                      ] [ "Cisco" "Nexus 2248 fabric extender" :fabric_extender]
  ["cisco" "nexus 5010 solutions kit"                                        ] [ "Cisco" "Nexus 5010 solutions kit" :standalone_switch]
  ["cisco" "nexus 5010 switch"                                               ] [ "Cisco" "Nexus 5010 switch" :standalone_switch]
  ["cisco" "nexus 5020"                                                      ] [ "Cisco" "Nexus 5020" :standalone_switch]
  ["cisco" "nexus 5020 switch"                                               ] [ "Cisco" "Nexus 5020 switch" :standalone_switch]
  ["cisco" "nexus 5020 w/fex"                                                ] [ "Cisco" "Nexus 5020 w/fex" :standalone_switch]
  ["cisco" "nexus 5548p"                                                     ] [ "Cisco" "Nexus 5548p" :standalone_switch]
  ["cisco" "nexus 5548p switch"                                              ] [ "Cisco" "Nexus 5548p switch" :standalone_switch]
  ["cisco" "nexus 5584p switch"                                              ] [ "Cisco" "Nexus 5584p switch" :standalone_switch]
  ["cisco" "nexus 5596up"                                                    ] [ "Cisco" "Nexus 5596up" :standalone_switch]
  ["cisco" "switch"                                                          ] [ "Cisco" "switch" :standalone_switch]
  ["cisco" "ws-c3750-48ts-s"                                                 ] [ "Cisco" "ws-c3750-48ts-s" :standalone_switch]
  ["f5 networks" nil                                                         ] [ "F5 Networks" nil :load_balancer]
  ["f5 networks" "switch f5"                                                 ] [ "F5 Networks" "F5" :load_balancer]
  ["hewlett-packard" "AF617A"                                                ] [ "HP" "AF617A" :remote_console]
  ["hp" "hp kvm switch"                                                      ] [ "HP" "KVM switch" :remote_console]
  ["hp" "hp procurve 6600 switch"                                            ] [ "HP" "Procurve 6600 switch" :standalone_switch]
  ["hp" "procurve"                                                           ] [ "HP" "Procurve" :standalone_switch]
  ["ibm" "catalyst 3750"                                                     ] [ "IBM" "Catalyst 3750" :standalone_switch]
  ["ibm" "catalyst 4506"                                                     ] [ "IBM" "Catalyst 4506" :standalone_switch]
  ["ibm" "catalyst 6504e"                                                    ] [ "IBM" "Catalyst 6504e" :switch_chassis]
  ["ibm" "catalyst 6509 ve"                                                  ] [ "IBM" "Catalyst 6509 ve" :switch_chassis]
  ["ibm" "cisco catalyst 3750 switch"                                        ] [ "IBM" "Cisco catalyst 3750 switch" :standalone_switch]
  ["ibm" "cisco catalyst 4948 switch"                                        ] [ "IBM" "Cisco catalyst 4948 switch" :standalone_switch]
  ["ibm" "cisco nexus 5020 switch"                                           ] [ "IBM" "Cisco nexus 5020 switch" :standalone_switch]
  ["ibm" "cisco nexus 5548 switch"                                           ] [ "IBM" "Cisco nexus 5548 switch" :standalone_switch]
  ["ibm" "cisco-catalyst 3750 g switch"                                      ] [ "IBM" "Cisco-catalyst 3750 g switch" :standalone_switch]
  ["ibm" "gss 4492r"                                                         ] [ "IBM" "Gss 4492r" :load_balancer]
  ["ibm" "ibm catalyst 3750- 24 ts"                                          ] [ "IBM" "Catalyst 3750- 24 ts" :standalone_switch]
  ["ibm" "ibm catalyst 3750- 48 ts"                                          ] [ "IBM" "Catalyst 3750- 48 ts" :standalone_switch]
  ["ibm" "ibm catalyst 6500/48 port module"                                  ] [ "IBM" "Catalyst 6500/48 port module" :switch_chassis]
  ["ibm" "ibm cisco cat3750"                                                 ] [ "IBM" "Cisco cat3750" :standalone_switch]
  ["ibm" "ibm console switch (2x16)"                                         ] [ "IBM" "console switch (2x16)" :remote_console]
  ["ibm" "ibm global 2x16 console manager"                                   ] [ "IBM" "Global 2x16 Console Manager" :remote_console]
  ["ibm" "nam2220"                                                           ] [ "IBM" "nam2220" :server]
  ["ibm" "nam2220 - applicance"                                              ] [ "IBM" "nam2220 - applicance" :server]
  ["ibm" "nexus 2148"                                                        ] [ "IBM" "Nexus 2148" :fabric_extender]
  ["ibm" "nexus 7010 bundle"                                                 ] [ "IBM" "Nexus 7010 bundle" :switch_chassis]
  ["ibm" "nexus 7010 lab bundle"                                             ] [ "IBM" "Nexus 7010 lab bundle" :switch_chassis]
  ["ibm" "nexus 7018"                                                        ] [ "IBM" "Nexus 7018" :switch_chassis]
  ["raritan" "console switch"                                                ] [ "Raritan" "console switch" :remote_console]
  ["raritan" "dominion"                                                      ] [ "Raritan" "Dominion" :remote_console]
  ["raritan" "raritan dominion serial switch"                                ] [ "Raritan" "Dominion serial switch" :remote_console]
  ["raritan" "raritan dominion sx16 switch"                                  ] [ "Raritan" "Dominion sx16 switch" :remote_console]})

(def ip-records
  (extract :template {:CREATE :ip :UNIQUE :name}
           :clean {:id (external "Remedy")}
           :fields {:id            :external_ids
                    :name          :name
                    :description   :description
                    :ccl1          nil
                    :ccl2          nil
                    :ccl3          nil}))

(defn interfaces-with-ips [ips]
  {:add (map-indexed (fn [n record]
                       {:CREATE :interface
                        :name (str "eth" n)
                        :ip record})
                     (ip-records ips))})

(defn model-record [manufacturer model model-num]
  (if (and model manufacturer)
    {:model {:add {:CREATE :device_model
                   :UNIQUE :name
                   :name (or model model-num)
                   :model_number (when-not model model-num)
                   :manufacturer (when manufacturer
                                   {:set {:CREATE :manufacturer :UNIQUE :name
                                          :name manufacturer}})}}}))

(def device-records
  (extract
    :reader i/json-lines
    :run create-unique
    :run-opts {:model #(:class %) :key :name
               :ignore #{:id :hpsa_id :hpsa_status :cc :class :model_number :manufacturer :ips}}
    :fields {:class                      :class
             :ccl1                       :cc
             :ccl2                       :cc
             :ccl3                       :cc
             :manufacturer               :manufacturer
             :status                     :lifecycle_status
             :status_reason              :lifecycle_status_reason
             :asset_tag_num              nil,
             :release_order_number       nil
             :sid                        nil,
             :role_description           nil
             :role                       nil
             :number_of_fiber            nil
             :number_of_ethernet         nil
             :name                       :name,
             :description                :description
             :serial_num                 :serial_number,
             :number_of_ru               :height
             :bottom_ru                  :bottom_ru
             :number_of_cpus             :cpu
             :max_memory                 :memory,
             :local_storage              nil
             :legacy_vendor              nil,
             :legacy_sla                 nil,
             :ibm_type                   nil,
             :previous_builders          nil,
             :id                         :id
             :product_name               :model
             :product_model_or_version   :model_number
             :site                       :location
             :room                       :location
             :hpsa_id                    :hpsa_id
             :hpsa_status                :hpsa_status
             :ips                        :interfaces}
    :clean {:class class-name-map
            :ips interfaces-with-ips
            :manufacturer lower-case
            :product_name lower-case
            ; can't downcase this because it was used as-is to generate the key for model-cleanup
            ;:product_model_or_version lower-case
            :site lower-case
            :room lower-case
            :name lower-case
            :hpsa_status lower-case
            :hpsa_id (external "HPSA")
            :id (external "Remedy")}
    :merge-rules {:cc vectorize
                  :location (fn [a b]
                              (cond
                                (s/blank? a) b
                                (s/blank? b) a
                                :else (str a " - " b)))}
    :post-merge {:location (extract-rel-unique :add :location :name) }
    :mappings
    [(fn [device]
       (cond-> device
         (cc-map (:cc device)) (assoc ,, :class (cc-map (:cc device)))
         (not (:model device)) (assoc ,, :model (:model_number device))
         (:hpsa_id device)     (update-in ,, [:hpsa_id] #(assoc % :status (:hpsa_status device)))))
     (fn [device]
       (assoc device :external_records (get-some device :id :hpsa_id)))
     (fn [device]
       (let [[manufacturer model class :as found] (model-cleanup ((juxt :manufacturer :model) device))]
         (merge
           (if found
             (assoc device :class class)
             device)
           (model-record (or manufacturer (:manufacturer device))
                         (or model (:model device))
                         (:model_number device)))))]
    :filters [:class]
    ))
