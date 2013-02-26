(ns lm.import.devices
  (:require [xn.client :as xn]
            [xn.import :as i :refer [extract-records create-unique set-one-rels add-many-rels]]
            [xn.repl :refer [info prident]]))



(def class-name-map {"Remedy::Server" :server,
                     "Remedy::MainframePrinter" :server,
                     "Remedy::Appliance" :server,
                     "Remedy::VirtualServer" :vm,
                     "Remedy::BladeChassis" :blade_chassis,
                     "Remedy::StorageDevice" :storage_device,
                     "Remedy::Mainframe" :server,
                     "Remedy::SdeVirtualServer" :vm})

(def cc-map {["Hardware" "Network" "Front End Processor"] :server,
             ["Hardware" "Network" "Router"] :router,
             ["Hardware" "Network" "Switch"] :switch,
             ["Hardware" "Network" "Firewall"] :firewall,
             ["Hardware" "Network" "Load Balancer"] :load_balancer,
             ["Hardware" "Network" "Virtual Load Balancer"] :virtual_load_balancer,
             ["Hardware" "Network" "Virtual Firewall"] :firewall_context,
             ["Hardware" "Network" "Access Gateway"] :firewall,
             ["Hardware" "Network" "Controller"] :firewall,
             ["Hardware" "Network" "sWITCH"] :switch,
             ["Hardware" "Network" "Switch Line Card"] :line_card,
             ["Hardware" "Network" "Console"] :remote_console,
             ["Hardware" "Network" "IDS/IPS"] :server,
             ["Hardware" "Network" "Virtual Switch"] :virtual_switch,
             ["Hardware" "Network" "Infrastructure"] :server,
             ["Hardware" "Network" "NAM"] :nam_switch_module })

(defn ip-records [ips]
  (->> ips
    (extract-records {:class         nil
                      :id            :id
                      :name          :name
                      :description   :description
                      :ccl1          nil
                      :ccl2          nil
                      :ccl3          nil})))

(defn device-records [records]
  (->> records
    (extract-records
      {:class class-name-map
       :ips  (fn [ips] (when ips (map :name ips)))}
      {:cc (fn [a b] (if (vector? a) (conj a b) [a b]))
       :location (fn [a b] (str a " - " b))}
      {:class                      :class
       :ccl1                       :cc
       :ccl2                       :cc
       :ccl3                       :cc
       :manufacturer               :manufacturer
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
       :ips                        :ips})
    (map (fn [r]
           (if-let [model (cc-map (:cc r))]
             (assoc r :class model)
             r)))
    (filter :class)
    (map (fn [r]
           (if (:model r)
             r
             (assoc r :model (:model_number r)))))))

(defn make-sources []
  (->> [{:name "Remedy" :direction "in"}
        {:name "HPSA" :direction "in"}]
    (create-unique {:model :data_source :key :name})))

(defn make-manufacturers [records]
  (->> records
    (extract-records {:manufacturer :name})
    (filter :name)
    (create-unique {:model :manufacturer :key :name})))

(defn load! []
  (let [records (device-records raw)
        sources (make-sources)
        remedy-id (sources "Remedy")
        hpsa-id (sources "HPSA")
        manufacturers (make-manufacturers records)
        models (->> records
                 (extract-records {:model        :name
                                   :model_number :model_number
                                   :manufacturer :manufacturer})
                 (filter :name)
                 (set-one-rels {:manufacturer manufacturers})
                 (create-unique {:model :model :key :name}))
        locations (->> records
                    (extract-records {:location :name})
                    (create-unique {:model :location :key :name}))
        remedy (->> records
                 (extract-records [:id])
                 (map #(assoc % :data_source remedy-id))
                 (create-unique {:model :external_record :key :id}))
        hpsa (->> records
               (extract-records {:hpsa_id     :id
                                 :hpsa_status :status})
               (filter :id)
               (map #(assoc % :data_source hpsa-id))
               (create-unique {:model :external_record :key :id}))
        ips (->> raw
              (mapcat (comp ip-records :ips))
              (create-unique {:model :ip, :key :name}))
        devices (->> records
                  (map #(assoc % :external_records (remove nil? ((juxt :id :hpsa_id) %))))
                  (set-one-rels {:model models :location locations})
                  (add-many-rels {:external_records (merge remedy hpsa)})
                  (add-many-rels {:ips ips})
                  (create-unique {:model #(:class %) :key :name :ignore #{:id :hpsa_id :hpsa_status :cc :class :model_number}}))]
    devices))
