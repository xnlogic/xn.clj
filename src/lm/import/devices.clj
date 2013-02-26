(ns lm.import.devices
  (:require [xn.client :as xn]
            [xn.import :as i :refer [extract-records create-unique set-one-rels add-many-rels]]
            [clojure.string :as s]
            [xn.repl :refer [info prident]]))


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
             ["Hardware" "Network" "Switch"]                :switch,
             ["Hardware" "Network" "Firewall"]              :firewall,
             ["Hardware" "Network" "Load Balancer"]         :load_balancer,
             ["Hardware" "Network" "Virtual Load Balancer"] :virtual_load_balancer,
             ["Hardware" "Network" "Virtual Firewall"]      :firewall_context,
             ["Hardware" "Network" "Access Gateway"]        :firewall,
             ["Hardware" "Network" "Controller"]            :firewall,
             ["Hardware" "Network" "sWITCH"]                :switch,
             ["Hardware" "Network" "Switch Line Card"]      :line_card,
             ["Hardware" "Network" "Console"]               :remote_console,
             ["Hardware" "Network" "IDS/IPS"]               :server,
             ["Hardware" "Network" "Virtual Switch"]        :virtual_switch,
             ["Hardware" "Network" "Infrastructure"]        :server,
             ["Hardware" "Network" "NAM"]                   :nam_switch_module })

(defn ip-records [ips]
  (extract-records {:class         nil
                    :id            :id
                    :name          :name
                    :description   :description
                    :ccl1          nil
                    :ccl2          nil
                    :ccl3          nil}
                   ips))

(defn lower-case [s]
  (when s (s/lower-case s)))

(defn device-records [records]
  (->> records
    (extract-records
      {:class class-name-map
       :ips ip-records
       :manufacturer lower-case
       :product_name lower-case
       :product_name_or_version lower-case
       :site lower-case
       :room lower-case
       :name lower-case
       :hpsa_status lower-case
       }
      {:cc (fn [a b] (if (vector? a) (conj a b) [a b]))
       :location (fn [a b]
                   (cond
                     (s/blank? a) b
                     (s/blank? b) a
                     :else (str a " - " b)))
       }
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

(defn make-manufacturers [records]
  (->> records
    (extract-records {:manufacturer :name})
    (create-unique {:model :manufacturer :key :name})))

(defn make-models [records]
  (let [manufacturers (make-manufacturers records)]
    (->> records
      (extract-records {:model        :name
                        :model_number :model_number
                        :manufacturer :manufacturer})
      (set-one-rels {:manufacturer manufacturers})
      (create-unique {:model :device_model :key :name}))))

(defn make-locations [records]
  (->> records
    (extract-records {:location :name})
    (create-unique {:model :location :key :name})))

(defn make-remedy [records]
  (let [remedy-id ((create-unique {:model :data_source :key :name}
                                  {:name "Remedy" :direction "in"})
                     "Remedy")]
    (->> records
      (extract-records {:id :name})
      (map #(assoc % :data_source remedy-id))
      (create-unique {:model :external_record :key :name}))))

(defn make-hpsa [records]
  (let [hpsa-id ((create-unique {:model :data_source :key :name}
                                {:name "HPSA" :direction "in"})
                   "HPSA")]
    (->> records
      (extract-records {:hpsa_id     :name
                        :hpsa_status :status})
      (map #(assoc % :data_source hpsa-id))
      (create-unique {:model :external_record :key :name}))))

(defn make-ips [ips]
  (create-unique {:model :ip, :key :name} ips))

(defn make-ifaces [ip-ids]
  (->> ip-ids
    (map-indexed (fn [n ip-id]
                   (let [[iface-id & _] (xn/execute {:method :put
                                                     :url "/model/interface"
                                                     :body {:name (str "eth" n)
                                                            :ip ip-id}})]
                     iface-id)))))

(defn add-ifaces [records]
  (->> records
    (map (fn [r]
           (->> r :ips
             make-ips
             vals
             make-ifaces
             (assoc-in r [:interfaces :add]))))))

(defn make-devices [raw & {:keys [records models locations external ifaces?] :or {ifaces? true}}]
  (let [records   (or records   (device-records raw))
        models    (or models    (make-models records))
        locations (or locations (make-locations records))
        remedy    (or external  (make-remedy records))
        hpsa      (or external  (make-hpsa records))]
    (cond->>  records
      true    (map #(assoc % :external_records (remove nil? ((juxt :id :hpsa_id) %))))
      true    (set-one-rels {:model models :location locations})
      true    (add-many-rels {:external_records (or external (merge remedy hpsa))})
      ifaces? add-ifaces
      true    (create-unique {:model #(:class %) :key :name :ignore #{:id :hpsa_id :hpsa_status :cc :class :model_number}}))))

(defn load! [filename]
  (let [raw (i/json-lines filename)]
    (make-devices raw)) )

(defn made [part key]
  (->> (xn/make-request {:url (str "is/" (name part) "/properties/" (name key))
                         :method :get
                         :query {:limit :1000000}})
    (map reverse)
    (map vec)
    (into {})))
