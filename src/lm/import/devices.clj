
;  :ips
;  []}
;
(ns lm.import.devices
  (:require [xn.client :as xn]
            [xn.import :as i]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [xn.repl :refer [info prident]]))


; create-unique -> {keyvalue id}
(defn create-unique [{:keys [model key-name]} records]
  (->> records
    (map (fn [body]
           [(body key-name)
            ((xn/execute {:method :put :url (str "/model/" (name model) "?unique=" (name key-name))
                         :body body}) 0)]))
    (into {})))

(defn merge-with-rules
  "Based on clojure.core/merge-with"
  [rules & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          (if (contains? m k)
                            (assoc m k ((rules k (:default rules last)) (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defn extract-records
  ([fields records]
   (extract-records {} {} fields records))
  ([clean-rules merge-rules fields records]
   (let [default-rule (fn [v] (cond
                                (string? v) (let [v (.trim v)] (when-not (= "" v) v))
                                (and (number? v) (zero? v)) nil
                                :else v))
         fields (if (map? fields)
                  (filter (fn [[from to]] to) fields)
                  (into {} (map vector fields fields)))]
     (->> (or records [])
       (map (fn [r]
              (->> fields
                (map (fn [[from to]]
                       (let [v (r from)
                             v ((clean-rules from (:default clean-rules default-rule)) v)]
                         {to v})))
                (apply merge-with-rules merge-rules))))
       (filter #(not-every? nil? %))
       (set)))))




(defn set-one-rels [fields records]
  (reduce (fn [records [field rels]]
            (map (fn [r] (update-in r [field] #(rels %))) records))
          records
          fields))

(defn add-many-rels [fields records])

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
      {:class class-name-map}
      {:cc (fn [a b] (if (vector? a) (conj a b) [a b]))
       :address (fn [a b] (str a " - " b))}
      {:class   :class
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
       :site                       :address
       :room                       :address
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
             (assoc r :model (:model_number r)))))
    (map (fn [r]
           (update-in r [:ips] ip-records)))))

(defn load! []
  (let [records (device-records raw)
        sources (->> [{:name "Remedy" :direction "in"}
                      {:name "HPSA" :direction "in"}]
                  (create-unique {:model :data_source :key :name}))
        remedy-id (sources "Remedy")
        hpsa-id (sources "HPSA")
        manufacturers (->> records
                        (extract-records {:manufacturer :name})
                        (filter :name)
                        (create-unique {:model :manufacturer :key :name}))
        models (->> records
                 (extract-records {:model :name :model_number :model_number :manufacturer :manufacturer})
                 (filter :name)
                 (set-one-rels {:manufacturer manufacturers})
                 (create-unique {:model :model :key :name}))
        locations (->> raw
                    (extract-records [:site :room])
                    (filter #(or (:site %) (:room %)))
                    (map (fn [{:keys [site room]}]
                           (let [addr (s/join " - " [site room])]
                             {:name addr :address addr})))
                    (create-unique {:model :location :key :name}))
        remedy (->> raw
                 (extract-records [:id])
                 (map #(assoc % :data_source remedy-id))
                 (create-unique {:model :external_record :key :id}))
        hpsa (->> raw
               (extract-records {:hpsa_id :id :hpsa_status :status})
               (filter :id)
               (map #(assoc % :data_source hpsa-id))
               (create-unique {:model :external_record :key :id}))
        devices (->> raw
                  (extract-records
                    [:name
                     :description
                     :serial_number
                     :height
                     :bottom_ru
                     :cpu
                     :memory])
                  (set-one-rels {:model models :location locations})
                  (add-many-rels {:external_records (merge remedy hpsa)})
                  (map (fn [r] ))
                  (create-unique {:model ()})
                  )]
    true
    ))


