(ns lm.savvis
  (:use xn.import)
  (:require [xn.client :as xn]
            [clj-xpath.core :refer [$x:node* $x:text?]]
            [clojure.pprint :refer [pprint]]
            [xn.repl :refer [prident]]
            [clojure.string :as s])
  (:import java.io.ByteArrayInputStream))

(def fields
  {:lifecycle_status "./componentStatus"
   :name "./custHostName"             ; key
   :description "./custHostDesc"
   :cpu "./cpu"
   :memory "./memory"
   :ciid "./serverCIID"               ; externals id - change = new external id, delete old one
   :savvis-name "./svvsHostName"

   :subnet-name "./serverGroupName"   ; I assume that subnet exists if the IP is unchanged.
   :default-gw "./defaultGateway"     ; for subnet if any subnet info changes, it's a new subnet
   :subnet-mask "./subnetMask"

   :ip "./ipAddr"                     ; first iface. eth0 same change rules as nat

   :nat-ip "./natIP"                  ; virtual iface name: nat. if ip changes, make new ip record and change rel

   :software "./osLabel"                    ; software, change = new record (standard importer rule)
   })

(defn- find-items [file]
  ($x:node* "//Computes/list/source/item" file))

(defn- item-fields [fields]
  (fn [item]
    (reduce (fn [record [k path]]
              (when-let [v ($x:text? path item)]
                (assoc record k v)))
            {}
            fields)))

(defn computes [file]
  (->> (clojure.java.io/input-stream file)
       find-items
       (map (item-fields fields))))

(def mask
  {"255.0.0.0" 8
   "255.128.0.0" 9
   "255.192.0.0" 10
   "255.224.0.0" 11
   "255.240.0.0" 12
   "255.248.0.0" 13
   "255.252.0.0" 14
   "255.254.0.0" 15
   "255.255.0.0" 16
   "255.255.128.0" 17
   "255.255.192.0" 18
   "255.255.224.0" 19
   "255.255.240.0" 20
   "255.255.248.0" 21
   "255.255.252.0" 22
   "255.255.254.0" 23
   "255.255.255.0" 24
   "255.255.255.128" 25
   "255.255.255.192" 26
   "255.255.255.224" 27
   "255.255.255.240" 28
   "255.255.255.248" 29
   "255.255.255.252" 30
   "255.255.255.254" 31
   "255.255.255.255" 32})

(defn ->network-address [{:as item :keys [default-gw subnet-mask]}]
  (-> item
      (assoc :network-address
             (str (s/join "." (update-in (mapv #(Integer/parseInt %)
                                               (s/split default-gw #"\."))
                                         [3] dec))
                  "/"
                  (mask subnet-mask)))
      (dissoc :default-gw :subnet-mask)))

(defn existing-external [field ->ext-name]
  (fn [item]
    (let [ext-name (->ext-name (field item))
          exts (get-in item [:existing-vm :rel :external_records])
          ext (some #(when (= (:name %) ext-name) %) exts)]
      ;(pprint [(:name item) ext-name ext (count exts)])
      (if ext
        ; remove the external from both places. leftover external records will be deleted and
        ; leftover values in the field will be created.
        (-> item
            (dissoc field)
            (update-in [:rel :external_records] #(remove #{ext} %)))
        item))))

(defn existing-vms [items]
  (let [vms (xn/get-map :name {:url "/model/vm" :query {:format "full,full"}})]
    (->> items
         (map (fn [{:as item :keys [name]}]
           (assoc item :existing-vm (vms name))))
         (map (existing-external :ciid (external-name "savvis_ciid")))
         (map (existing-external :savvis-name (external-name "savvis_hostname"))))))

(defn ip-iface [field iface-name]
  (fn [{:as item :keys [existing-vm]}]
    (pprint (get-in existing-vm [:rel :interfaces]))
    (if (some (fn [iface]
                (some #(= (field item) (:name %))
                      (get-in iface [:rel :ip])))
              (get-in existing-vm [:rel :interfaces]))
      (dissoc item field)
      item)))

(defn mix-with-existing [file]
  (->> file
       computes
       existing-vms ; full,full to also get externals and ifaces and software
       (map ->network-address)
       (map (ip-iface :ip "eth0"))
       (map (ip-iface :nat-ip "nat"))
       ))

(def vms
  (extract
    :reader mix-with-existing
    :run create-unique
    :run-opts {:model :vm :key :name
               :ignore #{}}
    :pre [#(cond-> %
             :ip (assoc :interfaces [(select-keys % [:ip :network-address :subnet-name])])
             :nat-ip (assoc :nat-ifaces [(select-keys % [:nat-ip])])
             true (dissoc :ip :network-address :subnet-name :nat-ip))]
    :fields (conj (keys fields) :interfaces :nat-ifaces )
    :clean {:cpu #(Integer/parseInt %)
            :memory #(Integer/parseInt %)
            :ciid (external "savvis_ciid")
            :savvis-name (external "savvis_hostname")
            :software (extract-rel-unique :set :software :name)
            :interfaces (extract-rel-records
                          :add :interface nil
                          :template {:name "eth0"}
                          :pre [(fn [r] {:ip r})]
                          :fields [:ip]
                          :clean {:ip (extract-rel-records
                                        :set :ip :name
                                        :pre [#(assoc % :subnet (select-keys % [:network-address :subnet-name]))]
                                        :fields {:ip :name
                                                 :subnet :subnet}
                                        :clean {:subnet (extract-rel-records
                                                          :set :subnet :network_address
                                                          :fields {:network-address :network_address
                                                                   :subnet-name :name})})})
            :nat-ifaces (extract-rel-records
                          :add :interface nil
                          :template {:name "nat"}
                          :fields {:nat-ip :ip}
                          :post-merge {:ip (extract-rel-unique :set :ip :name)}
                          )}
    :mappings [(fn [vm]
                 (let [exts (remove nil? ((juxt :savvis-name :ciid) vm))]
                   (cond-> vm
                     (seq exts) (assoc :external_records
                                       {:add exts})
                     true (dissoc :savvis-name :ciid))))]))
