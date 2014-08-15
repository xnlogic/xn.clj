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
   :nat "./nat"                       ; true/false indicating presence of a nat

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

(defn computes [file]
  (->> (clojure.java.io/input-stream file)
       find-items
       (map (item-fields fields))
       (map ->network-address)
       (map (fn [{:as item :keys [nat]}]
              (cond-> item
                (= "false" nat) (dissoc :nat-ip)
                true (dissoc :nat))))))

(defn existing-external [field ->ext-name]
  (fn [item]
    (let [ext-name (->ext-name (field item))
          exts (get-in item [:existing-vm :external_records])
          ext (some #(when (= (:name %) ext-name) %) exts)]
      (if ext
        ; remove the external from both places. leftover external records will be deleted and
        ; leftover values in the field will be created.
        (-> item
            (dissoc field)
            (update-in [:existing-vm :external_records] #(remove #{ext} %)))
        item))))

(defn existing-vms [items]
  ; full,full to also get externals and ifaces and software
  (let [vms (xn/get-map :name {:url "/model/cloud_vm" :query {:format "full,full"}})]
    (->> items
         (map (fn [{:as item :keys [name]}]
           (assoc item :existing-vm (vms name))))
         (map (existing-external :ciid (external-name "savvis_ciid")))
         (map (existing-external :savvis-name (external-name "savvis_hostname"))))))

(defn ip-iface [field iface-name]
  (fn [{:as item :keys [existing-vm]}]
    (let [ifaces (map (fn [iface] [(:name iface) (get-in iface [:ip :name]) iface])
                      (get-in existing-vm [:interfaces]))
          ip-exists? (set (map second ifaces))
          iface (some #(when (= iface-name (second %)) (last %)) ifaces)]
      (cond
        (ip-exists? (field item))
        (dissoc item field)
        iface
        (-> item
            (dissoc field)
            (update-in [:change-ips] conj {:url (get-in iface [:meta :xnid])
                                           :ip (field item)}))
        :else
        item))))

(defn mix-with-existing [records]
  (->> records
       existing-vms
       (map (ip-iface :ip "eth0"))
       (map (ip-iface :nat-ip "nat"))))

(def create-vms
  (extract
    :reader (comp mix-with-existing computes)
    :run create-unique
    :run-opts {:model :cloud_vm :key :name}
    :pre [#(cond-> %
             (:ip %) (assoc :interfaces [(select-keys % [:ip :network-address :subnet-name])])
             (:nat-ip %) (assoc :nat-ifaces [(select-keys % [:nat-ip])])
             true (dissoc :ip :network-address :subnet-name :nat-ip))]
    :fields (conj (keys fields) :interfaces :nat-ifaces )
    :clean {:cpu #(Integer/parseInt %)
            :memory #(Integer/parseInt %)
            :ciid (external "savvis_ciid")
            :savvis-name (external "savvis_hostname")
            :software (extract-rel-unique :add :software :name)
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
                     true (dissoc :savvis-name :ciid))))
               (fn [{:as vm :keys [interfaces nat-ifaces]}]
                 (dissoc
                   (cond
                     (and interfaces nat-ifaces)
                     (update-in vm [:interfaces :add] conj (first (:add nat-ifaces)))
                     nat-ifaces
                     (assoc vm :interfaces nat-ifaces)
                     :else vm)
                   :nat-ifaces))]))

(defn invalid-external-records [records]
  (->> records
       (mapcat #(get-in % [:existing-vm :external_records]))
       (map #(get-in % [:meta :xnid]))))

(defn remove-invalid-external-records [records]
  (doseq [xnid (invalid-external-records records)]
    (xn/make-request {:method :delete :url xnid})))

(def update-changed-ips
  (extract
    :run update
    :run-opts {:url :url :ignore [:url]}
    :fields [:ip :url]
    :clean {:ip (extract-rel-unique :set :ip :name)}))

(defn subnet-ips [records]
  (->> records
       (group-by #(select-keys % [:network-address :subnet-name]))
       (map (fn [[k group]] (assoc k :ips (map :ip group))))))

(def add-subnets-to-ips
  (extract
    :run create-unique
    :run-opts {:model :subnet :key :network_address}
    :fields {:network-address :network_address
             :subnet-name :name
             :ips :ips}
    :clean {:ips (extract-rel-unique :add :ip :name)}
    ))

(defn run-import [files]
  (create-data-sources "savvis_ciid" "savvis_hostname")
  (doseq [file files]
    (let [records (computes file)
          mixed (mix-with-existing records)]
      (create-vms mixed :execute true)
      (update-changed-ips (mapcat :change-ips mixed) :execute true)
      (add-subnets-to-ips (subnet-ips records) :execute true)
      (remove-invalid-external-records mixed))))
