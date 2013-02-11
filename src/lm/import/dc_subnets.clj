(ns lm.import.dc-subnets
  (:require [xn.client :as xn]
            [xn.import :as i]
            [clojure.data.json :as json]
            [clojure.string :as s]))

(defn dcs [filename]
  (json/read-str (slurp filename) :key-fn keyword))

; file format
#_[{:datacenter ...
    :class "Nmdb::PodLocation", :id 1
    :name "...", :datacenter_xnid "..."
    :pods [[:zones-in-pod]
           [{:zone ...
             :class "Nmdb::Zone", :id 0, :name "...", :pod "..."
             :subnets
             [{:Subnet ...
               :class "Nmdb::IpSegment", :id 1, :subnet "...",
               :name "...",
               :pod "...", :zone "...",
               :direction "Other", :notes ""}]
             :vlans
             [{:Vlan ...
               :direction "Other", :notes "...",
               :vlan_type "Primary", :zone "...", :class "Nmdb::Vlan",
               :pod "...", :vlan 0,
               :primary_vlan 0, :id 1}]
             }
            ]]}]

; A Pod is a subzone to us
; A zone is a subzone of a pod
; Some subnets area already loaded but currently associated to the DC. Move them into their subzone
; vlans do not exist yet
;

(defn create-subnet [{:keys [id subnet name direction notes]} zone-id]
  (xn/execute {:method :put :url "model/subnet"
               :body {:name name
                      :network_address subnet
                      :direction direction
                      :zones {:set [zone-id]}
                      :notes (str "Imported from NMDB ID: " id
                                  (when (not= "" notes) (str " NMDB Notes: " notes)))}}))

(defn update-subnet [{:keys [id name direction notes]} zone-id existing]
  (xn/execute {:method :patch :url (str "model/subnet/" (:id existing))
               :body {
                      ; TODO: uncomment these lines if you want to replace the existing name or direction with NMDB data
                      ; :name name
                      ; :direction direction
                      :zones {:set [zone-id]}
                      :notes (str (:notes existing) "; Updated from NMDB ID: " id
                                  (when (not= "" notes) (str " NMDB Notes: " notes)))}}))

(defn create-vlan [{:keys [direction notes vlan_type vlan primary_vlan id]} zone-id]
  (xn/execute {:method :put :url "model/vlan"
               :body {
                      ; TODO: is vlan the name? It's always just the vlan number.
                      :name vlan
                      :description notes
                      :zone zone-id
                      :direction direction
                      :vlan_type vlan_type
                      :notes (str "Imported from NMDB ID: " id
                                  (when (not= vlan primary_vlan) (str " (Primary VLAN: " primary_vlan)))}}))

(defn create-zone [{:keys [name id subnets vlans]} pod-id existing-subnets]
  (let [subnets (group-by (comp boolean existing-subnets :subnet) subnets)
        sn-to-create (subnets false)
        sn-to-update (subnets true)
        [zone-id valn record]
        (xn/execute {:method :put :url "model/zone"
                     :body {:name name
                            :parent_zone pod-id
                            :notes (str "Imported from NMDB ID: " id
                                        " (VLANs: " (count vlans)
                                        " Subnets: " (count subnets) ")")}})]
    (doseq [subnet sn-to-update]
      (update-subnet subnet zone-id (existing-subnets (:subnet subnet))))
    (doseq [subnet sn-to-create]
      (create-subnet subnet zone-id))
    (doseq [vlan vlans]
      (create-vlan vlan zone-id))))

(defn create-pod [name dc-id zones subnets]
  (let [[pod-id valn record]
        (xn/execute {:method :put :url "model/zone"
                     :body {:name name
                            :parent_zone dc-id
                            :notes (str "Imported from NMDB (Zones: " (count zones)
                                        " VLANs: " (count (mapcat :vlans zones))
                                        " Subnets: " (count (mapcat :subnets zones)) ")")}})]
    (doseq [zone zones]
      (create-zone zone pod-id subnets))))

(defn create-dc [dc pods zones]
  (let [[id valn record]
        (xn/execute {:method :put :url "model/datacenter"
                     :body {:name (:name dc)
                            :notes (str "Imported from NMDB ID: " (:id dc)
                                        " (Pods: " (count pods)
                                        " Zones: " (count zones)
                                        " VLANs: " (count (mapcat :vlans zones))
                                        " Subnets: " (count (mapcat :subnets zones)) ")")}})]
    id))

(defn load-dc [dc]
  (let [xnid (:datacenter_xnid dc)
        xnid (when (not= "" xnid) xnid)
        subnets (if xnid
                  {}
                  (->> (xn/get-vec (xn/join-url xnid "/rel/subnets/properties/network_address,notes"))
                    (map (fn [[id subnet notes]] [subnet {:id id :notes notes}]))
                    (into {})))
        zones (apply concat (:pods dc))
        pods (group-by :pod zones)
        dc-id (if xnid
                (last (s/split #"/" xnid))
                (create-dc dc pods zones))]
    (doseq [[name zones] pods]
      (create-pod name dc-id zones subnets))))


(defn load-all [dcs]
  (doseq [dc dcs]
    (load-dc dc)))

; Config before calling, if needed:
; (xn.client/setup :url "..." :token "...")
(defn load! [filename]
  (-> filename
    dcs
    load-all))

