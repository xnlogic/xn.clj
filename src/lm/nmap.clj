(ns lm.nmap
  (:use xn.import
        [clj-xpath.core :rename {$x:node* node*
                                 $x:text* text*
                                 $x:text? text?}])
  (:require [xn.client :as xn]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as s]))

; ---- extract info from XML produced by nmap

(defn- hosts [data]
  (node* "/nmaprun/host[status[@state='up']]" data))

(defn- host-macs [host]
  (text* "./address[@addrtype='mac']/@addr" host))

(defn- host-ips [host]
  (text* "./address[@addrtype='ipv6' or @addrtype='ipv4']/@addr" host))

; TODO: test assumptions here:
(defn- host-ifaces
  "This assumes that if there are multiple IPs, there will be corresponding MACs
   and they will be produced in the same order so that they can be zipped together
   like this."
  [host]
  (mapv (fn [mac ip] {:mac mac :ip ip}) (host-macs host) (host-ips host)))

(defn- host-name [host]
  (text? "./hostnames/hostname/@name" host))

(defn- host-os [host]
  (text* "./os/osmatch/@name" host))

(defn- host-software [host]
  (set (text* "./ports/port/service[@conf='10']/@product" host)))

; ---- compose data into simple records

(defn- host-info [host]
  {:name (host-name host)
   :software (concat (host-software host) (host-os host))
   :interfaces (host-ifaces host)})

(defn read-nmap-xml [file]
  (->> (clojure.java.io/input-stream file)
       hosts
       (mapv host-info)))

; ---- format data for the API

(def nmap->lightmesh
  (extract
    :reader read-nmap-xml
    :run create-unique ; will this create if the model is not the same?
    :run-opts {:model :unknown_device
               :key :name :unique_in_parts [:networked]}
    :pre [#(cond-> %
             ; if no hostname, use the first IP
             (not (:name %)) (assoc :name (str "UNKNOWN - "(-> % :interfaces first :ip))))]
    ; possibly could use mac as external id?
    :fields [:name :software :interfaces]
    :clean {:software (extract-rel-unique :add :software :name)
            :interfaces (extract-rel-records
                          :add :interface nil
                          :fields [:mac :ip]
                          :clean {:ip (fn [ip] {:name ip})}
                          :post-merge {:ip (extract-rel-records
                                             :set :ip :name
                                             :fields [:name])})}
    :filters [:name]))

; ---- Testing

(comment
  (pprint (->> (clojure.java.io/input-stream "private/nmap.xml")
               hosts
               (mapv host-info)))

  (pprint (nmap->lightmesh (read-nmap-xml "private/nmap.xml"))))
