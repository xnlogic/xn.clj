; Import a large set of csv files, each containing IPs, DNS info, and
; various information about a single subnet.
;
; - import all IPs not already existing in the subnet
; - create DNS entries and relate them to their new or existing IP
; - add a note for each IP with the file and line it came from.
;
(ns lm.import.ips
  (:require [xn.client :as xn]
            [xn.import :as i]
            [clojure.string :as s]))


(defn subnet [filename]
  (->> filename i/csv flatten (filter i/page-url?) (some (i/model-url "subnet"))))

; if col 1 is Dead or Unknown, skip
(def dead?    (comp (partial re-find #"^Unknown|Dead$") s/trim first))
(def ip?      (comp (partial re-find #"^\d+\.\d+\.\d+\.\d+$") s/trim))
(defn dns? [s]
  (when (and (re-find #"\.(com|net|org)$" s)
             (not (re-find #"@" s)))
    s))


; if no other data in the line, skip
(defn blank? [data]
  (->> data (remove ip?) (remove #{""}) empty?))

; if there is more than one ip in the line, create both
; if we find a dns entry on the line, create it
; the raw csv goes in the ip's notes field
(defn line->records [[line cells]]
  (let [dns (some dns? cells)]
    (->> cells
      (filter ip?)
      (map (fn [ip] {:ip ip :line line :dns dns}))
      set)))

; Work with pairs: [raw-line-string [parsed csv data]]
(def parsed-lines      (partial map (juxt identity (comp first i/parse-csv))))
(def ip-lines          (partial filter (comp (i/has ip?) second)))
(def valid-lines       (partial remove (comp dead? second)))
(def significant-lines (partial remove (comp blank? second)))

; Turn pairs into records
(def records           (partial mapcat line->records))

(def to-import (comp records significant-lines valid-lines ip-lines parsed-lines i/file-lines) )

(defn dns-rel [created ip]
  (when-let [id (get-in @created [(ip :dns) :id])]
    (xn/add-rel :dns_entries id)))

(defn create-dns [created]
  (fn [dns]
    (xn/execute created dns
                {:method :put
                 :url "/model/dns_entry"
                 :body {:name dns}})))

(defn update-ip [created]
  (fn [ip]
    (xn/execute created ip
                {:method :patch
                 :url (get-in ip [:record :meta :xnid])
                 :body (merge {:notes (:line ip)}
                              (dns-rel created ip))})))

(defn create-ip [subnet-id created]
  (fn [ip]
    (xn/execute created ip
                {:method :put
                 :url "/model/ip"
                 :body (merge {:name (:ip ip) :notes (:line ip)}
                              (dns-rel created ip)
                              (xn/set-rel :subnet subnet-id))})))

(defn import! [filename]
  (if-let [subnet-url (subnet filename)]
    (if-let [subnet-id (-> subnet-url xn/get-one :id)]
      (let [ips (xn/get-map :name (str subnet-url "/rel/ips"))
            data (to-import filename)
            data (map #(assoc % :record (ips (:ip %))) data)
            data (map #(assoc % :line (str filename ":\n" (:line %))) data)
            created (atom {})]
        (doall
          (concat
            (->> data (filter :dns) (map :dns) set (map (create-dns created)))
            (->> data (filter :record) (map (update-ip created)))
            (->> data (remove :record) (map (create-ip subnet-id created)))))
        created)
      (str "could not find subnet " subnet-url))
    "no subnet"))

(defn import-all! [dir]
  (->> dir i/csv-files (map import!) vec))
