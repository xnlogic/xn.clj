(ns xn.tools
  (:require [clojure.string :as s]))

(defn non-unique [data]
  (last (reduce (fn [[seen multi] x]
                  (if (seen x)
                    [seen (conj multi x)]
                    [(conj seen x) multi]))
                [#{} #{}]
                data)))

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

(defn vectorize
  "Useful for merge-with or merge-with-rules to build a vector of conflicts"
  [a b]
  (if (vector? a) (conj a b) [a b]))

(defn make-set
  "Useful for merge-with or merge-with-rules to build a set of conflicts"
  [a b]
  (if (set? a) (conj a b) (conj #{a} b)))

(defn has [f] (partial some f))

(defn files
  ([dir] (files dir nil))
  ([dir ext]
   (let [list (->> dir clojure.java.io/file
                   .listFiles
                   (map #(.getPath %)))]
     (if ext
       (filter #(re-find (re-pattern (str "\\." ext "$")) %) list)
       list))))

(defn lower-case [s]
  (when s (s/lower-case (str s))))

(defn key-mapper
  ([fn-map]
   (key-mapper fn-map identity))
  ([fn-map default]
   (fn [r]
     (into {}
           (map (fn [[key value]]
                  (when value
                    [key ((fn-map key (:default fn-map default)) value)]))
                r)))))

(defn get-some [map & keys]
  (let [values (remove nil? ((apply juxt keys) map))]
    (when-not (empty? values)
      (vec values))))
