(ns xn.tools
  (:require [clojure.string :as s]
            [clojure.walk :refer [postwalk]]))

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
                            (update-in m [k] (rules k (:default rules (fn [a b] b))) v)
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

(defn fix-invalid-chars [mapping]
  (fn [text]
    (s/join (map #(mapping % %) text))))

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
    (when (seq values)
      (vec values))))

(defn vec-wrap [x]
  (cond (sequential? x) x
        (set? x) x
        (map? x) [x]
        (nil? x) []
        :else [x]))

(defmulti recursive-merge (fn [a b]
                            (cond
                              (and a b) [(class a) (class b)]
                              a [(class a) (class a)]
                              b [(class b) (class b)])))
(defmethod recursive-merge [java.util.Map java.util.Map] [a b]
  (merge-with recursive-merge a b))
(defmethod recursive-merge [java.util.Set java.util.Set] [a b]
  (clojure.set/union a b))
(defmethod recursive-merge [java.util.Collection java.util.Collection] [a b]
  (concat a b))
(defmethod recursive-merge :default [a b]
  b)

(defmulti -validator class)
(remove-all-methods -validator)
(defmethod -validator :default [x] nil)
(defmethod -validator clojure.lang.Keyword [x] x)
(defmethod -validator java.util.Map [x]
  (->> x
       (filter (fn [[k v]] v))
       (remove (fn [[k v]] (and (coll? v) (empty? v))))
       (into {})))
(defmethod -validator java.util.Collection [x]
  (cond
    (every? coll? x) (reduce recursive-merge x)
    (and (= 2 (count x)) (every? keyword? x)) nil
    :else x))


(defmulti charset-ok? class)
(defmethod charset-ok? :default [x] (-validator x))
(defmethod charset-ok? (class "") [s]
  ; I'm not sure which characters should be considered invalid so this is just
  ; a guestimation.
  (let [s (->> s (map int) (remove #(< 8 % 255)))]
    (if (empty? s) nil (set s))))

(defn validates [validator]
  (fn [records]
    (->> records
         (map #(postwalk validator %))
         (reduce recursive-merge))))

(comment
  (remove-all-methods charset-ok?) ; use to reset on method removal
  ((validates charset-ok?) [{:a "ok"}])
  (clojure.pprint/pprint
    ((validates charset-ok?) [{:a [{:b "abc" :c "áº≥Ω"
                                    :d (str (char 0) \a \a \½ \b \¿ \c (char 8216) \d \e)}
                                   {:e "ok"}
                                   {:f [{:g "ok"
                                         :i (str \a \a \½ \b \¿ \c (char 8216) \d \e)}]}]
                               :h "ok"}
                              {:a [{:b "abc" :d "áº≥Ω"
                                    :c (str (char 0) \a \a \½ \b \¿ \c (char 8216) \d \e)}
                                   {:e "ok"}
                                   {:f [{:g "ok"
                                         :i (str \a \a \½ \b \¿ \c (char 8216) \d \e)}]}]
                               :h "ok"}])))
