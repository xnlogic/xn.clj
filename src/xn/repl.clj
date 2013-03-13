(ns xn.repl
  (:use clojure.pprint)
  (:require [clojure.string :as s]))

(defn prident
  ([x] (pprint x) x)
  ([s x] (print (str s ": ")) (pprint x) x)
  ([s a x] (print (str s ": ")) (pprint a) (pprint x) x))

(defn info [name [id valn record :as result]]
  (if id
    (println (str "Executed " name ": id " id " (" (s/join ", " valn) ")"))
    (do
      (println (str name " did not get an id:"))
      (pprint result)))
  result)

