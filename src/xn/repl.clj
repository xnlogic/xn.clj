(ns xn.repl
  (:use clojure.pprint))

(defn prident
  ([x] (pprint x) x)
  ([s x] (print (str s ": ")) (pprint x) x))

