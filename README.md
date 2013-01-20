# xn.client

A simple and pragmatic library of useful functions for querying and
manipulating data via the [http://xnlogic.com](XN Logic) API

## Usage

Ensure that you have leiningen installed, preferable v2 and run `lein repl`

```clojure
(require '[xn.client :as xn])

(setup :url "https://production.url/v1" :token "yourapp token......")

(->> "/is/client" xn/get-vec (map :name) (take 10))
(->> "/is/client" (xn/get-map :id))

(def results (atom {}))
(xn/execute results 1
            {:method :put
             :url "/model/client"
             :body {:name "John Doe"
                    :sales-rep 1
                    :products {:set [32 84]}}}) 

```

## LightMesh

Code designed for the [http://lightmesh.com](LightMesh) domain is under
the `lm` namespace. 

```clojure
(require '[lm.import.ips :as ip])
(ip/import! "path/to/files")

```

## License

Copyright © 2013 XN Logic
