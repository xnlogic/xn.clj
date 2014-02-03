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

## Data Extraction Pipeline

The extract function

1. :reader - (csv, json-file, json-lines, or a custom function that returns a collection of "raw" data records). 
1. :pre - A vector of functions that are composed in order, to operate on the raw file
1. :rows - If the data is in a flat format like CSV. Specify field names for each column.
1. :fields - Rename fields, remove them or copy them into multiple columns or set them up to merge multiple columns into one.
1. :clean - Specify functions that will be used to clean any columns of raw data 
1. :merge-rules - Define the rules for merging data into a given field.
1. :post-merge - Specify functions to clean any columns of data after the merge and copy operations have happened
1. :mappings - Do any transformations necessary here in the context of a full resulting record
1. :filters - Any functions that return false here will remove the record from the import
1. :run - The function that will be given the resulting stream of transformed records: (fn [run-opts records])
1. :run-opts - The first argument to the run function

## LightMesh

Code designed for the [LightMesh](http://lightmesh.com) domain is under
the `lm` namespace. 

```clojure
(require '[lm.import.ips :as ip])
(ip/import! "path/to/files")

```

## License

Copyright © 2013 XN Logic


