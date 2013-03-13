(ns xn.client
  (:require [clojure.data.json :as json]
            [clojure.string :as s]
            [clj-http.client :as client]))

(def *url-root (atom "http://localhost:8080/v1"))
(def *token    (atom (get (System/getenv) "LMTOKEN")))

(defn setup [& {:keys [url token]}]
  (when url
    (reset! *url-root url))
  (when token (reset! *token token)))

(defn join-url
  ([url] url)
  ([a b] (str (if (= \/ (last a)) (subs a 0 (dec (count a))) a)
              (if (#{\. \/} (first b)) b (str \/ b))))
  ([a b c & more] (apply join-url (join-url a b) c more)))

(defn finalize-command [command]
  (merge {:headers {"Authorization" @*token}
          :method :get}
         command
         (when-let [body (:body command)]
           {:body (json/write-str body)})
         {:url (when-let [url (:url command)]
                 (when (re-find #"^http" url) (throw ()))
                 (str
                   (join-url @*url-root (:url command) ".edn")
                   (when (:query command)
                     (str "?" (s/join "&" (map #(s/join "=" (map name %)) (:query command)))))))}))

(defn make-request [command]
  (binding [*read-eval* false]
    (-> command
      finalize-command
      client/request
      :body
      read-string)))

(defn get-vec [url]
  (make-request {:method :get :url url}))

(defn get-one [url]
  (first (get-vec url)))

(defn get-set [url]
  (set (get-vec url)))

(defn get-map
  ([url] (get-map :id url))
  ([key url]
   (->> url
     get-vec
     (map (juxt key identity))
     (into {}))))

(defn execute
  ([command]
   (make-request command))
  ([created key command]
   (try
     (swap! created assoc key (execute command))
     (catch java.lang.Throwable e
       (swap! created assoc key e)))))

(defn add-rel [name id]
  (when (and name id)
    {name {:add id}}))

(defn set-rel [name id]
  (when (and name id)
    {name {:set id}}))

; More elaborite logic that understands :set :add and :remove is possible but
; overkill for now.
(def merge-attrs (partial merge-with merge))

(defn md
  ([name]
   (->> (join-url name "metadata")
     get-vec
     (map (fn [[k v]]
            (if (map? v)
              [k (sort (keys v))]
              [k v])))
     (into {})))
  ([name key]
   (key (md name))))

(defn ->path-properties
  ([url-parts]
   (let [[path properties]
         (reduce (fn [[path properties] [segment props]]
                   (if segment
                     [(join-url path segment)
                      (conj properties
                            (cond (map? props) [(s/join "," (map name (keys props)))
                                                (mapv (comp keyword name) (vals props))]
                                  (sequential? props) [(s/join "," (map name props))
                                                       (mapv (comp keyword name) props)]
                                  props [(name props) [(keyword (name props))]]
                                  :else ["" []]))]
                     [path properties]))
                 [nil []]
                 (partition 2 url-parts))]
     {:method :get
      :url (s/join "/" (apply vector path "path_properties" (map first properties)))
      :keys (apply concat (map second properties))}))
  ([url-parts opts]
   (merge (->path-properties url-parts) opts)))

(defn get-path-maps [url-parts opts]
  (let [command (->path-properties url-parts opts)]
    (let [keys (:keys command)]
      (map #(zipmap keys (apply concat %)) (make-request command)))))

(defn get-path-properties
  ([url-parts] (get-path-properties url-parts nil))
  ([url-parts opts]
   (map #(vec (apply concat %)) (make-request (->path-properties url-parts opts)))))

(defn account []
  (get-one "/account"))

(defn show-account []
  (let [a (account)]
    (println (str "loading data for client: " (a "client_name")))
    (println (str "          using account: " (:email a)))))

; show the current client on boot
(try (show-account)
  (catch Exception e (.getMessage e)))

