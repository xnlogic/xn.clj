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
