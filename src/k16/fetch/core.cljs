(ns k16.fetch.core
  (:require ["node-fetch" :as nfetch]
            [cljs-bean.core :as b]
            [promesa.core :as p]
            [cljs.reader :as reader]
            [sieppari.core :as sieppari]))

(def ^:dynamic *DEBUG* false)

(defn browser? []
  (and (exists? js/window)
       (not (exists? js/nw))))

(def fetch-impl (if (browser?) js/fetch nfetch))

(defn- as-transform [f response]
  (-> response
      meta
      :response
      f
      (p/then #(assoc response :body %))))

(defn as-json [response]
  (as-transform
   #(-> %
        .json
        (p/then b/->clj)
        (p/then (fn [b]
                  (into {} b))))
   response))

(defn as-blob [response]
  (as-transform
   #(.blob %)
   response))

(defn as-text [response]
  (as-transform
   #(.text %)
   response))

(defn as-edn [response]
  (as-transform
   #(-> %
        .text
        (p/then reader/read-string))
   response))

(defn when-ok [response]
  (if (:ok response)
    response
    (throw (ex-info "Response status not in [200-299]"
                    {:response response
                     :status (:status response)}))))

(defn- transform-response-node [n]
  (cond
    (instance? (.-Headers fetch-impl) n)
    (->> n
         .entries
         es6-iterator-seq
         (map b/->clj)
         (into {}))

    :else nil))

(defn response->clj [response]
  (let [clj-res (b/bean response
                        :recursive true
                        :transform transform-response-node)]
    (with-meta
      clj-res
      {:response response})))

(def interceptor_options->fetch
  {:name "interceptor_options->fetch"
   :enter (fn [ctx]
            (update ctx :request
                    (fn [options]
                      [(:url options)
                       (b/->js (-> options
                                   (dissoc :url :interceptors)
                                   (dissoc :url :post-interceptors)))])))
   :leave (fn [ctx]
            (update ctx :response response->clj))})

(defn handler [[url fetch-options]]
  (when *DEBUG*
    (js/console.log (str ">> " url) fetch-options))
  (let [promise (fetch-impl url fetch-options)]
    (if *DEBUG*
      (-> promise
          (p/then (fn [response]
                    (let [clone-response (.clone response)
                          clone-headers  (->> (.-headers clone-response)
                                             .entries
                                             es6-iterator-seq
                                             (mapv b/->clj)
                                             (into {}))
                          debug-response {:ok          (.-ok clone-response)
                                          :redirected  (.-redirected clone-response)
                                          :status      (.-status clone-response)
                                          :status-text (.-statusText clone-response)
                                          :type        (.-type clone-response)
                                          :url         (.-url clone-response)
                                          :headers     clone-headers}]
                      (-> (.text clone-response)
                          (p/then (fn [body]
                                    (js/console.log "<< "
                                                    (clj->js (assoc debug-response :body body)))))))
                    response)))
      promise)))

(def internal-pre-interceptors
  [{:name "internal-pre-interceptors"
    :leave (fn [ctx]
             (let [meta-map {:ctx ctx}]
               (update ctx :response #(with-meta % (merge (meta %) meta-map)))))}])

(def internal-post-interceptors
  [interceptor_options->fetch handler])

(defn execute [options]
  (let [{:keys [interceptors pre-interceptors post-interceptors]
         :or {interceptors []
              pre-interceptors internal-pre-interceptors
              post-interceptors internal-post-interceptors}}
        options
        p (p/deferred)]

    (sieppari/execute
     (concat pre-interceptors interceptors post-interceptors)
     options
     (partial p/resolve! p)
     (partial p/reject! p))

    p))

(defn fetch
  ([url] (fetch url {}))
  ([url options]
   (execute (assoc options :url url))))

(defn content-type-json-interceptor []
  {:name "content-type-json-interceptor"
   :enter (fn [ctx]
            (-> ctx
                (assoc-in [:request :headers "content-type"] "application/json")
                (update-in [:request :body] #(-> % b/->js js/JSON.stringify))))})

(defn create-accept-interceptor [{:keys [accept response-transformer name]}]
  (fn accept-interceptor
    ([] (accept-interceptor {}))
    ([{:keys [only-ok?] :or {only-ok? false}}]
     {:name (or name (str "accept-" accept "-interceptor"))
      :enter (fn [ctx]
               (-> ctx
                   (assoc-in [:request :headers "accept"] accept)))

      :leave (fn [{:keys [response] :as ctx}]
               (if (and (not (:ok response)) only-ok?)
                 ctx
                 (-> (response-transformer response)
                     (p/then #(assoc ctx :response %)))))})))

(def accept-json-interceptor
  (create-accept-interceptor
   {:accept "application/json"
    :response-transformer as-json}))

(def accept-edn-interceptor
  (create-accept-interceptor
   {:accept "application/edn"
    :response-transformer as-edn}))

(def accept-text-interceptor
  (create-accept-interceptor
   {:accept "text/plain"
    :response-transformer as-text}))
(comment

  (-> (fetch "https://api.staging.transit.dev/v2/campaign/pull-campaigns"
             {:method :post
              :body {:pull "[*]"}
              :interceptors [(content-type-json-interceptor)
                             (accept-json-interceptor)]})
      (p/then #(def res %))
      (p/catch #(def res %)))

  (meta res)

  nil)
