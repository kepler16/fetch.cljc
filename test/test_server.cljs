(ns test-server
  (:require-macros [test-config :refer [read-config]])
  (:require ["http" :as http]))

(def config (read-config "test/test_config.edn"))

(defonce *server (atom nil))

(defn cors! [^js res]
  (.setHeader res "Access-Control-Allow-Origin" "*")
  (.setHeader res "Access-Control-Allow-Methods" "OPTIONS,GET,POST,DELETE")
  (.setHeader res "Access-Control-Max-Age" 2592000))

(defn cases [^js req ^js res]
  (let [url (.-url req)]
    (when (= "/plain" url)
      (cors! res)
      (.setHeader res "Content-Type" "text/plain")
      (set! (.-statusCode res) 200)
      (.end res "plain text response"))))

(defn start [{:keys [host port handler]
              :or {host (:host config)
                   port (:port config)
                   handler cases}}]
  (prn ::start :host host  :port port)
  (reset! *server (.createServer http handler))
  (.listen ^js @*server port host))

(defn stop []
  (prn ::end)
  (.close @*server))

(comment
  (start {:port 3002})
  (stop)
  )