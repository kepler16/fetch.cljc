(ns k16.fetch.core-test
  (:require-macros [test-config :refer [read-config]])
  (:require [k16.fetch.core :as f]
            [promesa.core :as p]
            [cljs.core.async.interop :as a.i]
            [cljs.core.async :as a]
            [cljs.test :as t :refer-macros [deftest is testing async use-fixtures]]
            ["fetch-mock" :as fetch-mock]))

(def config (read-config "test/test_config.edn"))

(def mock-impl
  (or (.-default fetch-mock)
      fetch-mock))

(defn test-host [& path]
  (clojure.string/join
   "/"
   (-> "http://"
       (str (:host config) ":" (:port config))
       (cons path))))

(deftest test-mocked-post-request
  (testing "post json"
    (async done
           (a/go
             (let [result
                   (with-redefs
                    [f/fetch-impl
                     (-> (.sandbox mock-impl)
                         (.mock "https://test.test/" #js {:param 2}))]
                     (-> (f/fetch "https://test.test/"
                                  {:method :post
                                   :interceptors
                                   [(f/content-type-json-interceptor)
                                    (f/accept-json-interceptor)]})
                         (p/then f/when-ok)
                         (p/then :body)
                         (a.i/<p!)))]
               (is (= result {:param 2})))
             (done)))))

(deftest get-text-from-server
  (testing "get plain text from endpoint"
    (async done
           (a/go
             (let [result
                   (try (-> (f/fetch
                             (test-host "plain")
                             {:method :get
                              :interceptors [(f/accept-text-interceptor)]})
                            (p/then f/when-ok)
                            (p/then :body)
                            (a.i/<p!))
                        (catch :default e (js/console.error (ex-cause e))))]
               (is (= result "plain text response")))
             (done)))))
