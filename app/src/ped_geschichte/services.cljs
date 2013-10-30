(ns ped-geschichte.services
  (:require [io.pedestal.app.protocols :as p]
            [io.pedestal.app.messages :as msg]
            [cljs.reader :refer [read-string]]

            [geschichte.repo :as repo]
            [geschichte.meta :as meta]
            [geschichte.data :as data]
            [geschichte.store :as store]))

;; The services namespace responsible for communicating with back-end
;; services. It receives messages from the application's behavior,
;; makes requests to services and sends responses back to the
;; behavior.
;;
;; This namespace will usually contain a function which can be
;; configured to receive effect events from the behavior in the file
;;
;; app/src/ped_geschichte/start.cljs
;;
;; After creating a new application, set the effect handler function
;; to receive effects
;;
;; (app/consume-effect app services-fn)
;;
;; A very simple example of a services function which echoes all events
;; back to the behavior is shown below.


(defrecord MemoryStore [state]
  store/IKeyValueStore
  (-get [this key cb] (cb {:result (get @state key)}))
  (-put [this key val cb] (cb (swap! state assoc key val))))


(def mem-store (MemoryStore. (atom {:users #{"u1"}
                                    "u1" {:repositories #{"r"}}
                                    "u1/r" {:head 1756,
                                            1756 #{1693},
                                            1693 #{1662},
                                            1662 #{}}
                                    1662 "42"
                                    1693 "52"
                                    1756 "73"})))


(defn services-fn [{{:keys [actions] :as transact} :transact} queue]
  (.log js/console "mem-store: " (str @(:state mem-store)) "transact: " (str transact))
  ((fn actions-fn [[a & as]]
     (cond (= a :puts)
           (let [{:keys [puts]} transact]
             ((fn put-fn [[[k v mf] & ps]]
                (when k
                  (store/-put mem-store k v
                              (fn [e]
                                (.log js/console "put: " k v)
                                (when mf (p/put-message queue (mf)))
                                (put-fn ps))))) puts)
             (actions-fn as))

           (= a :gets)
           (let [{:keys [gets]} transact]
             ((fn get-fn [[[k mf] & gs]]
                (when k
                  (store/-get mem-store k
                              (fn [{:keys [result]}]
                                (.log js/console "get: " k result)
                                (p/put-message queue (mf result))
                                (get-fn gs))))) gets)
             (actions-fn as)))) actions))


;; During development, it is helpful to implement services which
;; simulate communication with the real services. This implementation
;; can be placed in the file:
;;
;; app/src/ped_geschichte/simulated/services.cljs
;;
