(ns ped-geschichte.indexeddb
  (:require [cljs.reader :refer [read-string]]
            [geschichte.store :as store]))

;; not yet used here, supposed to be drop-in replacement for dumb memory store

(extend-type js/IDBDatabase
  store/IKeyValueStore
  (-get [this key cb]
    (let [tx (.transaction this (clj->js ["rhodium-obj-store"]))
          obj-store (.objectStore tx "rhodium-obj-store")
          req (.get obj-store key)]
      (set! (.-onerror req) (fn [e]
                              (let [msg {:requested key
                                         :db-error (.-errorCode (.-target e))}]
                                (cb msg))))
      (set! (.-onsuccess req) (fn [e]
                                (cb {:requested key
                                     :result (if-let [res (.-result req)]
                                               (read-string (.-value res)))})))))

  (-put [this key value cb]
    (let [tx (.transaction this (clj->js ["rhodium-obj-store"]) "readwrite")
          obj-store (.objectStore tx "rhodium-obj-store")]
      (set! (.-onerror tx) (fn [e]
                             (let [msg {:requested key
                                        :db-error (.-errorCode (.-target e))}]
                               (cb msg))))
      (set! (.-oncomplete tx) #(cb {:requested key
                                    :status :ok}))
      (.delete obj-store key)
      (.add obj-store (clj->js {:key key :value (pr-str value)})))))


(defn- upgrade-db! [version event]
  (let [db (.-result (.-target event))]
    (cond (= version 1)
          (.createObjectStore db "rhodium-obj-store" (clj->js {:keyPath :key})))))

(defn open-database!
  ([db-name version cb] (open-database! db-name version cb upgrade-db!))
  ([db-name version cb up-fn]
     (let [req (.open (.-indexedDB js/window) (name db-name) version)]
       (set! (.-onerror req) #(cb {:db-error (.-errorCode (.-target %))}))
       (set! (.-onsuccess req) (fn [e]
                                 ; HACK force some dummy data
                                 #_(put-data (.-result req) dummy-data)
                                 (cb {:db-opened (.-result req)})))
       (set! (.-onupgradeneeded req) (partial up-fn version)))))
