(ns ped-geschichte.indexeddb
  (:require [cljs.reader :refer [read-string]]
            [geschichte.store :as store]))


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

  (-del [this key cb])
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
      (.add obj-store (clj->js {:key key :value (pr-str value)}))))
  (-transact [this trans cb]))


(def dummy-data
  [{:id "Nachhaltiges Mannheim 2014"
    :start (js/Date. 2013 0 1)
    :end (js/Date. 2014 0 1)
    :children ["Monatsplan"]}
   {:id "Monatsplan"
    :start (js/Date. 2013 9 1)
    :end (js/Date. 2013 10 1)
    :parent "Nachhaltiges Mannheim 2014"
    :children ["Kellnerschicht" "Kloputzen"]}
   {:id "Kellnerschicht"
    :start (js/Date. 2013 9 10)
    :end (js/Date. 2013 9 11)
    :parent "Monatsplan"}
   {:id "Kloputzen"
    :start (js/Date. 2013 9 20)
    :end (js/Date. 2013 9 21)
    :parent "Monatsplan"}])

(defn- put-data [db data]
  (doseq [d data] (-put db (:id d) d #(.log js/console "put: " %))))

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
                                 (put-data (.-result req) dummy-data)
                                 (cb {:db-opened (.-result req)})))
       (set! (.-onupgradeneeded req) (partial up-fn version)))))
