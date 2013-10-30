(ns ^:shared ped-geschichte.behavior
    (:require [clojure.string :as string]
              [geschichte.repo :as repo]
              [io.pedestal.app.util.platform :as p]
              [io.pedestal.app.messages :as msg]
              [io.pedestal.app :as app]))

;; While creating new behavior, write tests to confirm that it is
;; correct. For examples of various kinds of tests, see
;; test/ped_geschichte/behavior-test.clj.

;; TODO
;; - implement a simple toolbar
;; - show repo meta-data

(defn set-value-transform [old-value message]
  (:value message))

;; Emitters

(defn init-main [_]
  [{:geschichte
    {:staged {}
     :repo {}
     :commit {}
     :form
     {:select
      {:transforms
       {:set-meta [{msg/topic [:meta] (msg/param :value) {}}]
        :set-value [{msg/topic [:staged] (msg/param :value) {}}]
        :set-repo [{msg/topic [:repo] (msg/param :value) {}}]
        :fork [{msg/topic [:fork] (msg/param :value) {}}]
        :merge [{msg/topic [:merge] (msg/param :value) {}}]
        :commit [{msg/topic [:commit]}]}}}}}])


(defn commit [old {:keys [repo value commit meta]}]
  (if-not (or (= (:value old) value)
              (= (:ts old) commit)
              (nil? repo))
    (assoc (repo/commit repo
                        meta
                        (if (:head meta) #{(:head meta)} #{})
                        value)
      :ts commit)
    old))


(defn transact-to-kv [trans]
  [{msg/topic [:transact-to-kv] :transact trans}])


(defn commit-to-kv [{:keys [meta value repo] :as trans}]
  (when (and meta value repo)
    (transact-to-kv (assoc trans
                      :actions [:puts]
                      :puts [[(:head meta) value]
                             [repo meta (fn [] {msg/type :set-meta msg/topic [:meta] :value meta})]]))))


(defn load-meta-from-repo [new-repo]
  (when new-repo
    (transact-to-kv {:actions [:gets]
                     :gets [[new-repo
                             (fn [v] {msg/type :set-meta msg/topic [:meta] :value v})]]})))


(defn load-value-from-meta [{:keys [head] :as new-meta}]
  (when new-meta
    (transact-to-kv {:actions [:gets]
                     :gets [[head
                             (fn [v] {msg/type :set-value msg/topic [:staged] :value v})]]})))


(defn fork-to-kv [{:keys [new-repo old-repo meta]}]
  (when (and old-repo new-repo)
    (transact-to-kv {:actions [:puts]
                     :puts [[new-repo meta (fn [] {msg/type :set-repo msg/topic [:repo] :value new-repo})]]})))


(defn load-merge-meta [merge-repo]
  (when merge-repo
    (transact-to-kv {:actions [:gets]
                     :gets [[merge-repo
                             (fn [v] {msg/type :set-merge-meta msg/topic [:merge-meta] :value v})]]})))


(defn load-merge-head [merge-meta]
  (when merge-meta
    (transact-to-kv {:actions [:gets]
                     :gets [[(:head merge-meta)
                             (fn [v] {msg/type :set-merge-value msg/topic [:merge-value] :value v})] ()]})))


(defn resolve [{:keys [repo meta merge-meta staged to-merge] :as res}]
  (when res
    (commit-to-kv (repo/merge repo meta merge-meta (str staged to-merge)))))


(def example-app
  ;; There are currently 2 versions (formats) for dataflow
  ;; description: the original version (version 1) and the current
  ;; version (version 2). If the version is not specified, the
  ;; description will be assumed to be version 1 and an attempt
  ;; will be made to convert it to version 2.
  {:version 2
   :transform [[:set-repo [:repo] set-value-transform]
               [:set-value [:staged] set-value-transform]
               [:commit [:commit] p/date] ; UUID?
               [:set-meta [:meta] set-value-transform]
               [:fork [:fork] set-value-transform]
               [:merge [:merge] set-value-transform]
               [:set-merge-meta [:merge-meta] set-value-transform]
               [:set-merge-value [:merge-value] set-value-transform]]
   :derive #{;; determine committing
             [{[:repo] :repo
               [:commit] :commit
               [:staged] :value
               [:meta] :meta} [:trans-comm] commit :map]
             ;; determine forking
             [{[:meta] :meta
               [:fork] :new-repo
               [:repo] :old-repo} [:trans-fork] #(if (or (not (:new-repo %1))
                                                         (= (:new-repo %1)
                                                            (:old-repo %2))) %2 %1) :map]
             ;; determine merging
             [{[:repo] :repo
               [:meta] :meta
               [:merge-value] :to-merge
               [:merge-meta] :merge-meta
               [:staged] :staged} [:trans-merge] #(if (not= (:to-merge %1) ; TODO check
                                                            (:to-merge %2)) %2 %1) :map]}


   :effect #{[#{[:trans-comm]} commit-to-kv :single-val]
             [#{[:trans-fork]} fork-to-kv :single-val]
             [#{[:trans-merge]} resolve :single-val]
             [#{[:repo]} load-meta-from-repo :single-val]
             [#{[:meta]} load-value-from-meta :single-val]
             [#{[:merge]} load-merge-meta :single-val]
             [#{[:merge-meta]} load-merge-head :single-val]}
   :emit [{:init init-main}
          [#{[:staged]
             [:repo]
             [:meta]
             [:commit]
             [:fork]
             [:merge]
             [:merge-meta]
             [:merge-value]
             [:trans-comm]
             [:trans-fork]
             [:trans-merge]} (app/default-emitter [:geschichte])]]})


;; Once this behavior works, run the Data UI and record
;; rendering data which can be used while working on a custom
;; renderer. Rendering involves making a template:
;;
;; app/templates/ped-geschichte.html
;;
;; slicing the template into pieces you can use:
;;
;; app/src/ped_geschichte/html_templates.cljs
;;
;; and then writing the rendering code:
;;
;; app/src/ped_geschichte/rendering.cljs
