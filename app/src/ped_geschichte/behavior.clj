(ns ^:shared ped-geschichte.behavior
    (:require [clojure.string :as string]
              [geschichte.repo :as repo]
              [geschichte.meta :as meta]
              [io.pedestal.app.util.platform :as p]
              [io.pedestal.app.messages :as msg]
              [io.pedestal.app :as app]))

;; TODO
;; - implement a simple toolbar
;; - show repo meta-data as a dependency graph

(defn set-value-transform [old-value message]
  (:value message))

;; Emitters

(def schema {:type ::schema
             :version 1})

(defn init-main [_]
  [{:geschichte
    {:staged {}
     :repo {}
     :commit {}
     :meta {}
     :form
     {:select
      {:transforms
       ;; HACK: < for string comparison of keywords only works in JS
       (sorted-map-by < :pull [{msg/topic [:pull]
                                (msg/param :user) {}
                                (msg/param :branch) {}}]
                      :create-pull-request [{msg/topic [:create-pull-request]
                                             (msg/param :user) {}}]
                      :merge-pull-request [{msg/topic [:merge-pull-request]
                                            (msg/param :user) {}
                                            (msg/param :tip) {}}]
                      :checkout [{msg/topic [:repo] (msg/param :value) {}}]
                      :create [{msg/topic [:create]
                                (msg/param :value) {}
                                (msg/param :description) {}}]
                      :clone [{msg/topic [:clone]
                               (msg/param :repo) {}
                               (msg/param :branch) {}
                               (msg/param :user) {}}]
                      :set-value [{msg/topic [:staged] (msg/param :value) {}}]
                      :set-user [{msg/topic [:user] (msg/param :value) {}}]
                      :branch [{msg/topic [:branch] (msg/param :value) {}}]
                      :merge [{msg/topic [:merge] (msg/param :user) {} (msg/param :branch) {}}]
                      :commit [{msg/topic [:commit]}])}}}}])


(defn commit [old {:keys [value commit meta branch user]}]
  (if-not (or (= (:ts old) commit))
    (assoc (repo/commit meta
                        user
                        schema
                        branch
                        (first ((:branches meta) branch))
                        value)
      :ts commit
      :user user)
    old))


(defn transact-to-kv [trans]
  [{msg/topic [:transact-to-kv] :transact trans}])


(defn commit-to-kv [{:keys [user meta value] :as trans}]
  (when (and meta value user)
    (transact-to-kv (assoc trans
                      :actions [:puts]
                      :puts [[(first ((:branches meta) (:head meta))) value]
                             [(str user "/" (.-uuid (:id meta)))
                              meta
                              (fn [] [{msg/type :set-meta
                                      msg/topic [:meta]
                                      :value meta}])]]))))


(defn load-meta-from-repo [{:keys [repo user]}]
  (when (and repo user)
    (transact-to-kv {:actions [:gets]
                     :gets [[(str user "/" (.-uuid repo))
                             (fn [v] [{msg/type :set-meta
                                      msg/topic [:meta]
                                      :value v}])]]})))


(defn load-value-from-meta [new-meta]
  (when new-meta
    (transact-to-kv {:actions [:gets]
                     :gets [[(first ((:branches new-meta) (:head new-meta)))
                             (fn [v] [{msg/type :set-value
                                      msg/topic [:staged]
                                      :value v}])]]})))


(defn branch-to-kv [{:keys [meta branch user]}]
  (when (and meta branch user)
    (if-let [b ((:branches meta) branch)]
      (let [new-meta (assoc meta
                       :head branch
                       :last-update (repo/*date-fn*))]
        (transact-to-kv {:actions [:puts]
                         :puts [[(str user "/" (.-uuid (:id meta)))
                                 new-meta
                                 (fn [] [{msg/type :set-meta
                                         msg/topic [:meta]
                                         :value new-meta}])]]}))
      (let [new-meta (:meta (repo/branch meta branch (first ((:branches meta) (:head meta)))))]
        (transact-to-kv {:actions [:puts]
                         :puts [[(str user "/" (.-uuid (:id meta)))
                                 new-meta
                                 (fn [] [{msg/type :set-meta
                                         msg/topic [:meta]
                                         :value new-meta}])]]})))))

(defn load-merge-meta [{{:keys [user]} :merge repo :repo}]
  (when (and user repo)
    (transact-to-kv {:actions [:gets]
                     :gets [[(str user "/" (.-uuid repo))
                             (fn [v] [{msg/type :set-merge-meta
                                      msg/topic [:merge-meta]
                                      :value v}])]]})))

(defn load-merge-head [{:keys [merge meta]}]
  (when (and merge meta)
    (if-let [merge-head (first ((:branches meta) (:branch merge)))]
      (transact-to-kv {:actions [:gets]
                       :gets [[merge-head
                               (fn [v] [{msg/type :set-merge-head
                                        msg/topic [:merge-head]
                                        :value v}])]]}))))


(defn load-clone-from-kv [{:keys [user repo]}]
  (when (and user repo)
    (transact-to-kv {:actions [:gets]
                     :gets [[(str user "/" (.-uuid repo))
                             (fn [v] [{msg/type :set-clone-meta
                                      msg/topic [:clone-meta]
                                      :value v}])]]})))


(defn load-pull-from-kv [{{:keys [user]} :pull repo :repo}]
  (when (and user repo)
    (transact-to-kv {:actions [:gets]
                     :gets [[(str user "/" (.-uuid repo))
                             (fn [v] [{msg/type :set-pull-meta
                                      msg/topic [:pull-meta]
                                      :value v}])]]})))


(defn load-create-pull-from-kv [{:keys [pull repo]}]
  (when (and (:user pull) repo)
    (transact-to-kv {:actions [:gets]
                     :gets [[(str (:user pull) "/" (.-uuid repo))
                             (fn [v] [{msg/type :set-pull-request-meta
                                      msg/topic [:pull-request-meta]
                                      :value v}])]]})))


(defn load-merge-pull-from-kv [{:keys [meta merge-pull-request]}]
  (when (and meta merge-pull-request)
    (let [base (-> (get-in meta [:pull-requests (:user merge-pull-request) (:tip merge-pull-request)]) :cut first)]
      (transact-to-kv {:actions [:gets]
                       :gets [[(:tip merge-pull-request)
                               (fn [v] [{msg/type :merge-pull-request-other
                                        msg/topic [:merge-pull-request-other]
                                        :value v}])]
                              [base
                               (fn [v] [{msg/type :merge-pull-request-base
                                        msg/topic [:merge-pull-request-base]
                                        :value v}])]]}))))


(defn dumb-merge [{:keys [meta merge-meta staged merge-head merge user] :as res}]
  (when (and meta merge-meta staged merge-head merge user)
    (let [cur-branch (:head meta)
          merged (repo/merge-heads user
                                   schema
                                   cur-branch
                                   meta
                                   ((:branches meta) cur-branch)
                                   merge-meta
                                   ((:branches merge-meta) (:branch merge))
                                   {:value (str (:value staged)
                                                (:value merge-head))})]
      (commit-to-kv (assoc merged :user user)))))


(defn create-repository [old {:keys [create user]}]
  (let [{:keys [value description]} create]
    (if-not (= description (:description old))
      (assoc (repo/new-repository description
                                   user
                                   schema
                                   false
                                   {:value value})
        :user user))))


(defn create-to-kv [{:keys [user meta value]}]
  (when (and user meta value)
    (transact-to-kv {:actions [:puts]
                     :puts [[(str user "/" (.-uuid (:id meta))) meta
                             (fn [] [{msg/type :set-meta msg/topic [:meta] :value meta}])]
                            [(:id (:geschichte.meta/meta value)) value
                             (fn [] [{msg/type :set-value msg/topic [:staged] :value value}
                                    {msg/type :checkout msg/topic [:repo] :value (:id meta)}
                                    {msg/type :branch msg/topic [:branch] :value "master"}])]]})))


(defn clone-to-kv [{:keys [clone clone-meta user]}]
  (when (and clone clone-meta user)
    (let [cloned (repo/clone clone-meta (:branch clone) true)]
      (transact-to-kv {:actions [:puts]
                       :puts [[(str user "/" (.-uuid (:id clone-meta))) cloned
                               (fn [] [{msg/type :set-meta
                                       msg/topic [:meta] :value cloned}
                                      {msg/type :branch
                                       msg/topic [:branch] :value (:branch clone)}])]]}))))


(defn pull-to-kv [{:keys [pull pull-meta meta branch user]}]
  (when (and pull pull-meta meta branch)
    (let [pulled (repo/pull meta branch pull-meta (first ((:branches pull-meta) (:branch pull))))]
      (transact-to-kv {:actions [:puts]
                       :puts [[(str user "/" (.-uuid (:id pull-meta))) pulled
                               (fn [] [{msg/type :set-meta
                                       msg/topic [:meta] :value (:meta pulled)}])]]}))))


(defn merge-pull-to-kv [{:keys [meta value base other user branch request]}]
  (when (and meta value base other user branch request)
    (let [merged (repo/merge-lcas user
                                  schema
                                  branch
                                  meta
                                  ((:branches meta) branch)
                                  #{(:tip request)}
                                  {:value (str (:value other)
                                               (:value value))}
                                  request)]
      (transact-to-kv {:actions [:puts]
                       :puts [[(str user "/" (.-uuid (:id meta))) (:meta merged)
                               (fn [] [])]
                              [(:id (:geschichte.meta/meta (:value merged))) (:value merged)
                               (fn [] [{msg/type :set-meta
                                       msg/topic [:meta] :value (:meta merged)}
                                      {msg/type :staged
                                       msg/topic [:staged] :value (:value merged)}])]]}))))


(defn create-pull-to-kv [{:keys [pull pull-meta meta branch user repo]}]
  (when (and pull pull-meta meta branch user repo)
    (let [lcas (geschichte.meta/lowest-common-ancestors
                (:causal-order pull-meta)
                ((:branches pull-meta) (:head pull-meta))
                (:causal-order meta)
                ((:branches meta) branch))
          new-meta (assoc-in pull-meta [:pull-requests user (first ((:branches meta) (:head meta)))] lcas)]
      (transact-to-kv {:actions [:puts]
                       :puts [[(str (:user pull) "/" (.-uuid repo)) new-meta
                               (fn [] [#_{msg/type :set-meta
                                       msg/topic [:meta]
                                       :value new-meta}])]]}))))


(def ped-geschichte-app
  {:version 2
   :transform [[:create [:create] (fn [o v] (select-keys v #{:value :description}))]

               [:pull [:pull] (fn [o v] (assoc (select-keys v #{:user :branch})
                                         :repo (UUID. (:repo v))))]
               [:create-pull-request [:create-pull-request] (fn [o v] (select-keys v #{:user}))]
               [:set-pull-request-meta [:pull-request-meta] set-value-transform]
               [:merge-pull-request [:merge-pull-request]
                (fn [o v] (assoc (select-keys v #{:user}) :tip (UUID. (:tip v))))]
               [:merge-pull-request-base [:merge-pull-request-base] set-value-transform]
               [:merge-pull-request-other [:merge-pull-request-other] set-value-transform]

               [:checkout [:repo] (fn [o {v :value}] (if (string? v) (UUID. v) v))]

               [:commit [:commit] p/date] ; UUID?
               [:clone [:clone] (fn [o {:keys [user repo branch]}]
                                  {:user user :repo (UUID. repo) :branch branch
                                   :ts (p/date)})]

               [:branch [:branch] set-value-transform]
               [:merge [:merge] (fn [o v] (select-keys v #{:user :branch}))]

               [:set-user [:user] set-value-transform]
               [:set-value [:staged] (fn [o {v :value}] (if (string? v) {:value v} v))]

               [:set-meta [:meta] set-value-transform]
               [:set-merge-meta [:merge-meta] set-value-transform]
               [:set-merge-head [:merge-head] set-value-transform]
               [:set-clone-meta [:clone-meta] set-value-transform]
               [:set-pull-meta [:pull-meta] set-value-transform]]
   :derive #{
             ;; navigation
             [{[:repo] :repo
               [:user] :user} [:checkout] #(if-not (and (= (:repo %2)
                                                           (:repo %1))
                                                        (= (:user %2)
                                                           (:user %1))) %2 %1) :map]

             ;; creation and cloning
             [{[:create] :create
               [:user] :user} [:trans-create] #(if-not (and (= (:description (:meta %1))
                                                               (:description (:create %2)))
                                                            (= (:author (:value %1))
                                                               (:user (:create %2))))
               (create-repository %1 %2) %1) :map]

             [{[:clone] :clone
               [:user] :user
               [:clone-meta] :clone-meta} [:trans-clone] #(if-not (= (:clone-meta %1)
                                                                     (:clone-meta %2)) %2 %1) :map]

             ;; pulling
             [{[:pull] :pull
               [:repo] :repo} [:prepare-pull] #(if-not (= (:pull %1)
                                                          (:pull %2)) %2 %1) :map]

             [{[:pull] :pull
               [:meta] :meta
               [:branch] :branch
               [:user] :user
               [:pull-meta] :pull-meta} [:trans-pull] #(if-not (= (:pull-meta %1)
                                                                  (:pull-meta %2)) %2 %1) :map]

             [{[:create-pull-request] :pull
               [:repo] :repo} [:load-create-pull-request] #(if-not (= (:pull %1)
                                                                      (:pull %2)) %2 %1) :map]

             [{[:create-pull-request] :pull
               [:pull-request-meta] :pull-meta
               [:meta] :meta
               [:branch] :branch
               [:repo] :repo
               [:user] :user} [:trans-create-pull-request] #(if-not (= (:pull-meta %1)
                                                                       (:pull-meta %2)) %2 %1) :map]
             [{[:merge-pull-request] :merge-pull-request
               [:meta] :meta} [:load-merge-pull-request] #(if-not (= (:merge-pull-request %1)
                                                                     (:merge-pull-request %2)) %2 %1) :map]

             [{[:meta] :meta
               [:staged] :value
               [:user] :user
               [:branch] :branch
               [:merge-pull-request] :request
               [:merge-pull-request-base] :base
               [:merge-pull-request-other] :other} [:trans-merge-pull-request] #(if-not (= (:base %1)
                                                                                           (:base %2)) %2 %1) :map]

             ;; committing
             [{[:commit] :commit
               [:staged] :value
               [:meta] :meta
               [:branch] :branch
               [:user] :user} [:trans-commit] commit :map]

             ;; branching
             [{[:meta] :meta
               [:user] :user
               [:branch] :branch} [:trans-branch] (fn [o n] (if-not (= (:branch o)
                                                                      (:branch n))
                                                             n o)) :map]

             ;; merging
             [{[:repo] :repo
               [:merge] :merge} [:prepare-merge-meta] #(if (not= (:merge %1)
                                                                 (:merge %2)) %2 %1) :map]

             [{[:merge-meta] :meta
               [:merge] :merge} [:prepare-merge-head] #(if (not= (:meta %1)
                                                                 (:meta %2)) %2 %1) :map]

             [{[:meta] :meta
               [:merge] :merge
               [:merge-meta] :merge-meta
               [:merge-head] :merge-head
               [:user] :user
               [:staged] :staged} [:trans-merge] #(if (not= (:merge-head %1) ; TODO check
                                                            (:merge-head %2)) %2 %1) :map]}


   :effect #{[#{[:clone]} load-clone-from-kv :single-val]
             [#{[:prepare-pull]} load-pull-from-kv :single-val]
             [#{[:load-create-pull-request]} load-create-pull-from-kv :single-val]
             [#{[:load-merge-pull-request]} load-merge-pull-from-kv :single-val]
             [#{[:trans-merge-pull-request]} merge-pull-to-kv :single-val]
             [#{[:trans-create]} create-to-kv :single-val]
             [#{[:trans-commit]} commit-to-kv :single-val]
             [#{[:trans-branch]} branch-to-kv :single-val]
             [#{[:trans-merge]} dumb-merge :single-val]
             [#{[:trans-clone]} clone-to-kv :single-val]
             [#{[:trans-pull]} pull-to-kv :single-val]
             [#{[:trans-create-pull-request]} create-pull-to-kv :single-val]
             [#{[:checkout]} load-meta-from-repo :single-val]
             [#{[:meta]} load-value-from-meta :single-val]
             [#{[:prepare-merge-meta]} load-merge-meta :single-val]
             [#{[:prepare-merge-head]} load-merge-head :single-val]}
   :emit [{:init init-main}
          [#{[:user]

             [:create]
             [:trans-create]

             [:pull]
             [:trans-pull]
             [:create-pull-request]
             [:load-create-pull-request]
             [:trans-create-pull-request]

             [:merge]
             [:merge-meta]
             [:merge-head]
             [:prepare-merge-meta]
             [:prepare-merge-head]

             [:merge-pull-request]
             [:merge-pull-request-base]
             [:merge-pull-request-other]
             [:load-merge-pull-request]
             [:trans-merge-pull-request]

             [:staged]
             [:repo]
             [:checkout]
             [:meta]
             [:commit]
             [:clone]
             [:clone-meta]
             [:trans-clone]
             [:reverse]
             [:reverse-value]
             [:branch]
             [:trans-commit]
             [:trans-reverse]
             [:trans-branch]
             [:trans-merge]} (app/default-emitter [:geschichte])]]})
