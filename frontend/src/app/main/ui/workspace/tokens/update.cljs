(ns app.main.ui.workspace.tokens.update
  (:require
   [beicon.v2.core :as brx]
   [app.common.types.token :as ctt]
   [app.main.data.workspace.shape-layout :as dwsl]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.workspace.tokens.changes :as wtch]
   [app.main.ui.workspace.tokens.style-dictionary :as wtsd]
   [clojure.data :as data]
   [clojure.set :as set]
   [potok.v2.core :as ptk]
   [beicon.v2.core :as rx]))

;; Constants -------------------------------------------------------------------

(def filter-existing-values? false)

(def attributes->shape-update
  {#{:rx :ry} (fn [v ids _] (wtch/update-shape-radius-all v ids))
   #{:r1 :r2 :r3 :r4} wtch/update-shape-radius-single-corner
   ctt/stroke-width-keys wtch/update-stroke-width
   ctt/sizing-keys wtch/update-shape-dimensions
   ctt/opacity-keys wtch/update-opacity
   #{:x :y} wtch/update-shape-position
   #{:p1 :p2 :p3 :p4} (fn [resolved-value shape-ids attrs]
                        (dwsl/update-layout shape-ids {:layout-padding (zipmap attrs (repeat resolved-value))}))
   #{:column-gap :row-gap} wtch/update-layout-spacing
   #{:width :height} wtch/update-shape-dimensions
   #{:layout-item-min-w :layout-item-min-h :layout-item-max-w :layout-item-max-h} wtch/update-layout-sizing-limits
   ctt/rotation-keys wtch/update-rotation})

(def attribute-actions-map
 (reduce
  (fn [acc [ks action]]
    (into acc (map (fn [k] [k action]) ks)))
  {} attributes->shape-update))

;; Helpers ---------------------------------------------------------------------

(defn deep-merge
  "Like d/deep-merge but unions set values."
  ([a b]
   (cond
     (map? a) (merge-with deep-merge a b)
     (set? a) (set/union a b)
     :else b))
  ([a b & rest]
   (reduce deep-merge a (cons b rest))))

;; Data flows ------------------------------------------------------------------

(defn invert-collect-key-vals
  [xs resolved-tokens shape]
  (-> (reduce
       (fn [acc [k v]]
         (let [resolved-value (get-in resolved-tokens [v :resolved-value])]
           (if (and filter-existing-values? (= (get shape k) resolved-value))
             acc
             (update acc resolved-value (fnil conj #{}) k))))
       {} xs)))

(defn split-attribute-groups [attrs-values-map]
  (reduce
   (fn [acc [attrs v]]
     (cond
       (some attrs #{:rx :ry}) (let [[_ a b] (data/diff #{:rx :ry} attrs)]
                                 (assoc acc
                                        a v
                                        b v))

       (some attrs #{:widht :height}) (let [[_ a b] (data/diff #{:width :height} attrs)]
                                        (assoc acc
                                               a v
                                               b v))
       (some attrs ctt/spacing-keys) (let [[_ rst gap] (data/diff #{:row-gap :column-gap} attrs)
                                           [_ position padding] (data/diff #{:p1 :p2 :p3 :p4} rst)]
                                       (cond-> acc
                                         (seq gap) (assoc gap v)
                                         (seq position) (assoc position v)
                                         (seq padding) (assoc padding v)))
       :else (assoc acc attrs v)))
   {} attrs-values-map))

(defn shape-ids-by-values
  [attrs-values-map object-id]
  (->> (map (fn [[value attrs]] [attrs {value #{object-id}}]) attrs-values-map)
       (into {})))

(defn collect-shapes-update-info [resolved-tokens shapes]
  (reduce
   (fn [acc [object-id {:keys [applied-tokens] :as shape}]]
     (if (seq applied-tokens)
       (let [applied-tokens (-> (invert-collect-key-vals applied-tokens resolved-tokens shape)
                                (shape-ids-by-values object-id)
                                (split-attribute-groups))]
         (deep-merge acc applied-tokens))
       acc))
   {} shapes))

(defn actionize-shapes-update-info [shapes-update-info]
  (mapcat (fn [[attrs update-infos]]
            (let [action (some attribute-actions-map attrs)]
              (map
               (fn [[v shape-ids]]
                 (list action v shape-ids attrs))
               update-infos)))
       shapes-update-info))

(defn update-workspace-tokens []
  (let [resolved-tokens (wtsd/get-cached-tokens @refs/workspace-tokens)]
    (->> @refs/workspace-page-objects
         (collect-shapes-update-info resolved-tokens)
         (actionize-shapes-update-info))))

(defn update-workspace-tokens-event []
  (ptk/reify ::update-shape-position
    ptk/WatchEvent
    (watch [_ _ _]
      (->> (update-workspace-tokens)
           (mapv (fn [[f & attrs]]
                   (apply f attrs)))
           (brx/concat)))))

(comment
  (update-workspace-tokens)
  (st/emit! (update-workspace-tokens-event))
  nil)
