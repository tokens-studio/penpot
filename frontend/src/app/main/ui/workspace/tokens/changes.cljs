;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.tokens.changes
  (:require
   [app.common.types.shape.radius :as ctsr]
   [app.common.types.token :as ctt]
   [app.main.data.tokens :as dt]
   [app.main.data.workspace :as udw]
   [app.main.data.workspace.changes :as dch]
   [app.main.data.workspace.shape-layout :as dwsl]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.data.workspace.undo :as dwu]
   [app.main.store :as st]
   [app.main.ui.workspace.tokens.style-dictionary :as sd]
   [app.main.ui.workspace.tokens.token :as wtt]
   [beicon.v2.core :as rx]
   [clojure.set :as set]
   [potok.v2.core :as ptk]
   [promesa.core :as p]))

;; Token Updates ---------------------------------------------------------------

(defn apply-token
  "Apply `attributes` that match `token` for `shape-ids`.

  Optionally remove attributes from `attributes-to-remove`,
  this is useful for applying a single attribute from an attributes set
  while removing other applied tokens from this set."
  [{:keys [attributes attributes-to-remove token shape-ids on-update-shape] :as _props}]
  (ptk/reify ::apply-token
    ptk/WatchEvent
    (watch [_ state _]
      (->> (rx/from (sd/resolve-tokens+ (get-in state [:workspace-data :tokens])))
           (rx/mapcat
            (fn [sd-tokens]
              (let [undo-id (js/Symbol)
                    resolved-value (-> (get sd-tokens (:id token))
                                       (wtt/resolve-token-value))
                    tokenized-attributes (wtt/attributes-map attributes (:id token))]
                (rx/of
                 (dwu/start-undo-transaction undo-id)
                 (dch/update-shapes shape-ids (fn [shape]
                                                (cond-> shape
                                                  attributes-to-remove (update :applied-tokens #(apply (partial dissoc %) attributes-to-remove))
                                                  :always (update :applied-tokens merge tokenized-attributes))))
                 (when on-update-shape
                   (on-update-shape resolved-value shape-ids attributes))
                 (dwu/commit-undo-transaction undo-id)))))))))

(defn unapply-token
  "Removes `attributes` that match `token` for `shape-ids`.

  Doesn't update shape attributes."
  [{:keys [attributes token shape-ids] :as _props}]
  (ptk/reify ::unapply-token
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of
       (let [remove-token #(when % (wtt/remove-attributes-for-token-id attributes (:id token) %))]
         (dch/update-shapes
          shape-ids
          (fn [shape]
            (update shape :applied-tokens remove-token))))))))

(defn toggle-token
  [{:keys [token-type-props token shapes] :as _props}]
  (ptk/reify ::on-toggle-token
    ptk/WatchEvent
    (watch [_ _ _]
      (let [{:keys [attributes on-update-shape]} token-type-props
            unapply-tokens? (wtt/shapes-token-applied? token shapes (:attributes token-type-props))
            shape-ids (map :id shapes)]
        (if unapply-tokens?
          (rx/of
           (unapply-token {:attributes attributes
                           :token token
                           :shape-ids shape-ids}))
          (rx/of
           (apply-token {:attributes attributes
                         :token token
                         :shape-ids shape-ids
                         :on-update-shape on-update-shape})))))))

(defn on-apply-token [{:keys [token token-type-props selected-shapes] :as _props}]
  (let [{:keys [attributes on-apply on-update-shape]
         :or {on-apply dt/update-token-from-attributes}} token-type-props
        shape-ids (->> selected-shapes
                       (eduction
                        (remove #(wtt/shapes-token-applied? token % attributes))
                        (map :id)))]

    (p/let [sd-tokens (sd/resolve-workspace-tokens+ {:debug? true})]
      (let [resolved-token (get sd-tokens (:id token))
            resolved-token-value (wtt/resolve-token-value resolved-token)]
        (doseq [shape selected-shapes]
          (st/emit! (on-apply {:token-id (:id token)
                               :shape-id (:id shape)
                               :attributes attributes}))
          (on-update-shape resolved-token-value shape-ids attributes))))))

;; Shape Updates ---------------------------------------------------------------

(defn update-shape-radius-all [value shape-ids]
  (dch/update-shapes shape-ids
                     (fn [shape]
                       (when (ctsr/has-radius? shape)
                         (ctsr/set-radius-1 shape value)))
                     {:reg-objects? true
                      :attrs ctt/border-radius-keys}))

(defn update-opacity [value shape-ids]
  (dch/update-shapes shape-ids #(assoc % :opacity value)))

(defn update-rotation [value shape-ids]
  (ptk/reify ::update-shape-dimensions
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of
       (udw/trigger-bounding-box-cloaking shape-ids)
       (udw/increase-rotation shape-ids value)))))

(defn update-shape-radius-single-corner [value shape-ids attributes]
  (dch/update-shapes shape-ids
                     (fn [shape]
                       (when (ctsr/has-radius? shape)
                         (cond-> shape
                           (:rx shape) (ctsr/switch-to-radius-4)
                           :always (ctsr/set-radius-4 (first attributes) value))))
                     {:reg-objects? true
                      :attrs [:rx :ry :r1 :r2 :r3 :r4]}))

(defn update-stroke-width
  [value shape-ids]
  (dch/update-shapes shape-ids (fn [shape]
                                 (when (seq (:strokes shape))
                                   (assoc-in shape [:strokes 0 :stroke-width] value)))))

(defn update-layout-spacing [value shape-ids attributes]
  (if-let [layout-gap (cond
                          (:row-gap attributes) {:row-gap value}
                          (:column-gap attributes) {:column-gap value})]
    (dwsl/update-layout shape-ids {:layout-gap layout-gap})
    (dwsl/update-layout shape-ids {:layout-padding (zipmap attributes (repeat value))})))

(defn update-shape-dimensions [value shape-ids attributes]
  (ptk/reify ::update-shape-dimensions
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of
       (when (:width attributes) (dwt/update-dimensions shape-ids :width value))
       (when (:height attributes) (dwt/update-dimensions shape-ids :height value))))))

(defn update-layout-spacing-column [value shape-ids]
  (ptk/reify ::update-layout-spacing-column
    ptk/WatchEvent
    (watch [_ state _]
      (rx/concat
       (for [shape-id shape-ids]
         (let [shape (dt/get-shape-from-state shape-id state)
               layout-direction (:layout-flex-dir shape)
               layout-update (if (or (= layout-direction :row-reverse) (= layout-direction :row))
                               {:layout-gap {:column-gap value}}
                               {:layout-gap {:row-gap value}})]
           (dwsl/update-layout [shape-id] layout-update)))))))

(defn update-shape-position [value shape-ids attributes]
  (doseq [shape-id shape-ids]
    (st/emit! (dwt/update-position shape-id {(first attributes) value}))))

(defn apply-dimensions-token [{:keys [token-id token-type-props selected-shapes]} attributes]
  (let [token (dt/get-token-data-from-token-id token-id)
        attributes (set attributes)
        updated-token-type-props (cond
                                   (set/superset? #{:x :y} attributes)
                                   (assoc token-type-props
                                          :on-update-shape update-shape-position
                                          :attributes attributes)

                                   (set/superset? #{:stroke-width} attributes)
                                   (assoc token-type-props
                                          :on-update-shape update-stroke-width
                                          :attributes attributes)

                                   :else token-type-props)]
    (on-apply-token {:token token
                     :token-type-props updated-token-type-props
                     :selected-shapes selected-shapes})))

(defn update-layout-sizing-limits [value shape-ids attributes]
  (ptk/reify ::update-layout-sizing-limits
    ptk/WatchEvent
    (watch [_ _ _]
      (let [props (-> {:layout-item-min-w value
                       :layout-item-min-h value
                       :layout-item-max-w value
                       :layout-item-max-h value}
                      (select-keys attributes))]
        (dwsl/update-layout-child shape-ids props)))))
