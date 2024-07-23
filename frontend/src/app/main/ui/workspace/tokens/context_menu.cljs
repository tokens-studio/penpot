;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.tokens.context-menu
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.types.shape.radius :as ctsr]
   [app.main.data.modal :as modal]
   [app.main.data.shortcuts :as scd]
   [app.main.data.tokens :as dt]
   [app.main.data.workspace :as dw]
   [app.main.data.workspace.changes :as dch]
   [app.main.data.workspace.shape-layout :as dwsl]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.dropdown :refer [dropdown]]
   [app.main.ui.icons :as i]
   [app.main.ui.workspace.tokens.core :as wtc]
   [app.main.ui.workspace.tokens.token :as wtt]
   [app.util.dom :as dom]
   [app.util.timers :as timers]
   [clojure.set :as set :refer [rename-keys]]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def tokens-menu-ref
  (l/derived :token-context-menu refs/workspace-local))

(defn- prevent-default
  [event]
  (dom/prevent-default event)
  (dom/stop-propagation event))

(mf/defc menu-entry
  {::mf/props :obj}
  [{:keys [title shortcut on-click on-pointer-enter on-pointer-leave
           on-unmount children selected? icon disabled value]}]
  (let [submenu-ref (mf/use-ref nil)
        hovering?   (mf/use-ref false)
        on-pointer-enter
        (mf/use-callback
         (fn []
           (mf/set-ref-val! hovering? true)
           (let [submenu-node (mf/ref-val submenu-ref)]
             (when (some? submenu-node)
               (dom/set-css-property! submenu-node "display" "block")))
           (when on-pointer-enter (on-pointer-enter))))

        on-pointer-leave
        (mf/use-callback
         (fn []
           (mf/set-ref-val! hovering? false)
           (let [submenu-node (mf/ref-val submenu-ref)]
             (when (some? submenu-node)
               (timers/schedule
                50
                #(when-not (mf/ref-val hovering?)
                   (dom/set-css-property! submenu-node "display" "none")))))
           (when on-pointer-leave (on-pointer-leave))))

        set-dom-node
        (mf/use-callback
         (fn [dom]
           (let [submenu-node (mf/ref-val submenu-ref)]
             (when (and (some? dom) (some? submenu-node))
               (dom/set-css-property! submenu-node "top" (str (.-offsetTop dom) "px"))))))]

    (mf/use-effect
     (mf/deps on-unmount)
     (constantly on-unmount))

    (if icon
      [:li {:class (stl/css :icon-menu-item)
            :disabled disabled
            :data-value value
            :ref set-dom-node
            :on-click on-click
            :on-pointer-enter on-pointer-enter
            :on-pointer-leave on-pointer-leave}
       [:span
        {:class (stl/css :icon-wrapper)}
        (if selected? [:span {:class (stl/css :selected-icon)}
                       i/tick]
            [:span {:class (stl/css :selected-icon)}])
        [:span {:class (stl/css :shape-icon)} icon]]
       [:span {:class (stl/css :title)} title]]
      [:li {:class (stl/css :context-menu-item)
            :disabled disabled
            :ref set-dom-node
            :data-value value
            :on-click on-click
            :on-pointer-enter on-pointer-enter
            :on-pointer-leave on-pointer-leave}
       [:span {:class (stl/css :title)} title]
       (when shortcut
         [:span   {:class (stl/css :shortcut)}
          (for [[idx sc] (d/enumerate (scd/split-sc shortcut))]
            [:span {:key (dm/str shortcut "-" idx)
                    :class (stl/css :shortcut-key)} sc])])

       (when (> (count children) 1)
         [:span {:class (stl/css :submenu-icon)} i/arrow])

       (when (> (count children) 1)
         [:ul {:class (stl/css :token-context-submenu)
               :ref submenu-ref
               :style {:display "none" :left 235}
               :on-context-menu prevent-default}
          children])])))

(mf/defc menu-separator
  []
  [:li {:class (stl/css :separator)}])

(defn update-shape-radius-single-corner [value shape-ids attribute]
  (st/emit!
   (dch/update-shapes shape-ids
                      (fn [shape]
                        (when (ctsr/has-radius? shape)
                          (ctsr/set-radius-4 shape (first attribute) value)))
                      {:reg-objects? true
                       :attrs [:rx :ry :r1 :r2 :r3 :r4]})))

(defn apply-border-radius-token [{:keys [token-id token-type-props selected-shapes]} attributes]
  (let [token (dt/get-token-data-from-token-id token-id)
        updated-token-type-props (if (set/superset? #{:r1 :r2 :r3 :r4} attributes)
                                   (assoc token-type-props
                                          :on-update-shape update-shape-radius-single-corner
                                          :attributes attributes)
                                   token-type-props)]
    (wtc/on-apply-token {:token token
                         :token-type-props updated-token-type-props
                         :selected-shapes selected-shapes})))

(defn update-layout-spacing [value shape-ids attributes]
  (if-let [layout-gap (cond
                          (:row-gap attributes) {:row-gap value}
                          (:column-gap attributes) {:column-gap value})]
    (dwsl/update-layout shape-ids {:layout-gap layout-gap})
    (dwsl/update-layout shape-ids {:layout-padding (zipmap attributes (repeat value))})))

(defn apply-spacing-token [{:keys [token-id token-type-props selected-shapes]} attributes]
  (let [token (dt/get-token-data-from-token-id token-id)
        attributes (set attributes)
        updated-token-type-props (assoc token-type-props
                                        :on-update-shape update-layout-spacing
                                        :attributes attributes)]
    (wtc/on-apply-token {:token token
                         :token-type-props updated-token-type-props
                         :selected-shapes selected-shapes})))

(defn update-shape-position [value shape-ids attributes]
  (doseq [shape-id shape-ids]
    (st/emit! (dw/update-position shape-id {(first attributes) value}))))

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
                                          :on-update-shape wtc/update-stroke-width
                                          :attributes attributes)

                                   :else token-type-props)]
    (wtc/on-apply-token {:token token
                         :token-type-props updated-token-type-props
                         :selected-shapes selected-shapes})))

(defn update-shape-dimensions [value shape-ids attributes]
  (st/emit! (dwt/update-dimensions shape-ids (first attributes) value)))

(defn update-layout-sizing-limits [value shape-ids attributes]
  (st/emit! (dwsl/update-layout-child shape-ids {(first attributes) value})))

(defn apply-sizing-token [{:keys [token-id token-type-props selected-shapes]} attributes]
  (let [token (dt/get-token-data-from-token-id token-id)
        updated-token-type-props (cond
                                   (set/superset? #{:width :height} attributes)
                                   (assoc token-type-props
                                          :on-update-shape update-shape-dimensions
                                          :attributes attributes)

                                   (set/superset? #{:layout-item-min-w :layout-item-max-w
                                                    :layout-item-min-h :layout-item-max-h} attributes)
                                   (assoc token-type-props
                                          :on-update-shape update-layout-sizing-limits
                                          :attributes attributes)

                                   :else token-type-props)]
    (wtc/on-apply-token {:token token
                         :token-type-props updated-token-type-props
                         :selected-shapes selected-shapes})))

(defn apply-rotation-opacity-stroke-token [{:keys [token-id token-type-props selected-shapes]} attributes]
  (let [token (dt/get-token-data-from-token-id token-id)]
    (wtc/on-apply-token {:token token
                         :token-type-props token-type-props
                         :selected-shapes selected-shapes})))

(defn attribute-actions [token selected-shapes attributes]
  (let [ids-by-attributes (wtt/shapes-ids-by-applied-attributes token selected-shapes attributes)
        shape-ids (into #{} (map :id selected-shapes))]
    {:all-selected? (wtt/shapes-applied-all? ids-by-attributes shape-ids attributes)
     :shape-ids shape-ids
     :selected-pred #(seq (% ids-by-attributes))}))

(defn border-radius-attribute-actions [{:keys [token selected-shapes]}]
  (let [all-attributes #{:r1 :r2 :r3 :r4}
        {:keys [all-selected? selected-pred shape-ids]} (attribute-actions token selected-shapes all-attributes)
        single-attributes (->> {:r1 "Top Left"
                                :r2 "Top Right"
                                :r3 "Bottom Left"
                                :r4 "Bottom Right"}
                               (map (fn [[attr title]]
                                      (let [selected? (selected-pred attr)]
                                        {:title title
                                         :selected? (and (not all-selected?) selected?)
                                         :action #(let [props {:attributes #{attr}
                                                               :token token
                                                               :shape-ids shape-ids}
                                                        event (cond
                                                                all-selected? (-> (assoc props :attributes-to-remove #{:r1 :r2 :r3 :r4 :rx :ry})
                                                                                  (wtc/apply-token))
                                                                selected? (wtc/unapply-token props)
                                                                :else (-> (assoc props :on-update-shape wtc/update-shape-radius-single-corner)
                                                                          (wtc/apply-token)))]
                                                    (st/emit! event))}))))
        all-attribute (let [props {:attributes all-attributes
                                   :token token
                                   :shape-ids shape-ids}]
                        {:title "All"
                         :selected? all-selected?
                         :action #(if all-selected?
                                    (st/emit! (wtc/unapply-token props))
                                    (st/emit! (wtc/apply-token (assoc props :on-update-shape wtc/update-shape-radius-all))))})]
    (concat [all-attribute] single-attributes)))

(def spacing
  {:padding {:p1 "Top"
             :p2 "Right"
             :p3 "Bottom"
             :p4 "Left"}
   :gap {:column-gap "Column Gap"
         :row-gap "Row Gap"}})

(defn gap-attribute-actions [{:keys [token selected-shapes]}]
  (let [on-update-shape update-layout-spacing
        gap-attrs (:gap spacing)
        all-gap-attrs (into #{} (keys gap-attrs))
        {:keys [all-selected? selected-pred shape-ids]} (attribute-actions token selected-shapes all-gap-attrs)
        all-gap (let [props {:attributes all-gap-attrs
                             :token token
                             :shape-ids shape-ids}]
                  [{:title "All"
                    :selected? all-selected?
                    :action #(if all-selected?
                               (st/emit! (wtc/unapply-token props))
                               (st/emit! (wtc/apply-token (assoc props :on-update-shape on-update-shape))))}])
        single-gap (->> gap-attrs
                        (map (fn [[attr title]]
                               (let [selected? (selected-pred attr)]
                                 {:title title
                                  :selected? (and (not all-selected?) selected?)
                                  :action #(let [props {:attributes #{attr}
                                                        :token token
                                                        :shape-ids shape-ids}
                                                 event (cond
                                                         all-selected? (-> (assoc props :attributes-to-remove #{:row-gap :column-gap})
                                                                           (wtc/apply-token))
                                                         selected? (wtc/unapply-token props)
                                                         :else (-> (assoc props :on-update-shape on-update-shape)
                                                                   (wtc/apply-token)))]
                                             (st/emit! event))})))
                        (into))]
    (concat all-gap single-gap)))

(defn spacing-attribute-actions [{:keys [token selected-shapes] :as context-data}]
  (let [on-update-shape (fn [resolved-value shape-ids attrs]
                          (dwsl/update-layout shape-ids {:layout-padding (zipmap attrs (repeat resolved-value))}))
        padding-attrs (:padding spacing)
        all-padding-attrs (into #{} (keys padding-attrs))
        {:keys [all-selected? selected-pred shape-ids]} (attribute-actions token selected-shapes all-padding-attrs)
        horizontal-attributes #{:p1 :p3}
        horizontal-padding-selected? (and
                                      (not all-selected?)
                                      (every? selected-pred horizontal-attributes))
        vertical-attributes #{:p2 :p4}
        vertical-padding-selected? (and
                                    (not all-selected?)
                                    (every? selected-pred vertical-attributes))
        padding-items [{:title "All"
                        :selected? all-selected?
                        :action (fn []
                                  (let [props {:attributes all-padding-attrs
                                               :token token
                                               :shape-ids shape-ids}]
                                    (if all-selected?
                                      (st/emit! (wtc/unapply-token props))
                                      (st/emit! (wtc/apply-token (assoc props :on-update-shape on-update-shape))))))}
                       {:title "Horizontal"
                        :selected? horizontal-padding-selected?
                        :action (fn []
                                  (let [props {:token token
                                               :shape-ids shape-ids}
                                        event (cond
                                                all-selected? (wtc/apply-token (assoc props :attributes-to-remove vertical-attributes))
                                                horizontal-padding-selected? (wtc/apply-token (assoc props :attributes-to-remove horizontal-attributes))
                                                :else (wtc/apply-token (assoc props
                                                                              :attributes horizontal-attributes
                                                                              :on-update-shape on-update-shape)))]
                                    (st/emit! event)))}
                       {:title "Vertical"
                        :selected? vertical-padding-selected?
                        :action (fn []
                                  (let [props {:token token
                                               :shape-ids shape-ids}
                                        event (cond
                                                all-selected? (wtc/apply-token (assoc props :attributes-to-remove vertical-attributes))
                                                vertical-padding-selected? (wtc/apply-token (assoc props :attributes-to-remove vertical-attributes))
                                                :else (wtc/apply-token (assoc props
                                                                              :attributes vertical-attributes
                                                                              :on-update-shape on-update-shape)))]
                                    (st/emit! event)))}]
        single-padding-items (->> padding-attrs
                                  (map (fn [[attr title]]
                                         (let [same-axis-selected? (cond
                                                                     (get horizontal-attributes attr) horizontal-padding-selected?
                                                                     (get vertical-attributes attr) vertical-padding-selected?
                                                                     :else true)
                                               selected? (and
                                                          (not all-selected?)
                                                          (not same-axis-selected?)
                                                          (selected-pred attr))]
                                           {:title title
                                            :selected? selected?
                                            :action #(let [props {:attributes #{attr}
                                                                  :token token
                                                                  :shape-ids shape-ids}
                                                           event (cond
                                                                   all-selected? (-> (assoc props :attributes-to-remove all-padding-attrs)
                                                                                     (wtc/apply-token))
                                                                   selected? (wtc/unapply-token props)
                                                                   :else (-> (assoc props :on-update-shape on-update-shape)
                                                                             (wtc/apply-token)))]
                                                       (st/emit! event))}))))
        gap-items (gap-attribute-actions context-data)]
    (concat padding-items
            single-padding-items
            [:separator]
            gap-items)))

(defn all-or-sepearate-actions [attribute-labels on-update-shape {:keys [token selected-shapes]}]
  (let [attributes (set (keys attribute-labels))
        {:keys [all-selected? selected-pred shape-ids]} (attribute-actions token selected-shapes attributes)
        all-action (let [props {:attributes attributes
                                :token token
                                :shape-ids shape-ids}]
                     {:title "All"
                      :selected? all-selected?
                      :action #(if all-selected?
                                 (st/emit! (wtc/unapply-token props))
                                 (st/emit! (wtc/apply-token (assoc props :on-update-shape on-update-shape))))})
        single-actions (map (fn [[attr title]]
                              (let [selected? (selected-pred attr)]
                                {:title title
                                 :selected? (and (not all-selected?) selected?)
                                 :action #(let [props {:attributes #{attr}
                                                       :token token
                                                       :shape-ids shape-ids}
                                                event (cond
                                                        all-selected? (-> (assoc props :attributes-to-remove attributes)
                                                                          (wtc/apply-token))
                                                        selected? (wtc/unapply-token props)
                                                        :else (-> (assoc props :on-update-shape on-update-shape)
                                                                  (wtc/apply-token)))]
                                            (st/emit! event))}))
                            attribute-labels)]
    (concat [all-action] single-actions)))
(defn generic-attribute-actions [attributes title {:keys [token selected-shapes]}]
  (let [{:keys [on-update-shape] :as p} (get wtc/token-types (:type token))
        {:keys [selected-pred shape-ids]} (attribute-actions token selected-shapes attributes)]
    (map (fn [attribute]
           (let [selected? (selected-pred attribute)
                 props {:attributes #{attribute}
                        :token token
                        :shape-ids shape-ids}]

             {:title title
              :selected? selected?
              :action #(if selected?
                         (st/emit! (wtc/unapply-token props))
                         (st/emit! (wtc/apply-token (assoc props :on-update-shape on-update-shape))))}))
         attributes)))

(def shape-attribute-actions-map
  {:border-radius border-radius-attribute-actions
   :spacing spacing-attribute-actions
   :rotation (partial generic-attribute-actions #{:rotation} "Rotation")
   :opacity (partial generic-attribute-actions #{:opacity} "Opacity")
   :stroke-width (partial generic-attribute-actions #{:stroke-width} "Stroke Width")})

(defn shape-attribute-actions [{:keys [token] :as context-data}]
  (when-let [with-actions (get shape-attribute-actions-map (:type token))]
    (with-actions context-data)))

(defn shape-attribute-actions* [{:keys [token selected-shapes] :as context-data}]
  (let [attributes->actions (fn [update-fn coll]
                              (for [{:keys [attributes] :as item} coll]
                                (cond
                                  (= :separator item) item
                                  :else
                                  (let [selected? (wtt/shapes-token-applied? {:id token} selected-shapes attributes)]
                                    (assoc item
                                           :action #(update-fn context-data attributes)
                                           :selected? selected?)))))]
    (case (:type token)
      :sizing       (attributes->actions
                     apply-sizing-token
                     [{:title "All" :attributes #{:width :height :layout-item-min-w :layout-item-max-w :layout-item-min-h :layout-item-max-h}}
                      {:title "Width" :attributes #{:width}}
                      {:title "Height" :attributes #{:height}}
                      {:title "Min width" :attributes #{:layout-item-min-w}}
                      {:title "Max width" :attributes #{:layout-item-max-w}}
                      {:title "Min height" :attributes #{:layout-item-min-h}}
                      {:title "Max height" :attributes #{:layout-item-max-h}}])

      :dimensions    (attributes->actions
                      apply-dimensions-token
                      [{:title "Spacing" :submenu :spacing}
                       {:title "Sizing" :submenu :sizing}
                       {:title "Border Radius" :submenu :border-radius}
                       {:title "Border Width" :attributes #{:stroke-width}}
                       {:title "x" :attributes #{:x}}
                       {:title "y" :attributes #{:y}}])
                      ;;TODO: Background blur {:title "Background blur" :attributes #{:width}}])



      [])))

(defn generate-menu-entries [{:keys [token selected-shapes] :as context-data}]
  (let [{:keys [modal]} (get wtc/token-types (:type token))
        attribute-actions (when (seq selected-shapes)
                            (shape-attribute-actions context-data))
        default-actions [{:title "Delete Token" :action #(st/emit! (dt/delete-token (:id token)))}
                         {:title "Duplicate Token" :action #(st/emit! (dt/duplicate-token (:id token)))}
                         {:title "Edit Token" :action (fn [event]
                                                        (let [{:keys [key fields]} modal
                                                              token (dt/get-token-data-from-token-id (:id token))]
                                                          (st/emit! dt/hide-token-context-menu)
                                                          (dom/stop-propagation event)
                                                          (modal/show! key {:x (.-clientX ^js event)
                                                                            :y (.-clientY ^js event)
                                                                            :position :right
                                                                            :fields fields
                                                                            :token token})))}]]
    (concat
     attribute-actions
     (when attribute-actions [:separator])
     default-actions)))

(mf/defc token-pill-context-menu
  [context-data]
  (let [menu-entries (generate-menu-entries context-data)]
    (for [[index {:keys [title action selected? submenu] :as entry}] (d/enumerate menu-entries)]
      (cond
        (= :separator entry) [:& menu-separator]
        :else
        [:& menu-entry (cond-> {:key index
                                :title title}
                         (not submenu) (assoc :on-click action
                                              ;; TODO: Allow selected items wihtout an icon for the context menu
                                              :icon (mf/html [:div {:class (stl/css-case :empty-icon true
                                                                                         :hidden-icon (not selected?))}])
                                              :selected? selected?))
         (when submenu
           (let [submenu-entries (shape-attribute-actions (assoc context-data :token-type submenu))]
             (for [[index {:keys [title action selected?] :as sub-entry}] (d/enumerate submenu-entries)]
               (cond
                 (= :separator sub-entry) [:& menu-separator]
                 :else
                 [:& menu-entry {:key index
                                 :title title
                                 :on-click action
                                 :icon  (mf/html [:div {:class (stl/css-case :empty-icon true
                                                                             :hidden-icon (not selected?))}])
                                 :selected? selected?}]))))]))))

(mf/defc token-context-menu
  []
  (let [mdata          (mf/deref tokens-menu-ref)
        top            (- (get-in mdata [:position :y]) 20)
        left           (get-in mdata [:position :x])
        dropdown-ref   (mf/use-ref)
        objects (mf/deref refs/workspace-page-objects)
        selected (mf/deref refs/selected-shapes)
        selected-shapes (into [] (keep (d/getf objects)) selected)
        token-id (:token-id mdata)
        token (get (mf/deref refs/workspace-tokens) token-id)]
    (mf/use-effect
     (mf/deps mdata)
     #(let [dropdown (mf/ref-val dropdown-ref)]
        (when dropdown
          (let [bounding-rect (dom/get-bounding-rect dropdown)
                window-size (dom/get-window-size)
                delta-x (max (- (+ (:right bounding-rect) 250) (:width window-size)) 0)
                delta-y (max (- (:bottom bounding-rect) (:height window-size)) 0)
                new-style (str "top: " (- top delta-y) "px; "
                               "left: " (- left delta-x) "px;")]
            (when (or (> delta-x 0) (> delta-y 0))
              (.setAttribute ^js dropdown "style" new-style))))))
    [:& dropdown {:show (boolean mdata)
                  :on-close #(st/emit! dt/hide-token-context-menu)}
     [:div {:class (stl/css :token-context-menu)
            :ref dropdown-ref
            :style {:top top :left left}
            :on-context-menu prevent-default}
      (when  (= :token (:type mdata))
        [:ul {:class (stl/css :context-list)}
         [:& token-pill-context-menu {:token token
                                      :selected-shapes selected-shapes}]])]]))
