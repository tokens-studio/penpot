;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.tokens.sidebar
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.main.data.modal :as modal]
   [app.main.data.tokens :as dt]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.search-bar :refer [search-bar]]
   [app.main.ui.icons :as i]
   [app.main.ui.workspace.sidebar.assets.common :as cmm]
   [app.main.ui.workspace.tokens.common :as tcm]
   [app.main.ui.workspace.tokens.core :refer [tokens-applied?] :as wtc]
   [app.util.dom :as dom]
   [rumext.v2 :as mf]))

(defn on-apply-token [{:keys [token token-type-props selected-shapes] :as _props}]
  (let [{:keys [attributes on-apply on-update-shape]
         :or {on-apply dt/update-token-from-attributes}} token-type-props
        shape-ids (->> selected-shapes
                       (eduction
                        (remove #(tokens-applied? token % attributes))
                        (map :id)))
        token-value (d/parse-integer (:value token))]
    (doseq [shape selected-shapes]
      (st/emit! (on-apply {:token-id (:id token)
                           :shape-id (:id shape)
                           :attributes attributes}))
      (on-update-shape token-value shape-ids))))

(mf/defc token-pill
  {::mf/wrap-props false}
  [{:keys [on-click token highlighted? on-context-menu]}]
  (let [{:keys [name value]} token]
    [:div {:class (stl/css-case :token-pill true
                                :token-pill-highlighted highlighted?)
           :title (str "Token value: " value)
           :on-click on-click
           :on-context-menu on-context-menu}
     name]))

(mf/defc token-component
  [{:keys [type file tokens selected-shapes token-type-props]}]
  (let [open? (mf/use-state false)
        {:keys [modal attributes title]} token-type-props

        on-context-menu (mf/use-fn
                         (fn [event token]
                           (dom/prevent-default event)
                           (dom/stop-propagation event)
                           (st/emit! (tcm/show-token-context-menu {:type :token
                                                                   :position (dom/get-client-position event)
                                                                   :token-id (:id token)}))))

        on-toggle-open-click (mf/use-fn
                              (mf/deps open? tokens)
                              #(when (seq tokens)
                                 (swap! open? not)))
        on-popover-open-click (mf/use-fn
                               (fn [event]
                                 (let [{:keys [key fields]} modal]
                                   (dom/stop-propagation event)
                                   (modal/show! key {:x (.-clientX ^js event)
                                                     :y (.-clientY ^js event)
                                                     :position :right
                                                     :fields fields
                                                     :token-type type}))))

        on-token-pill-click (mf/use-fn
                             (mf/deps selected-shapes token-type-props)
                             (fn [event token]
                               (dom/stop-propagation event)
                               (on-apply-token {:token token
                                                :token-type-props token-type-props
                                                :selected-shapes selected-shapes})))
        tokens-count (count tokens)]
    [:div {:on-click on-toggle-open-click}
     [:& cmm/asset-section {:file-id (:id file)
                            :title title
                            :assets-count tokens-count
                            :open? @open?}
      [:& cmm/asset-section-block {:role :title-button}
       [:button {:class (stl/css :action-button)
                 :on-click on-popover-open-click}
        i/add]]
      (when open?
        [:& cmm/asset-section-block {:role :content}
         [:div {:class (stl/css :token-pills-wrapper)}
          (for [token tokens]
            [:& token-pill
             {:key (:id token)
              :token token
              :highlighted? (tokens-applied? token selected-shapes attributes)
              :on-click #(on-token-pill-click % token)
              :on-context-menu #(on-context-menu % token)}])]])]]))

(defn sorted-token-groups
  "Separate token-types into groups of `:empty` or `:filled` depending if tokens exist for that type.
  Sort each group alphabetically (by their `:token-key`)."
  [tokens]
  (let [tokens-by-group (->> (vals tokens)
                             (group-by :type))
        {:keys [empty filled]} (->> wtc/token-types
                                    (map (fn [[token-key token-type-props]]
                                           {:token-key token-key
                                            :token-type-props token-type-props
                                            :tokens (get tokens-by-group token-key [])}))
                                    (group-by (fn [{:keys [tokens]}]
                                                (if (empty? tokens) :empty :filled))))]
    {:empty (sort-by :token-key empty)
     :filled (sort-by :token-key filled)}))

(mf/defc tokens-explorer
  [_props]
  (let [file (mf/deref refs/workspace-file)
        current-page-id (:current-page-id @st/state)
        workspace-data (mf/deref refs/workspace-data)
        tokens (get workspace-data :tokens)
        token-groups (mf/with-memo [tokens]
                       (sorted-token-groups tokens))
        selected-shape-ids (mf/deref refs/selected-shapes)
        selected-shapes (tcm/workspace-shapes workspace-data current-page-id selected-shape-ids)]
    [:article
     [:div.assets-bar
      (for [{:keys [token-key token-type-props tokens]} (concat (:filled token-groups)
                                                                (:empty token-groups))]
        [:& token-component {:key token-key
                             :type token-key
                             :file file
                             :selected-shapes selected-shapes
                             :tokens tokens
                             :token-type-props token-type-props}])]]))

(mf/defc tokens-sidebar-tab
  {::mf/wrap [mf/memo]
   ::mf/wrap-props false}
  [_props]
  [:div {:class (stl/css :sidebar-tab-wrapper)}
   [:& tokens-explorer]])
