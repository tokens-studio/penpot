;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.tokens.modals.themes
  (:require-macros [app.main.style :as stl])
  (:require
   [app.main.data.modal :as modal]
   [app.main.ui.components.radio-buttons :refer [radio-button radio-buttons]]
   [cuerdas.core :as str]
   [app.main.ui.workspace.tokens.sets :as wts]
   [app.main.data.tokens :as wdt]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.icons :as i]
   [app.util.dom :as dom]
   [rumext.v2 :as mf]))

(def ^:private chevron-icon
  (i/icon-xref :arrow (stl/css :chevron-icon)))

(def ^:private close-icon
  (i/icon-xref :close (stl/css :close-icon)))

(mf/defc empty-themes
  [{:keys []}]
  "Empty")


(mf/defc switch
  [{:keys [selected? name on-change]}]
  (let [selected (if selected? :on :off)]
    [:& radio-buttons {:selected selected
                       :on-change on-change
                       :name name}
     [:& radio-button {:id :on
                       :value :on
                       :icon i/tick
                       :label ""}]
     [:& radio-button {:id :off
                       :value :off
                       :icon i/close
                       :label ""}]]))

(mf/defc themes-overview
  [{:keys [set-state]}]
  (let [active-theme-ids (mf/deref refs/workspace-active-theme-ids)
        themes (mf/deref refs/workspace-ordered-token-themes)
        on-edit-theme (fn [theme e]
                        (dom/prevent-default e)
                        (dom/stop-propagation e)
                        (set-state (fn [_] {:type :edit-theme
                                            :theme theme})))]
    [:div
     [:ul {:class (stl/css :theme-group-wrapper)}
      (for [[group themes] themes]
        [:li {:key (str "token-theme-group" group)}
         (when (seq group)
           [:span {:class (stl/css :theme-group-label)} group])
         [:ul {:class (stl/css :theme-group-rows-wrapper)}
          (for [{:keys [id name] :as theme} themes
                :let [selected? (some? (get active-theme-ids id))]]
            [:li {:key (str "token-theme-" id)
                  :class (stl/css :theme-row)}
             [:div {:class (stl/css :theme-row-left)}
              [:div {:on-click (fn [e]
                                 (dom/prevent-default e)
                                 (dom/stop-propagation e)
                                 (st/emit! (wdt/toggle-token-theme id)))}
               [:& switch {:name (str "Theme" name)
                           :on-change (constantly nil)
                           :selected? selected?}]]
              [:span {:class (stl/css :theme-row-label)} name]]
             [:div {:class (stl/css :theme-row-right)}
              (if-let [sets-count (some-> theme :sets seq count)]
                [:button {:class (stl/css :sets-count-button)
                          :on-click #(on-edit-theme theme %)}
                 (str sets-count " sets")
                 chevron-icon]
                [:button {:class (stl/css :sets-count-empty-button)
                          :on-click #(on-edit-theme theme %)}
                 "No sets defined"
                 chevron-icon])
              [:div {:class (stl/css :delete-theme-button)}
               [:button {:on-click (fn [e]
                                     (dom/prevent-default e)
                                     (dom/stop-propagation e)
                                     (st/emit! (wdt/delete-token-theme id)))}
                i/delete]]]])]])]
     [:div {:class (stl/css :button-footer)}
      [:button {:class (stl/css :create-theme-button)}
       i/add
       "Create theme"]]]))

(mf/defc edit-theme
  [{:keys [state]}]
  (let [{:keys [theme]} @state
        token-sets (mf/deref refs/workspace-token-sets)
        selected-token-set-id (mf/deref refs/workspace-selected-token-set-id)
        token-set-selected? (mf/use-callback
                             (mf/deps selected-token-set-id)
                             (fn [id]
                               (= id selected-token-set-id)))
        active-token-set-ids (mf/deref refs/workspace-active-set-ids)
        token-set-active? (mf/use-callback
                           (mf/deps active-token-set-ids)
                           (fn [id]
                             (get active-token-set-ids id)))]
    [:div {:class (stl/css :sets-list-wrapper)}
     [:& wts/controlled-sets-list
      {:token-sets token-sets
       :token-set-selected? (constantly false)
       :token-set-active? token-set-active?
       :on-select (fn [id]
                    (js/console.log "id" id))
       :on-toggle (fn [id]
                    (js/console.log "id" id))}]]))

(mf/defc themes
  [{:keys [] :as _args}]
  (let [active-theme-ids (mf/deref refs/workspace-active-theme-ids)
        themes (mf/deref refs/workspace-ordered-token-themes)
        state (mf/use-state (if (empty? themes)
                              {:type :empty-themes}
                              {:type :themes-overview}))
        set-state (mf/use-callback #(swap! state %))
        title (case (:type @state)
                :edit-theme "Edit Theme"
                "Themes")
        component (case (:type @state)
                    :empty-themes empty-themes
                    :themes-overview themes-overview
                    :edit-theme edit-theme)]
    [:div

     [:div {:class (stl/css :modal-title)} title]
     [:div {:class (stl/css :modal-content)}
      [:& component {:state state
                     :set-state set-state}]]]))

(mf/defc modal
  {::mf/wrap-props false}
  [{:keys [] :as _args}]
  (let [handle-close-dialog (mf/use-callback #(st/emit! (modal/hide)))]
    [:div {:class (stl/css :modal-overlay)}
     [:div {:class (stl/css :modal-dialog)}
      [:button {:class (stl/css :close-btn) :on-click handle-close-dialog} close-icon]
      [:& themes]]]))
