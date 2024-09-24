;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.tokens.theme-select
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.types.tokens-lib :as ctob]
   [app.common.uuid :as uuid]
   [app.main.data.modal :as modal]
   [app.main.data.tokens :as wdt]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.dropdown :refer [dropdown]]
   [app.main.ui.icons :as i]
   [app.util.dom :as dom]
   [rumext.v2 :as mf]))

(mf/defc themes-list
  [{:keys [themes active-theme-ids on-close grouped?]}]
  (when (seq themes)
    [:ul
     (for [[_ {:keys [id group name] :as theme}] themes
           :let [selected? (get active-theme-ids (ctob/theme-path theme))]]
       [:li {:key id
             :class (stl/css-case
                     :checked-element true
                     :sub-item grouped?
                     :is-selected selected?)
             :on-click (fn [e]
                         (dom/stop-propagation e)
                         (st/emit! (wdt/toggle-token-theme-active? group name))
                         (on-close))}
        [:span {:class (stl/css :label)} name]
        [:span {:class (stl/css :check-icon)} i/tick]])]))

(mf/defc theme-options
  [{:keys [on-close]}]
  (let [active-theme-ids (dm/fixme (mf/deref refs/workspace-active-theme-paths))
        theme-groups (mf/deref refs/workspace-token-theme-tree)]
    [:ul
     (for [[group themes] theme-groups]
       [:li {:key group}
        (when (seq group)
          [:span {:class (stl/css :group)} group])
        [:& themes-list {:themes themes
                         :active-theme-ids active-theme-ids
                         :on-close on-close
                         :grouped? true}]])
     [:li {:class (stl/css-case :checked-element true
                                :checked-element-button true)
           :on-click #(modal/show! :tokens/themes {})}
      [:span "Edit themes"]
      [:span {:class (stl/css :icon)} i/arrow]]]))

(mf/defc theme-select
  [{:keys []}]
  (let [;; Store
        temp-theme-id (dm/legacy (mf/deref refs/workspace-temp-theme-id))
        active-theme-ids (dm/legacy (-> (mf/deref refs/workspace-active-theme-paths)
                                        (disj temp-theme-id)))
        active-themes-count (count active-theme-ids)
        themes (mf/deref refs/workspace-token-theme-tree)

        ;; Data
        current-label (cond
                        (> active-themes-count 1) (str active-themes-count " themes active")
                        ;; (pos? active-themes-count) (get-in themes [(first active-theme-ids) :name])
                        :else "No theme active")

        ;; State
        state* (mf/use-state
                {:id (uuid/next)
                 :is-open? false})
        state (deref state*)
        is-open? (:is-open? state)

        ;; Dropdown
        dropdown-element* (mf/use-ref nil)
        on-close-dropdown (mf/use-fn #(swap! state* assoc :is-open? false))
        on-open-dropdown (mf/use-fn #(swap! state* assoc :is-open? true))]
    [:div {:on-click on-open-dropdown
           :class (stl/css :custom-select)}
     [:span {:class (stl/css :current-label)} current-label]
     [:span {:class (stl/css :dropdown-button)} i/arrow]
     [:& dropdown {:show is-open? :on-close on-close-dropdown}
      [:div {:ref dropdown-element*
             :class (stl/css :custom-select-dropdown)}
       [:& theme-options {:on-close on-close-dropdown}]]]]))
