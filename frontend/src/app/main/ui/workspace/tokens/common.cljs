;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.tokens.common
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.changes-builder :as pcb]
   [app.common.geom.point :as gpt]
   [app.main.data.workspace.changes :as dch]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]
   [rumext.v2 :as mf]))

;; Helpers ---------------------------------------------------------------------

(defn workspace-shapes [workspace page-id shape-ids]
  (-> (get-in workspace [:pages-index page-id :objects])
      (keep shape-ids)))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

;; Components ------------------------------------------------------------------

(mf/defc input
  {::mf/wrap-props false}
  [{:keys [type placeholder]
    :or {type "text"}}]
  [:input {:type type
           :class (stl/css :input)
           :placeholder placeholder}])

(mf/defc labeled-input
  {::mf/wrap-props false}
  [{:keys [input-ref label default-value on-change auto-focus?]}]
  [:label {:class (stl/css :labeled-input)}
   [:span {:class (stl/css :label)} label]
   [:input {:ref input-ref
            :default-value default-value
            :autoFocus auto-focus?
            :on-change on-change}]])

;; Token Context Menu Functions -------------------------------------------------

(defn show-token-context-menu
  [{:keys [position token-id] :as params}]
  (dm/assert! (gpt/point? position))
  (ptk/reify ::show-token-context-menu
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-local :token-context-menu] params))))

(def hide-token-context-menu
  (ptk/reify ::hide-token-context-menu
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-local :token-context-menu] nil))))

(defn delete-token
  [id]
  (dm/assert! (uuid? id))
  (ptk/reify ::delete-token
    ptk/WatchEvent
    (watch [it state _]
      (let [data    (get state :workspace-data)
            changes (-> (pcb/empty-changes it)
                        (pcb/with-library-data data)
                        (pcb/delete-token id))]
        (rx/of (dch/commit-changes changes))))))