;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.tokens.common
  (:require-macros [app.main.style :as stl])
  (:require
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

(mf/defc labeled-input
  {::mf/wrap-props false}
  [{:keys [input-ref label default-value on-change auto-focus? auto-complete?]}]
  (let [input-props (cond-> {:ref input-ref
                             :default-value default-value
                             :autoFocus auto-focus?
                             :on-change on-change}
                      ;; Disable auto-complete on form fields for proprietary password managers
                      ;; https://github.com/orgs/tokens-studio/projects/69/views/11?pane=issue&itemId=63724204
                      (not auto-complete?) (assoc :data-1p-ignore true
                                                  :data-lpignore true
                                                  :autoComplete "off"))]
    [:label {:class (stl/css :labeled-input)}
     [:span {:class (stl/css :label)} label]
     [:& :input input-props]]))
