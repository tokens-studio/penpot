;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.data.tokens
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.changes-builder :as pcb]
   [app.common.types.shape :as cts]
   [app.common.uuid :as uuid]
   [app.main.data.workspace.changes :as dch]
   [app.main.ui.workspace.tokens.common :refer [workspace-shapes]]
   [beicon.v2.core :as rx]
   [clojure.data :as data]
   [potok.v2.core :as ptk]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO HYMA: Copied over from workspace.cljs
(defn update-shape
  [id attrs]
  (dm/assert!
   "expected valid parameters"
   (and (cts/check-shape-attrs! attrs)
        (uuid? id)))

  (ptk/reify ::update-shape
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (dch/update-shapes [id] #(merge % attrs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOKENS Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn toggle-or-apply-token
  "Remove any shape attributes from token if they exists.
  Othewise apply token attributes."
  [shape token]
  (let [[shape-leftover token-leftover _matching] (data/diff (:applied-tokens shape) token)]
    (merge {} shape-leftover token-leftover)))

(defn get-shape-from-state [shape-id state]
  (let [current-page-id (get state :current-page-id)
        shape (-> (workspace-shapes (:workspace-data state) current-page-id #{shape-id})
                  (first))]
    shape))

(defn token-from-attributes [token-id attributes]
  (->> (map (fn [attr] [attr token-id]) attributes)
       (into {})))

(defn update-token-from-attributes
  [{:keys [token-id shape-id attributes]}]
  (ptk/reify ::update-token-from-attributes
    ptk/WatchEvent
    (watch [_ state _]
      (let [shape (get-shape-from-state shape-id state)
            token (token-from-attributes token-id attributes)
            next-applied-tokens (toggle-or-apply-token shape token)]
        (rx/of (update-shape shape-id {:applied-tokens next-applied-tokens}))))))

(defn add-token
  [token]
  (let [token (update token :id #(or % (uuid/next)))]
    (ptk/reify ::add-token
      ptk/WatchEvent
      (watch [it _ _]
        (let [changes (-> (pcb/empty-changes it)
                          (pcb/add-token token))]
          (rx/of (dch/commit-changes changes)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEMP (Move to test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (def shape-1 {:r3 3})

  (def token-1 {:rx 1
                :ry 1})


  (def shape-after-token-1-is-applied {:rx 1
                                       :ry 1
                                       :r3 3})

  (def token-2 {:r3 1})


  (def shape-after-token-2-is-applied {:rx 1
                                       :ry 1
                                       :r3 1})

  (def token-3 {:r3 1})

  (def shape-after-token-3-is-applied {:rx 1
                                       :ry 1})

  (= (toggle-or-apply-token shape-1 token-1)
     shape-after-token-1-is-applied)
  (= (toggle-or-apply-token shape-after-token-1-is-applied token-2)
     shape-after-token-2-is-applied)
  (= (toggle-or-apply-token shape-after-token-2-is-applied token-3)
     shape-after-token-3-is-applied)
  nil)
