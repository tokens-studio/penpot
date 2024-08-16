;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.tokens-theme-list
  (:require
   [app.common.data :as d]
   [app.common.time :as dt]))

(defn- touch
  "Updates the `modified-at` timestamp of a token set."
  [token-set]
  (assoc token-set :modified-at (dt/now)))

(defn add-token-theme
  [file-data {:keys [index id] :as token-theme}]
  (-> file-data
      (update :token-themes
              (fn [token-themes]
                (let [exists? (some (partial = id) token-themes)]
                  (cond
                    exists?      token-themes
                    (nil? index) (conj (or token-themes []) id)
                    :else        (d/insert-at-index token-themes index [id])))))
      (update :token-themes-index assoc id token-theme)))

(defn update-token-theme
  [file-data token-theme-id f & args]
  (d/update-in-when file-data [:token-themes-index token-theme-id] #(-> (apply f % args) (touch))))

(defn delete-token-theme
  [file-data token-id]
  file-data)

(defn add-token-set
  [file-data {:keys [index id] :as token-set}]
  (-> file-data
      (update :token-set-groups
              (fn [token-set-groups]
                (let [exists? (some (partial = id) token-set-groups)]
                  (cond
                    exists?      token-set-groups
                    (nil? index) (conj (or token-set-groups []) id)
                    :else        (d/insert-at-index token-set-groups index [id])))))
      (update :token-sets-index assoc id token-set)))

(defn update-token-set
  [file-data token-set-id f & args]
  (d/update-in-when file-data [:token-sets-index token-set-id] #(-> (apply f % args) (touch))))

(defn delete-token-set
  [file-data token-id]
  file-data)
