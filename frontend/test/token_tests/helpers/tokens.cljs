(ns token-tests.helpers.tokens
  (:require
   [app.common.test-helpers.ids-map :as thi]
   [app.main.ui.workspace.tokens.token :as wtt]))

(defn add-token [state label params]
  (let [id (thi/new-id! label)
        token (assoc params :id id)]
    (update-in state [:data :tokens] assoc id token)))

(defn get-token [file label]
  (let [id (thi/id label)]
    (get-in file [:data :tokens id])))

(defn apply-token-to-shape [file shape-label token-label attributes]
  (let [first-page-id (get-in file [:data :pages 0])
        shape-id (thi/id shape-label)
        token-id (thi/id token-label)
        applied-attributes (wtt/attributes-map attributes token-id)]
    (update-in file [:data
                     :pages-index first-page-id
                     :objects shape-id
                     :applied-tokens]
               merge applied-attributes)))
