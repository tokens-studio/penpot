;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.tokens.form
  (:require-macros [app.main.style :as stl])
  (:require
   ["lodash.debounce" :as debounce]
   [app.main.ui.workspace.tokens.common :as tokens.common]
   [app.main.ui.workspace.tokens.style-dictionary :as sd]
   [app.util.dom :as dom]
   [cuerdas.core :as str]
   [malli.core :as m]
   [malli.error :as me]
   [promesa.core :as p]
   [rumext.v2 :as mf]))

;; Schemas ---------------------------------------------------------------------

(defn token-name-schema
  "Generate a dynamic schema validation to check if a token name already exists.
  `existing-token-names` should be a set of strings."
  [existing-token-names]
  (let [non-existing-token-schema
        (m/-simple-schema
         {:type :token/name-exists
          :pred #(not (get existing-token-names %))
          :type-properties {:error/fn #(str (:value %) " is an already existing token name")
                            :existing-token-names existing-token-names}})]
    (m/schema
     [:and
      [:string {:min 1 :max 255}]
      non-existing-token-schema])))

;; Helpers ---------------------------------------------------------------------

(defn finalize-name [name]
  (str/trim name))

(defn finalize-value [name]
  (str/trim name))

;; Component -------------------------------------------------------------------

(defn use-debonced-resolve-callback
  [name-ref token tokens callback & {:keys [cached timeout]
                                     :or {cached {}
                                          timeout 160}}]
  (let [timeout-id-ref (mf/use-ref nil)
        cache (mf/use-ref cached)
        debounced-resolver-callback
        (mf/use-callback
         (mf/deps token callback tokens)
         (fn [event]
           (let [input (dom/get-target-val event)
                 timeout-id (js/Symbol)
                 ;; Dont execute callback when the timout-id-ref is outdated because this function got called again
                 timeout-outdated-cb? #(not= (mf/ref-val timeout-id-ref) timeout-id)]
             (mf/set-ref-val! timeout-id-ref timeout-id)
             (js/setTimeout
              (fn []
                (when (not (timeout-outdated-cb?))
                  (if-let [cached (get (mf/ref-val cache) tokens)]
                    (callback cached)
                    (let [token-references (sd/find-token-references input)
                          ;; When creating a new token we dont have a token name yet,
                          ;; so we use a temporary token name that hopefully doesn't clash with any of the users token names.
                          token-name (if (empty? @name-ref) "__TOKEN_STUDIO_SYSTEM.TEMP" @name-ref)
                          direct-self-reference? (get token-references token-name)
                          empty-input? (empty? (str/trim input))]
                      (cond
                        empty-input? (callback nil)
                        direct-self-reference? (callback :error/token-direct-self-reference)
                        :else
                        (let [token-id (or (:id token) (random-uuid))
                              new-tokens (update tokens token-id merge {:id token-id
                                                                        :value input
                                                                        :name token-name})]
                          (-> (sd/resolve-tokens+ new-tokens)
                              (p/finally
                                (fn [resolved-tokens _err]
                                  (when-not (timeout-outdated-cb?)
                                    (let [{:keys [errors resolved-value] :as resolved-token} (get resolved-tokens token-id)]
                                      (cond
                                        resolved-value (do
                                                         (mf/set-ref-val! cache (assoc (mf/ref-val cache) input resolved-tokens))
                                                         (callback resolved-token))
                                        (= #{:style-dictionary/missing-reference} errors) (callback :error/token-missing-reference)
                                        :else (callback :error/unknown-error)))))))))))))

              timeout))))]
    debounced-resolver-callback))

(mf/defc form
  {::mf/wrap-props false}
  [{:keys [token] :as _args}]
  (let [tokens (sd/use-resolved-workspace-tokens)
        existing-token-names (mf/use-memo
                              (mf/deps tokens)
                              (fn []
                                (-> (into #{} (map (fn [[_ {:keys [name]}]] name) tokens))
                                     ;; Allow setting token to already used name
                                    (disj (:name token)))))

        ;; State
        state* (mf/use-state (merge {:name ""
                                     :value ""
                                     :description ""}
                                    token))
        state @state*

        form-touched (mf/use-state nil)
        update-form-touched (mf/use-callback
                             (debounce #(reset! form-touched (js/Symbol)) 120))

        ;; Name
        name (mf/use-var (or (:name token) ""))
        name-errors (mf/use-state nil)
        name-schema (mf/use-memo
                     (mf/deps existing-token-names)
                     #(token-name-schema existing-token-names))
        on-update-name (mf/use-callback
                        (debounce (fn [e]
                                    (let [value (dom/get-target-val e)
                                          errors (->> (finalize-name value)
                                                      (m/explain name-schema))]
                                      (mf/set-ref-val! name value)
                                      (reset! name-errors errors)
                                      (update-form-touched)))))

        ;; Value
        value (mf/use-var (or (:value token) ""))
        token-resolve-result (mf/use-state (get-in tokens [(:id token) :resolved-value]))
        set-resolve-value (mf/use-callback
                           (fn [token-or-err]
                             (let [value (cond
                                           (= token-or-err :error/token-direct-self-reference) :error/token-self-reference
                                           (= token-or-err :error/token-missing-reference) :error/token-missing-reference
                                           (:resolved-value token-or-err) (:resolved-value token-or-err))]
                               (reset! token-resolve-result value))))
        on-update-value (use-debonced-resolve-callback name token tokens set-resolve-value)
        value-error? (when (keyword? @token-resolve-result)
                       (= (namespace @token-resolve-result) "error"))

        disabled? (or
                   @name-errors
                   value-error?
                   (empty? (finalize-name (mf/ref-val name))))]

        ;; on-submit (fn [e]
        ;;             (dom/prevent-default e)
        ;;             (let [token-value (-> (fields->map state)
        ;;                                   (first)
        ;;                                   (val))
        ;;                   token (cond-> {:name (:name state)
        ;;                                  :type (or (:type token) token-type)
        ;;                                  :value token-value
        ;;                                  :description (:description state)}
        ;;                           (:id token) (assoc :id (:id token)))]
        ;;               (st/emit! (dt/add-token token))
        ;;               (modal/hide!)))]
    [:form
     {#_#_:on-submit on-submit}
     [:div {:class (stl/css :token-rows)}
      [:div
       [:& tokens.common/labeled-input {:label "Name"
                                        :error? @name-errors
                                        :input-props {:default-value @name
                                                      :auto-focus true
                                                      :on-change on-update-name}}]
       (when @name-errors
         [:p {:class (stl/css :error)}
          (me/humanize @name-errors)])]
      [:& tokens.common/labeled-input {:label "Value"
                                       :input-props {:default-value @value
                                                     :on-change on-update-value}}]
      [:div {:class (stl/css-case :resolved-value true
                                  :resolved-value-placeholder (nil? @token-resolve-result)
                                  :resolved-value-error value-error?)}
       (case @token-resolve-result
         :error/token-self-reference "Token has self reference"
         :error/token-missing-reference "Token has missing reference"
         :error/unknown-error ""
         nil "Enter token value"
         [:p @token-resolve-result])]
      [:& tokens.common/labeled-input {:label "Description"
                                       :input-props {:default-value (:description state)
                                                     #_#_:on-change #(on-update-description %)}}]
      [:div {:class (stl/css :button-row)}
       [:button {:class (stl/css :button)
                 :type "submit"
                 :disabled disabled?}
        "Save"]]]]))
