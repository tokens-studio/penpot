(ns app.main.data.tokenscript
  (:require
   ["@tokens-studio/tokenscript-interpreter" :refer [BaseSymbolType
                                                     processTokens TokenSymbol]]
   [app.common.logging :as l]
   [app.common.time :as ct]
   [app.main.data.workspace.tokens.errors :as wte]
   [app.main.refs :as refs]
   [clojure.string]
   [okulary.util :as ou]))

(l/set-level! :debug)

;; Helpers ---------------------------------------------------------------------

(defn tokenscript-symbol? [v]
  (instance? BaseSymbolType v))

(defn structured-token? [v]
  (instance? TokenSymbol v))

;; Builders --------------------------------------------------------------------

(defn token-set->token-set-map [tokens]
  (let [token-map (js/Map.)]
    (doseq [[k v] tokens]
      (let [{:keys [value type]} v
            value (if (or (sequential? value) (map? value))
                    #js {"$value" (clj->js value)
                         "$type" (name type)}
                    value)]
        (js/console.log "value" value)
        (.set token-map k value)))
    token-map))

(defn create-token-builder
  "Collects resolved tokens during build time into a clojure structure.
   Returns Tokenscript Symbols in `:resolved-value` key."
  [tokens]
  (let [output (volatile! tokens)]
    #js {:onResolve
         (fn [^js/String token-name ^js/Symbol resolved-symbol]
           (vswap! output assoc-in [token-name :resolved-value] resolved-symbol))
         :onError
         (fn [^js/String token-name ^js/Error _error ^js/String _original-value]
           (let [value (get tokens token-name)
                 default-error [(wte/error-with-value :error.style-dictionary/invalid-token-value value)]]
             (vswap! output assoc-in [token-name :errors] default-error)))
         :getResult
         (fn []
           @output)}))

(defn build
  "Builds tokens in `tokens-set` using tokenscript-interpreter."
  [tokens]
  (let [input (token-set->token-set-map tokens)
        result (processTokens input #js {:builder (create-token-builder tokens)})]
    result))

;; Main ------------------------------------------------------------------------

(defn resolve-tokens [tokens]
  (let [tpoint (ct/tpoint-ms)
        result (build tokens)
        elapsed (tpoint)]
    (l/dbg :hint "tokenscript/resolve-tokens" :elapsed elapsed)
    (.-output result)))

(comment
  (-> (token-set->token-set-map @refs/workspace-all-tokens-in-selected-set)
      (.get "foo"))
  (do
    (defonce a (atom nil))
    (-> (build @refs/workspace-all-tokens-in-selected-set)
        (doto js/console.log)))

  (let [entries (-> (build @refs/workspace-all-tokens-in-selected-set)
                    (.-output)
                    (get "foo")
                    :resolved-value
                    .-value
                    .entries)]
    (ou/doiter entries (fn [n] (aget n 1))))


  (get @a "typography")


  nil)
