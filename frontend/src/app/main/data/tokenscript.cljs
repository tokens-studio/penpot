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

(defn tokenscript-symbol? [v]
  (instance? BaseSymbolType v))

(defn structured-token? [v]
  (instance? TokenSymbol v))

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

(defn tokenscript->penpot-token [token tokenscript-symbol]
  (assoc token :resolved-value tokenscript-symbol))

(defn create-token-builder
  "Creates a builder class for processing tokens."
  [tokens]
  (let [output (volatile! tokens)]
    #js {:onResolve
         (fn [^js/string token-name ^js/Symbol resolved-value]
           (vswap! output update token-name tokenscript->penpot-token resolved-value))
         :onError
         (fn [^js/string token-name ^js/Error error ^js/string _original-value]
           (let [value (get tokens token-name)]
             (vswap! output assoc-in [token-name :errors] [(wte/error-with-value :error.style-dictionary/invalid-token-value value)])))
         :getResult
         (fn []
           @output)}))

(defn build
  "Builds tokens in `tokens-set` using tokenscript-interpreter."
  [tokens]
  (let [input (token-set->token-set-map tokens)
        result (processTokens input #js {:builder (create-token-builder tokens)})]
    (js/console.log "result" result)
    result))

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
