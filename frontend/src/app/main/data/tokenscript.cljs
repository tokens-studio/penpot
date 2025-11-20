(ns app.main.data.tokenscript
  (:require
   ["@tokens-studio/tokenscript-interpreter" :refer [BaseSymbolType ListSymbol
                                                     NumberWithUnitSymbol
                                                     processTokens TokenSymbol]]
   [app.common.logging :as l]
   [app.common.time :as ct]
   [app.main.data.workspace.tokens.errors :as wte]
   [app.main.refs :as refs]
   [clojure.string]))

(l/set-level! :debug)

;; Helpers ---------------------------------------------------------------------

(defn tokenscript-symbol? [v]
  (instance? BaseSymbolType v))

(defn structured-token? [v]
  (instance? TokenSymbol v))

(defn number-with-unit-symbol? [v]
  (instance? NumberWithUnitSymbol v))

(defn list-symbol? [v]
  (instance? ListSymbol v))

(defn rem-number-with-unit? [v]
  (and (number-with-unit-symbol? v)
       (= (.-unit v) "rem")))

(defn rem->px [^js v]
  (* (.-value v) 16))

(defn tokenscript-symbols->penpot-unit [^js v]
  (cond
    (list-symbol? v) (tokenscript-symbols->penpot-unit (.nth 1 v))
    (rem-number-with-unit? v) (rem->px v)
    :else (.-value v)))

;; Processors ------------------------------------------------------------------

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

(defn clj-tokens->tokenscript-tokens
  "Convert clojure map into tokenscript map structure."
  [tokens]
  (let [token-map (js/Map.)]
    (doseq [[k {:keys [value type]}] tokens]
      (.set token-map k #js {"$type" type "$value" (clj->js value)}))
    token-map))

(defn process-tokens
  "Builds tokens in `tokens-set` using tokenscript-interpreter."
  [tokens]
  (let [input (clj-tokens->tokenscript-tokens tokens)
        result (processTokens input #js {:builder (create-token-builder tokens)})]
    result))

;; Main ------------------------------------------------------------------------

(defn resolve-tokens [tokens]
  (let [tpoint (ct/tpoint-ms)
        result (process-tokens tokens)
        elapsed (tpoint)]
    (l/dbg :hint "tokenscript/resolve-tokens" :elapsed elapsed)
    (.-output result)))

(comment
  (.-output (process-tokens @refs/workspace-all-tokens-in-selected-set))
  nil)
