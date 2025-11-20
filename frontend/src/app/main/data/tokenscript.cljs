(ns app.main.data.tokenscript
  (:require
   ["@tokens-studio/tokenscript-interpreter" :refer [TokenResolver BaseSymbolType processTokens]]
   [app.common.logging :as l]
   [app.common.time :as ct]
   [app.main.data.workspace.tokens.errors :as wte]
   [app.main.refs :as refs]
   [clojure.string]
   [cuerdas.core :as str]))

(l/set-level! :debug)

(defn tokenscript-symbol? [v]
  (instance? BaseSymbolType v))

(defn clj->tokenscript
  [o]
  (cond
    (sequential? o) (str/join ", " o)
    (map? o)
    (let [lines (volatile! ["variable output: Dictionary;"])]
      (doseq [[k v] o]
        (let [value-str (if (vector? v)
                          (str/join ", " v)
                          v)
              line (str "output.set(" "\"" (name k) "\"" ", " value-str ");")]
          (vswap! lines conj line)))
      (vswap! lines conj "return output;")
      (clojure.string/join "\n" @lines))))

(defn token-set->token-set-map [tokens]
  (let [token-map (js/Map.)]
    (doseq [[k v] tokens]
      (let [{:keys [value]} v
            value (if (or (sequential? value) (map? value))
                    (clj->tokenscript value)
                    value)]
        (.set token-map k value)))
    token-map))

(defn tokenscript->penpot-token [token tokenscript-symbol]
  (js/console.log "tokenscript-symbol)" tokenscript-symbol)
  (assoc token :resolved-value tokenscript-symbol))

(defn create-token-builder
  "Creates a builder class for processing tokens."
  [tokens]
  (let [output (volatile! tokens)]
    #js {:name "penpot-tokens"
         :onResolve
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
  (token-set->token-set-map @refs/workspace-all-tokens-in-selected-set)
  (do
    (defonce a (atom nil))
    (-> (build @refs/workspace-all-tokens-in-selected-set)
        (doto js/console.log)))


  (get @a "typography")


  nil)
