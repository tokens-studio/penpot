(ns app.main.data.tokenscript
  (:require
   ["@tokens-studio/tokenscript-interpreter" :refer [jsValueToSymbolType
                                                     TokenResolver]]
   [app.main.refs :as refs]
   [clojure.string]
   [cuerdas.core :as str]))

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

(-> (clj->tokenscript (get-in @refs/workspace-all-tokens-in-selected-set ["typography" :value]))
    js/console.log)


(defn token-set->token-set-map [tokens]
  (let [token-map (js/Map.)]
    (doseq [[k v] tokens]
      (let [{:keys [value]} v
            value (if (or (sequential? value) (map? value))
                    (clj->tokenscript value)
                    value)]
        (.set token-map k value)))
    token-map))

(defn build
  "Builds tokens in `tokens-set` using tokenscript-interpreter."
  [tokens]
  (let [resolver (TokenResolver.)
        input (token-set->token-set-map tokens)
        output (volatile! tokens)]
    (.processTokens resolver input
                    #js {:onResolve
                         (fn [^js/string token-name ^js/Symbol resolved-value]
                           (vswap! output assoc token-name resolved-value))
                         :onError
                         (fn [^js/string token-name ^js/Error error]

                           (js/console.error error)
                           (js/console.error "Token resolve error" (str "\"" token-name "\"") error))})
    @output))

(comment
  (token-set->token-set-map @refs/workspace-all-tokens-in-selected-set)
  (do
    (defonce a (atom nil))
    (-> (build @refs/workspace-all-tokens-in-selected-set)
        (doto js/console.log)))


  (get @a "typography")


  nil)
