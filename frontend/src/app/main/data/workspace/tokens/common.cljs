(ns app.main.data.workspace.tokens.common)

(defn get-workspace-tokens-lib [state]
  (get-in state [:workspace-data :tokens-lib]))
