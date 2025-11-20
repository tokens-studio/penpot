(ns app.main.data.workspace.tokens.format
  (:require
   [app.main.data.tokenscript :as ts]
   [cuerdas.core :as str]))

(def category-dictionary
  {:stroke-width "Stroke Width"
   :spacing "Spacing"
   :sizing "Sizing"
   :border-radius "Border Radius"
   :x "X"
   :y "Y"
   :font-size "Font Size"
   :font-family "Font Family"
   :font-weight "Font Weight"
   :line-height "Line Height"
   :letter-spacing "Letter Spacing"
   :text-case "Text Case"
   :text-decoration "Text Decoration"
   :offsetX "X"
   :offsetY "Y"
   :blur "Blur"
   :spread "Spread"
   :color "Color"
   :inset "Inner Shadow"})

(declare format-token-value)

(defn- format-map-entries
  "Formats a sequence of [k v] entries into a formatted string."
  [entries]
  (->> entries
       (map (fn [[k v]]
              (str "- " (category-dictionary (keyword k)) ": " (format-token-value v))))
       (str/join "\n")
       (str "\n")))

(defn format-token-value
  "Converts token value of any shape to a string."
  [token-value]
  (cond
    (ts/rem-number-with-unit? token-value)
    (str (ts/rem->px token-value) "px")

    (ts/color-symbol? token-value) (.to token-value "hex")

    (ts/tokenscript-symbol? token-value) (.toString token-value)

    (instance? js/Map token-value)
    (format-map-entries (es6-iterator-seq (.entries token-value)))

    (map? token-value)
    (format-map-entries token-value)

    (and (sequential? token-value) (every? map? token-value))
    (str/join "\n" (map format-token-value token-value))

    (sequential? token-value)
    (str/join ", " token-value)

    :else
    (str token-value)))
