;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.ds.foundations.assets.icon
  (:refer-clojure :exclude [mask drop filter remove])
  (:require-macros
   [app.common.data.macros :as dm]
   [app.main.style :as stl]
   [app.main.ui.ds.foundations.assets.icon :refer [collect-icons]])
  (:require
   [rumext.v2 :as mf]))

(def ^:icon-id absolute "absolute")
(def ^:icon-id add "add")
(def ^:icon-id align-bottom "align-bottom")
(def ^:icon-id align-content-column-around "align-content-column-around")
(def ^:icon-id align-content-column-between "align-content-column-between")
(def ^:icon-id align-content-column-center "align-content-column-center")
(def ^:icon-id align-content-column-end "align-content-column-end")
(def ^:icon-id align-content-column-evenly "align-content-column-evenly")
(def ^:icon-id align-content-column-start "align-content-column-start")
(def ^:icon-id align-content-column-stretch "align-content-column-stretch")
(def ^:icon-id align-content-row-around "align-content-row-around")
(def ^:icon-id align-content-row-between "align-content-row-between")
(def ^:icon-id align-content-row-center "align-content-row-center")
(def ^:icon-id align-content-row-end "align-content-row-end")
(def ^:icon-id align-content-row-evenly "align-content-row-evenly")
(def ^:icon-id align-content-row-start "align-content-row-start")
(def ^:icon-id align-content-row-stretch "align-content-row-stretch")
(def ^:icon-id align-horizontal-center "align-horizontal-center")
(def ^:icon-id align-items-column-center "align-items-column-center")
(def ^:icon-id align-items-column-end "align-items-column-end")
(def ^:icon-id align-items-column-start "align-items-column-start")
(def ^:icon-id align-items-row-center "align-items-row-center")
(def ^:icon-id align-items-row-end "align-items-row-end")
(def ^:icon-id align-items-row-start "align-items-row-start")
(def ^:icon-id align-left "align-left")
(def ^:icon-id align-right "align-right")
(def ^:icon-id align-self-column-bottom "align-self-column-bottom")
(def ^:icon-id align-self-column-center "align-self-column-center")
(def ^:icon-id align-self-column-stretch "align-self-column-stretch")
(def ^:icon-id align-self-column-top "align-self-column-top")
(def ^:icon-id align-self-row-center "align-self-row-center")
(def ^:icon-id align-self-row-left "align-self-row-left")
(def ^:icon-id align-self-row-right "align-self-row-right")
(def ^:icon-id align-self-row-stretch "align-self-row-stretch")
(def ^:icon-id align-top "align-top")
(def ^:icon-id align-vertical-center "align-vertical-center")
(def ^:icon-id arrow "arrow")
(def ^:icon-id arrow-up "arrow-up")
(def ^:icon-id arrow-down "arrow-down")
(def ^:icon-id arrow-left "arrow-left")
(def ^:icon-id arrow-right "arrow-right")
(def ^:icon-id asc-sort "asc-sort")
(def ^:icon-id board "board")
(def ^:icon-id boards-thumbnail "boards-thumbnail")
(def ^:icon-id boolean-difference "boolean-difference")
(def ^:icon-id boolean-exclude "boolean-exclude")
(def ^:icon-id boolean-flatten "boolean-flatten")
(def ^:icon-id boolean-intersection "boolean-intersection")
(def ^:icon-id boolean-union "boolean-union")
(def ^:icon-id bug "bug")
(def ^:icon-id character-a "character-a")
(def ^:icon-id character-b "character-b")
(def ^:icon-id character-c "character-c")
(def ^:icon-id character-d "character-d")
(def ^:icon-id character-e "character-e")
(def ^:icon-id character-f "character-f")
(def ^:icon-id character-g "character-g")
(def ^:icon-id character-h "character-h")
(def ^:icon-id character-i "character-i")
(def ^:icon-id character-j "character-j")
(def ^:icon-id character-k "character-k")
(def ^:icon-id character-l "character-l")
(def ^:icon-id character-m "character-m")
(def ^:icon-id character-n "character-n")
(def ^:icon-id character-ntilde "character-ntilde")
(def ^:icon-id character-o "character-o")
(def ^:icon-id character-p "character-p")
(def ^:icon-id character-q "character-q")
(def ^:icon-id character-r "character-r")
(def ^:icon-id character-s "character-s")
(def ^:icon-id character-t "character-t")
(def ^:icon-id character-u "character-u")
(def ^:icon-id character-v "character-v")
(def ^:icon-id character-w "character-w")
(def ^:icon-id character-x "character-x")
(def ^:icon-id character-y "character-y")
(def ^:icon-id character-z "character-z")
(def ^:icon-id clip-content "clip-content")
(def ^:icon-id clipboard "clipboard")
(def ^:icon-id close-small "close-small")
(def ^:icon-id close "close")
(def ^:icon-id code "code")
(def ^:icon-id column-reverse "column-reverse")
(def ^:icon-id column "column")
(def ^:icon-id comments "comments")
(def ^:icon-id component-copy "component-copy")
(def ^:icon-id component "component")
(def ^:icon-id constraint-horizontal "constraint-horizontal")
(def ^:icon-id constraint-vertical "constraint-vertical")
(def ^:icon-id corner-bottom-left "corner-bottom-left")
(def ^:icon-id corner-bottom-right "corner-bottom-right")
(def ^:icon-id corner-bottom "corner-bottom")
(def ^:icon-id corner-center "corner-center")
(def ^:icon-id corner-radius "corner-radius")
(def ^:icon-id corner-top "corner-top")
(def ^:icon-id corner-top-left "corner-top-left")
(def ^:icon-id corner-top-right "corner-top-right")
(def ^:icon-id curve "curve")
(def ^:icon-id delete-text "delete-text")
(def ^:icon-id delete "delete")
(def ^:icon-id desc-sort "desc-sort")
(def ^:icon-id detach "detach")
(def ^:icon-id detached "detached")
(def ^:icon-id distribute-horizontally "distribute-horizontally")
(def ^:icon-id distribute-vertical-spacing "distribute-vertical-spacing")
(def ^:icon-id document "document")
(def ^:icon-id download "download")
(def ^:icon-id drop "drop")
(def ^:icon-id easing-ease-in-out "easing-ease-in-out")
(def ^:icon-id easing-ease-in "easing-ease-in")
(def ^:icon-id easing-ease-out "easing-ease-out")
(def ^:icon-id easing-ease "easing-ease")
(def ^:icon-id easing-linear "easing-linear")
(def ^:icon-id effects "effects")
(def ^:icon-id elipse "elipse")
(def ^:icon-id exit "exit")
(def ^:icon-id expand "expand")
(def ^:icon-id feedback "feedback")
(def ^:icon-id fill-content "fill-content")
(def ^:icon-id filter "filter")
(def ^:icon-id fixed-width "fixed-width")
(def ^:icon-id flex-grid "flex-grid")
(def ^:icon-id flex-horizontal "flex-horizontal")
(def ^:icon-id flex-vertical "flex-vertical")
(def ^:icon-id flex "flex")
(def ^:icon-id flip-horizontal "flip-horizontal")
(def ^:icon-id flip-vertical "flip-vertical")
(def ^:icon-id gap-horizontal "gap-horizontal")
(def ^:icon-id gap-vertical "gap-vertical")
(def ^:icon-id graphics "graphics")
(def ^:icon-id grid-column "grid-column")
(def ^:icon-id grid-columns "grid-columns")
(def ^:icon-id grid-gutter "grid-gutter")
(def ^:icon-id grid-margin "grid-margin")
(def ^:icon-id grid "grid")
(def ^:icon-id grid-row "grid-row")
(def ^:icon-id grid-rows "grid-rows")
(def ^:icon-id grid-square "grid-square")
(def ^:icon-id group "group")
(def ^:icon-id gutter-horizontal "gutter-horizontal")
(def ^:icon-id gutter-vertical "gutter-vertical")
(def ^:icon-id help "help")
(def ^:icon-id hide "hide")
(def ^:icon-id history "history")
(def ^:icon-id hsva "hsva")
(def ^:icon-id hug-content "hug-content")
(def ^:icon-id icon "icon")
(def ^:icon-id img "img")
(def ^:icon-id info "info")
(def ^:icon-id interaction "interaction")
(def ^:icon-id join-nodes "join-nodes")
(def ^:icon-id external-link "external-link")
(def ^:icon-id justify-content-column-around "justify-content-column-around")
(def ^:icon-id justify-content-column-between "justify-content-column-between")
(def ^:icon-id justify-content-column-center "justify-content-column-center")
(def ^:icon-id justify-content-column-end "justify-content-column-end")
(def ^:icon-id justify-content-column-evenly "justify-content-column-evenly")
(def ^:icon-id justify-content-column-start "justify-content-column-start")
(def ^:icon-id justify-content-row-around "justify-content-row-around")
(def ^:icon-id justify-content-row-between "justify-content-row-between")
(def ^:icon-id justify-content-row-center "justify-content-row-center")
(def ^:icon-id justify-content-row-end "justify-content-row-end")
(def ^:icon-id justify-content-row-evenly "justify-content-row-evenly")
(def ^:icon-id justify-content-row-start "justify-content-row-start")
(def ^:icon-id layers "layers")
(def ^:icon-id library "library")
(def ^:icon-id locate "locate")
(def ^:icon-id lock "lock")
(def ^:icon-id margin "margin")
(def ^:icon-id margin-bottom "margin-bottom")
(def ^:icon-id margin-left "margin-left")
(def ^:icon-id margin-left-right "margin-left-right")
(def ^:icon-id margin-right "margin-right")
(def ^:icon-id margin-top "margin-top")
(def ^:icon-id margin-top-bottom "margin-top-bottom")
(def ^:icon-id mask "mask")
(def ^:icon-id masked "masked")
(def ^:icon-id menu "menu")
(def ^:icon-id merge-nodes "merge-nodes")
(def ^:icon-id move "move")
(def ^:icon-id msg-error "msg-error")
(def ^:icon-id msg-neutral "msg-neutral")
(def ^:icon-id msg-success "msg-success")
(def ^:icon-id msg-warning "msg-warning")
(def ^:icon-id open-link "open-link")
(def ^:icon-id padding-bottom "padding-bottom")
(def ^:icon-id padding-extended "padding-extended")
(def ^:icon-id padding-left "padding-left")
(def ^:icon-id padding-left-right "padding-left-right")
(def ^:icon-id padding-right "padding-right")
(def ^:icon-id padding-top "padding-top")
(def ^:icon-id padding-top-bottom "padding-top-bottom")
(def ^:icon-id path "path")
(def ^:icon-id pentool "pentool")
(def ^:icon-id percentage "percentage")
(def ^:icon-id picker "picker")
(def ^:icon-id pin "pin")
(def ^:icon-id play "play")
(def ^:icon-id puzzle "puzzle")
(def ^:icon-id rectangle "rectangle")
(def ^:icon-id reload "reload")
(def ^:icon-id remove "remove")
(def ^:icon-id rgba "rgba")
(def ^:icon-id rgba-complementary "rgba-complementary")
(def ^:icon-id rotation "rotation")
(def ^:icon-id row "row")
(def ^:icon-id row-reverse "row-reverse")
(def ^:icon-id search "search")
(def ^:icon-id separate-nodes "separate-nodes")
(def ^:icon-id shown "shown")
(def ^:icon-id size-horizontal "size-horizontal")
(def ^:icon-id size-vertical "size-vertical")
(def ^:icon-id snap-nodes "snap-nodes")
(def ^:icon-id status-alert "status-alert")
(def ^:icon-id status-tick "status-tick")
(def ^:icon-id status-update "status-update")
(def ^:icon-id status-wrong "status-wrong")
(def ^:icon-id stroke-arrow "stroke-arrow")
(def ^:icon-id stroke-circle "stroke-circle")
(def ^:icon-id stroke-diamond "stroke-diamond")
(def ^:icon-id stroke-rectangle "stroke-rectangle")
(def ^:icon-id stroke-rounded "stroke-rounded")
(def ^:icon-id stroke-size "stroke-size")
(def ^:icon-id stroke-squared "stroke-squared")
(def ^:icon-id stroke-triangle "stroke-triangle")
(def ^:icon-id svg "svg")
(def ^:icon-id swatches "swatches")
(def ^:icon-id switch "switch")
(def ^:icon-id text "text")
(def ^:icon-id text-align-center "text-align-center")
(def ^:icon-id text-align-left "text-align-left")
(def ^:icon-id text-align-right "text-align-right")
(def ^:icon-id text-auto-height "text-auto-height")
(def ^:icon-id text-auto-width "text-auto-width")
(def ^:icon-id text-bottom "text-bottom")
(def ^:icon-id text-fixed "text-fixed")
(def ^:icon-id text-justify "text-justify")
(def ^:icon-id text-letterspacing "text-letterspacing")
(def ^:icon-id text-lineheight "text-lineheight")
(def ^:icon-id text-lowercase "text-lowercase")
(def ^:icon-id text-ltr "text-ltr")
(def ^:icon-id text-middle "text-middle")
(def ^:icon-id text-mixed "text-mixed")
(def ^:icon-id text-palette "text-palette")
(def ^:icon-id text-paragraph "text-paragraph")
(def ^:icon-id text-rtl "text-rtl")
(def ^:icon-id text-stroked "text-stroked")
(def ^:icon-id text-top "text-top")
(def ^:icon-id text-underlined "text-underlined")
(def ^:icon-id text-uppercase "text-uppercase")
(def ^:icon-id thumbnail "thumbnail")
(def ^:icon-id tick "tick")
(def ^:icon-id to-corner "to-corner")
(def ^:icon-id to-curve "to-curve")
(def ^:icon-id tree "tree")
(def ^:icon-id unlock "unlock")
(def ^:icon-id user "user")
(def ^:icon-id vertical-align-items-center "vertical-align-items-center")
(def ^:icon-id vertical-align-items-end "vertical-align-items-end")
(def ^:icon-id vertical-align-items-start "vertical-align-items-start")
(def ^:icon-id view-as-icons "view-as-icons")
(def ^:icon-id view-as-list "view-as-list")
(def ^:icon-id wrap "wrap")

(def icon-list "A collection of all icons" (collect-icons))

(def ^:private icon-size-m 16)
(def ^:private icon-size-s 12)

(def ^:private schema:icon
  [:map
   [:class {:optional true} :string]
   [:id [:and :string [:fn #(contains? icon-list %)]]]
   [:size  {:optional true}
    [:maybe [:enum "s" "m"]]]])

(mf/defc icon*
  {::mf/props :obj
   ::mf/schema schema:icon}
  [{:keys [id size class] :rest props}]
  (let [class (dm/str (or class "") " " (stl/css :icon))
        props (mf/spread-props props {:class class :width icon-size-m :height icon-size-m})
        size-px (cond (= size "s") icon-size-s :else icon-size-m)
        offset (/ (- icon-size-m size-px) 2)]
    [:> "svg" props
     [:use {:href (dm/str "#icon-" id) :width size-px :height size-px :x offset :y offset}]]))
