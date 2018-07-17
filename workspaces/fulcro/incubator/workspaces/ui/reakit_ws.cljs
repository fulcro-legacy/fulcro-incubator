(ns fulcro.incubator.workspaces.ui.reakit-ws
  (:require [nubank.workspaces.core :as ws]
            [nubank.workspaces.model :as wsm]
            [nubank.workspaces.card-types.react :as ct.react]
            [fulcro.incubator.ui.core :refer [js-spread]]
            [fulcro.incubator.ui.reakit :as rk]
            [fulcro.incubator.ui.icons.font-awesome :as fa]))

(ws/defcard reakit-base
  (ct.react/react-card
    (rk/block
      (rk/base "Base")
      (rk/base {:backgroundColor "palevioletred" :color "white"} "Base"))))

(ws/defcard reakit-block
  {::wsm/card-width 2 ::wsm/card-height 11}
  (ct.react/react-card
    (rk/block
      (rk/block {:width "100px" :height "100px" :backgroundColor "rgb(219, 112, 147)"})
      (rk/block {:width "100px" :height "100px" :backgroundColor "rgb(219, 112, 198)"})
      (rk/block {:width "100px" :height "100px" :backgroundColor "rgb(205, 112, 219)"}))))

(ws/defcard reakit-box
  {::wsm/card-width 1 ::wsm/card-height 3}
  (ct.react/react-card
    (rk/box "Box")))

(ws/defcard reakit-flex
  {::wsm/card-width 5 ::wsm/card-height 5}
  (ct.react/react-card
    (rk/flex
      (rk/paragraph {:marginRight "1em"}
        "Aliqua tempor adipisicing dolor Lorem ut aliqua nostrud esse. Ex esse sunt\n    irure aliqua dolor labore. Ad nostrud esse qui duis dolore in aliquip. Esse\n    velit laborum magna duis ad magna commodo qui laboris in duis incididunt\n    laboris.")
      (rk/paragraph "Deserunt occaecat consectetur id aliquip aliqua mollit ipsum laborum in\n    fugiat dolor reprehenderit."))))

(ws/defcard reakit-button
  (ct.react/react-card
    (rk/grid {:column true :gap "1em"}
      (rk/button "Button")
      (rk/button (fa/bell) "Bell"))))

(def grid-template
  "\"a a a\" 60px
   \"b c c\" minmax(200px, 1fr)
   \"d d d\" 100px / 150px")

(ws/defcard reakit-grid-item
  {::wsm/card-width 6 ::wsm/card-height 11}
  {::wsm/align ::wsm/align-top-flex}
  (ct.react/react-card
    (rk/grid {:template grid-template :flex "1"}
      (rk/grid-item {:area "a" :backgroundColor "red"} "Header")
      (rk/grid-item {:area "b" :backgroundColor "green"} "Sidebar")
      (rk/grid-item {:area "c" :backgroundColor "blue"} "Content")
      (rk/grid-item {:area "d" :backgroundColor "yellow"} "Footer"))))

(ws/defcard reakit-popover
  {::wsm/card-width 2 ::wsm/card-height 5}
  {::wsm/align ::wsm/align-top}
  (ct.react/react-card
    (rk/popover-container
      (fn [popover]
        (rk/inline-block {:relative true}
          (rk/backdrop (js-spread {:background "transparent" :as rk/PopoverHide} popover))
          (rk/button (js-spread {:as rk/PopoverToggle} popover) "Toggle")
          (rk/popover popover
            (rk/popover-arrow)
            "Click outside to hide"))))))

(ws/defcard reakit-tooltip
  {::wsm/card-width 3 ::wsm/card-height 6}
  (ct.react/react-card
    (rk/button
      "Hover me"
      (rk/tooltip {:pos "top"} (rk/tooltip-arrow {:pos "bottom"}) "Tooltip")
      (rk/tooltip {:pos "right"} (rk/tooltip-arrow {:pos "left"}) "Tooltip")
      (rk/tooltip {:pos "bottom"} (rk/tooltip-arrow {:pos "top"}) "Tooltip")
      (rk/tooltip {:pos "left"} (rk/tooltip-arrow {:pos "right"}) "Tooltip"))))

(ws/defcard reakit-table
  {::wsm/card-width 2 ::wsm/card-height 6}
  (ct.react/react-card
    (rk/block {:overflowX "auto"}
      (rk/table
        (rk/table-caption "A Basic table")
        (rk/table-head
          (rk/table-row
            (rk/table-cell {:header true})
            (rk/table-cell {:header true} "Chars")
            (rk/table-cell {:header true} "Popularity")))
        (rk/table-body
          (rk/table-row
            (rk/table-cell {:header true} "Foo")
            (rk/table-cell "3")
            (rk/table-cell "0.7"))
          (rk/table-row
            (rk/table-cell {:header true} "Bar")
            (rk/table-cell "3")
            (rk/table-cell "0.4")))))))
