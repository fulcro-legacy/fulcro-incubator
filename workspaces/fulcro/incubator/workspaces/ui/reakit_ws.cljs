(ns fulcro.incubator.workspaces.ui.reakit-ws
  (:require [nubank.workspaces.core :as ws]
            [nubank.workspaces.model :as wsm]
            [nubank.workspaces.card-types.react :as ct.react]
            [fulcro.incubator.ui.core :refer [js-spread]]
            [fulcro.incubator.ui.reakit :as rk]
            [fulcro.incubator.ui.icons.font-awesome :as fa]
            [fulcro.client.primitives :as fp]
            [nubank.workspaces.card-types.fulcro :as ct.fulcro]
            [nubank.workspaces.lib.fulcro-portal :as f.portal]))

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

(ws/defcard reakit-input
  (ct.react/react-card
    (rk/input {:type "text"})))

(fp/defsc LocalizedKit [this _]
  {:css [[:.container
          {:margin "10px"}]

         [:.head
          {:color       "#222"
           :font-weight "bold"}]

         [:.copy
          {:color "#888"}]]
   :query []
   :ident (fn [] [::lk "single"])}
  (rk/card :.container
    (rk/heading :.head {:as "h3"} "Card Heading")
    (rk/image {:src "https://placekitten.com/180/300" :alt "Kitten" :width 180 :height 300})
    (rk/paragraph {:classes [:.copy]} "Description for the card")
    (rk/shadow)))

(ws/defcard reakit-card
  {::wsm/card-width 2 ::wsm/card-height 13}
  (ct.fulcro/fulcro-card
    {::f.portal/root LocalizedKit}))

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

(ws/defworkspace reakit
  "[\"^ \",\"c10\",[[\"^ \",\"w\",2,\"x\",0,\"i\",\"~$fulcro.incubator.workspaces.ui.reakit-ws/reakit-base\",\"y\",0,\"minH\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",2,\"i\",\"~$fulcro.incubator.workspaces.ui.reakit-ws/reakit-block\",\"y\",0,\"^1\",2,\"h\",11],[\"^ \",\"w\",1,\"x\",9,\"i\",\"~$fulcro.incubator.workspaces.ui.reakit-ws/reakit-box\",\"y\",11,\"^1\",2,\"h\",3],[\"^ \",\"w\",2,\"x\",5,\"i\",\"~$fulcro.incubator.workspaces.ui.reakit-ws/reakit-button\",\"y\",16,\"^1\",2,\"h\",4],[\"^ \",\"w\",5,\"x\",2,\"i\",\"~$fulcro.incubator.workspaces.ui.reakit-ws/reakit-flex\",\"y\",11,\"^1\",2,\"h\",5],[\"^ \",\"w\",6,\"x\",4,\"i\",\"~$fulcro.incubator.workspaces.ui.reakit-ws/reakit-grid-item\",\"y\",0,\"^1\",2,\"h\",11],[\"^ \",\"w\",2,\"x\",0,\"i\",\"~$fulcro.incubator.workspaces.ui.reakit-ws/reakit-input\",\"y\",9,\"^1\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",0,\"i\",\"~$fulcro.incubator.workspaces.ui.reakit-ws/reakit-popover\",\"y\",4,\"^1\",2,\"h\",5],[\"^ \",\"w\",2,\"x\",0,\"i\",\"~$fulcro.incubator.workspaces.ui.reakit-ws/reakit-table\",\"y\",13,\"^1\",2,\"h\",6],[\"^ \",\"w\",3,\"x\",2,\"i\",\"~$fulcro.incubator.workspaces.ui.reakit-ws/reakit-tooltip\",\"y\",16,\"^1\",2,\"h\",6],[\"^ \",\"w\",2,\"x\",7,\"i\",\"~$fulcro.incubator.workspaces.ui.reakit-ws/reakit-card\",\"y\",11,\"^1\",2,\"h\",13]],\"c8\",[[\"^ \",\"w\",2,\"x\",0,\"i\",\"^0\",\"y\",0,\"^1\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",2,\"i\",\"^2\",\"y\",16,\"^1\",2,\"h\",11],[\"^ \",\"w\",1,\"x\",7,\"i\",\"^3\",\"y\",11,\"^1\",2,\"h\",3],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^4\",\"y\",19,\"^1\",2,\"h\",4],[\"^ \",\"w\",5,\"x\",2,\"i\",\"^5\",\"y\",11,\"^1\",2,\"h\",5],[\"^ \",\"w\",6,\"x\",2,\"i\",\"^6\",\"y\",0,\"^1\",2,\"h\",11],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^7\",\"y\",9,\"^1\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^8\",\"y\",4,\"^1\",2,\"h\",5],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^9\",\"y\",13,\"^1\",2,\"h\",6],[\"^ \",\"w\",3,\"x\",4,\"i\",\"^:\",\"y\",16,\"^1\",2,\"h\",6],[\"^ \",\"i\",\"^;\",\"w\",2,\"h\",13,\"x\",4,\"y\",22,\"^1\",2]],\"c16\",[[\"^ \",\"i\",\"~$nubank.workspaces.workspaces.cards/highlight-card\",\"w\",5,\"h\",12,\"x\",0,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"~$nubank.workspaces.workspaces.cards/spotlight-card\",\"w\",4,\"h\",17,\"x\",5,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"~$nubank.workspaces.workspaces.cards\",\"w\",4,\"h\",15,\"x\",9,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^0\",\"w\",2,\"h\",4,\"x\",0,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^2\",\"w\",2,\"h\",11,\"x\",2,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^3\",\"w\",1,\"h\",3,\"x\",10,\"y\",6,\"^1\",2],[\"^ \",\"i\",\"^4\",\"w\",2,\"h\",4,\"x\",2,\"y\",11,\"^1\",2],[\"^ \",\"i\",\"^5\",\"w\",5,\"h\",5,\"x\",4,\"y\",11,\"^1\",2],[\"^ \",\"i\",\"^6\",\"w\",6,\"h\",11,\"x\",4,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^7\",\"w\",2,\"h\",4,\"x\",0,\"y\",9,\"^1\",2],[\"^ \",\"i\",\"^8\",\"w\",2,\"h\",5,\"x\",0,\"y\",4,\"^1\",2],[\"^ \",\"i\",\"^9\",\"w\",2,\"h\",6,\"x\",0,\"y\",13,\"^1\",2],[\"^ \",\"i\",\"^:\",\"w\",3,\"h\",6,\"x\",10,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^;\",\"w\",2,\"h\",13,\"x\",13,\"y\",0,\"^1\",2]],\"c14\",[[\"^ \",\"w\",2,\"x\",0,\"i\",\"^0\",\"y\",0,\"^1\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",2,\"i\",\"^2\",\"y\",0,\"^1\",2,\"h\",11],[\"^ \",\"w\",1,\"x\",10,\"i\",\"^3\",\"y\",6,\"^1\",2,\"h\",3],[\"^ \",\"w\",2,\"x\",2,\"i\",\"^4\",\"y\",11,\"^1\",2,\"h\",4],[\"^ \",\"w\",5,\"x\",4,\"i\",\"^5\",\"y\",11,\"^1\",2,\"h\",5],[\"^ \",\"w\",6,\"x\",4,\"i\",\"^6\",\"y\",0,\"^1\",2,\"h\",11],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^7\",\"y\",9,\"^1\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^8\",\"y\",4,\"^1\",2,\"h\",5],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^9\",\"y\",13,\"^1\",2,\"h\",6],[\"^ \",\"w\",3,\"x\",10,\"i\",\"^:\",\"y\",0,\"^1\",2,\"h\",6],[\"^ \",\"i\",\"^;\",\"w\",2,\"h\",13,\"x\",11,\"y\",6,\"^1\",2]],\"c2\",[[\"^ \",\"i\",\"^<\",\"w\",2,\"h\",12,\"x\",0,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^=\",\"w\",2,\"h\",17,\"x\",0,\"y\",12,\"^1\",2],[\"^ \",\"i\",\"^>\",\"w\",2,\"h\",15,\"x\",0,\"y\",29,\"^1\",2],[\"^ \",\"i\",\"^0\",\"w\",2,\"h\",4,\"x\",0,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^2\",\"w\",2,\"h\",11,\"x\",2,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^3\",\"w\",1,\"h\",3,\"x\",10,\"y\",6,\"^1\",2],[\"^ \",\"i\",\"^4\",\"w\",2,\"h\",4,\"x\",2,\"y\",11,\"^1\",2],[\"^ \",\"i\",\"^5\",\"w\",5,\"h\",5,\"x\",4,\"y\",11,\"^1\",2],[\"^ \",\"i\",\"^6\",\"w\",6,\"h\",11,\"x\",4,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^7\",\"w\",2,\"h\",4,\"x\",0,\"y\",9,\"^1\",2],[\"^ \",\"i\",\"^8\",\"w\",2,\"h\",5,\"x\",0,\"y\",4,\"^1\",2],[\"^ \",\"i\",\"^9\",\"w\",2,\"h\",6,\"x\",0,\"y\",13,\"^1\",2],[\"^ \",\"i\",\"^:\",\"w\",3,\"h\",6,\"x\",10,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^;\",\"w\",2,\"h\",13,\"x\",0,\"y\",44,\"^1\",2]],\"c12\",[[\"^ \",\"w\",2,\"x\",0,\"i\",\"^0\",\"y\",0,\"^1\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",2,\"i\",\"^2\",\"y\",0,\"^1\",2,\"h\",11],[\"^ \",\"w\",1,\"x\",10,\"i\",\"^3\",\"y\",4,\"^1\",2,\"h\",3],[\"^ \",\"w\",2,\"x\",10,\"i\",\"^4\",\"y\",0,\"^1\",2,\"h\",4],[\"^ \",\"w\",5,\"x\",2,\"i\",\"^5\",\"y\",11,\"^1\",2,\"h\",5],[\"^ \",\"w\",6,\"x\",4,\"i\",\"^6\",\"y\",0,\"^1\",2,\"h\",11],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^7\",\"y\",9,\"^1\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^8\",\"y\",4,\"^1\",2,\"h\",5],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^9\",\"y\",13,\"^1\",2,\"h\",6],[\"^ \",\"w\",3,\"x\",7,\"i\",\"^:\",\"y\",11,\"^1\",2,\"h\",6],[\"^ \",\"i\",\"^;\",\"w\",2,\"h\",13,\"x\",10,\"y\",7,\"^1\",2]],\"c4\",[[\"^ \",\"w\",2,\"x\",0,\"i\",\"^0\",\"y\",11,\"^1\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",2,\"i\",\"^2\",\"y\",30,\"^1\",2,\"h\",11],[\"^ \",\"w\",1,\"x\",0,\"i\",\"^3\",\"y\",31,\"^1\",2,\"h\",3],[\"^ \",\"w\",2,\"x\",2,\"i\",\"^4\",\"y\",26,\"^1\",2,\"h\",4],[\"^ \",\"w\",4,\"x\",0,\"i\",\"^5\",\"y\",21,\"^1\",2,\"h\",5],[\"^ \",\"w\",4,\"x\",0,\"i\",\"^6\",\"y\",0,\"^1\",2,\"h\",11],[\"^ \",\"w\",2,\"x\",2,\"i\",\"^7\",\"y\",11,\"^1\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",2,\"i\",\"^8\",\"y\",15,\"^1\",2,\"h\",6],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^9\",\"y\",15,\"^1\",2,\"h\",6],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^:\",\"y\",26,\"^1\",2,\"h\",5],[\"^ \",\"i\",\"^;\",\"w\",2,\"h\",13,\"x\",0,\"y\",34,\"^1\",2]],\"c18\",[[\"^ \",\"i\",\"^<\",\"w\",5,\"h\",12,\"x\",0,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^=\",\"w\",4,\"h\",17,\"x\",5,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^>\",\"w\",4,\"h\",15,\"x\",9,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^0\",\"w\",2,\"h\",4,\"x\",0,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^2\",\"w\",2,\"h\",11,\"x\",2,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^3\",\"w\",1,\"h\",3,\"x\",10,\"y\",6,\"^1\",2],[\"^ \",\"i\",\"^4\",\"w\",2,\"h\",4,\"x\",2,\"y\",11,\"^1\",2],[\"^ \",\"i\",\"^5\",\"w\",5,\"h\",5,\"x\",4,\"y\",11,\"^1\",2],[\"^ \",\"i\",\"^6\",\"w\",6,\"h\",11,\"x\",4,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^7\",\"w\",2,\"h\",4,\"x\",0,\"y\",9,\"^1\",2],[\"^ \",\"i\",\"^8\",\"w\",2,\"h\",5,\"x\",0,\"y\",4,\"^1\",2],[\"^ \",\"i\",\"^9\",\"w\",2,\"h\",6,\"x\",0,\"y\",13,\"^1\",2],[\"^ \",\"i\",\"^:\",\"w\",3,\"h\",6,\"x\",10,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^;\",\"w\",2,\"h\",13,\"x\",13,\"y\",0,\"^1\",2]],\"c20\",[[\"^ \",\"i\",\"^<\",\"w\",5,\"h\",12,\"x\",0,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^=\",\"w\",4,\"h\",17,\"x\",5,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^>\",\"w\",4,\"h\",15,\"x\",9,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^0\",\"w\",2,\"h\",4,\"x\",0,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^2\",\"w\",2,\"h\",11,\"x\",2,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^3\",\"w\",1,\"h\",3,\"x\",10,\"y\",6,\"^1\",2],[\"^ \",\"i\",\"^4\",\"w\",2,\"h\",4,\"x\",2,\"y\",11,\"^1\",2],[\"^ \",\"i\",\"^5\",\"w\",5,\"h\",5,\"x\",4,\"y\",11,\"^1\",2],[\"^ \",\"i\",\"^6\",\"w\",6,\"h\",11,\"x\",4,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^7\",\"w\",2,\"h\",4,\"x\",0,\"y\",9,\"^1\",2],[\"^ \",\"i\",\"^8\",\"w\",2,\"h\",5,\"x\",0,\"y\",4,\"^1\",2],[\"^ \",\"i\",\"^9\",\"w\",2,\"h\",6,\"x\",0,\"y\",13,\"^1\",2],[\"^ \",\"i\",\"^:\",\"w\",3,\"h\",6,\"x\",10,\"y\",0,\"^1\",2],[\"^ \",\"i\",\"^;\",\"w\",2,\"h\",13,\"x\",13,\"y\",0,\"^1\",2]],\"c6\",[[\"^ \",\"w\",2,\"x\",0,\"i\",\"^0\",\"y\",11,\"^1\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",4,\"i\",\"^2\",\"y\",21,\"^1\",2,\"h\",11],[\"^ \",\"w\",1,\"x\",3,\"i\",\"^3\",\"y\",20,\"^1\",2,\"h\",3],[\"^ \",\"w\",2,\"x\",0,\"i\",\"^4\",\"y\",26,\"^1\",2,\"h\",4],[\"^ \",\"w\",4,\"x\",0,\"i\",\"^5\",\"y\",15,\"^1\",2,\"h\",5],[\"^ \",\"w\",6,\"x\",0,\"i\",\"^6\",\"y\",0,\"^1\",2,\"h\",11],[\"^ \",\"w\",2,\"x\",4,\"i\",\"^7\",\"y\",11,\"^1\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",2,\"i\",\"^8\",\"y\",11,\"^1\",2,\"h\",4],[\"^ \",\"w\",2,\"x\",4,\"i\",\"^9\",\"y\",15,\"^1\",2,\"h\",6],[\"^ \",\"w\",3,\"x\",0,\"i\",\"^:\",\"y\",20,\"^1\",2,\"h\",6],[\"^ \",\"w\",2,\"x\",2,\"i\",\"^;\",\"y\",26,\"^1\",2,\"h\",13]]]")
