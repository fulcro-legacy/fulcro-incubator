(ns fulcro.incubator.workspaces.main
  (:require [nubank.workspaces.core :as ws]
            [fulcro.incubator.workspaces.db-helpers-ws]
            [fulcro.incubator.mutation-interface-ws]
            [fulcro.incubator.flicker-free-ws]
            [fulcro.incubator.defsc-extension-ws]
            [fulcro.incubator.pessimistic-mutations-ws]
            [fulcro.incubator.workspaces.ui.reakit-ws]))

(ws/mount)
