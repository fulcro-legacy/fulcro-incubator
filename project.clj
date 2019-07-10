(defproject fulcrologic/fulcro-incubator "0.0.37"
  :description "Tools for Fulcro apps"
  :url "https://github.com/fulcrologic/fulcro-incubator"
  :license {:name "MIT" :url "https://opensource.org/licenses/MIT"}

  :plugins [[lein-tools-deps "0.4.3"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}

  :jar-exclusions [#"public/.*" #"^workspaces/.*" #"\.DS_Store" #"main" #"test"])
