(defproject fulcrologic/fulcro-incubator "0.0.39"
  :description "Tools for Fulcro apps"
  :url "https://github.com/fulcro-legacy/fulcro-incubator"
  :license {:name "MIT" :url "https://opensource.org/licenses/MIT"}

  :plugins [[lein-tools-deps "0.4.3"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}

  :jar-exclusions [#"public/.*" #"^workspaces/.*" #"\.DS_Store" #"main" #"test"])
