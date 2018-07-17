(defproject fulcro-incubator "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-tools-deps "0.4.1"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]
                           :aliases [:dev :workspaces-dev]}
  :jar-exclusions [#"public/.*" #"^workspaces/.*" #"\.DS_Store"]
  :profiles {:dev {:source-paths ["src" "workspaces"]}})
