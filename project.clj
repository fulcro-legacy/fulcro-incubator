(defproject fulcrologic/fulcro-incubator "1.0.0-SNAPSHOT"
  :description "Tools for Fulcro apps"
  :url "https://github.com/fulcrologic/fulcro-incubator"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-tools-deps "0.4.1"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]
                           :aliases [:dev]}
  :jar-exclusions [#"public/.*" #"^workspaces/.*" #"\.DS_Store"]
  :profiles {:dev {:source-paths ["src" "workspaces"]}})
