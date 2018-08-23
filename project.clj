(defproject fulcrologic/fulcro-incubator "0.0.1"
  :description "Tools for Fulcro apps"
  :url "https://github.com/fulcrologic/fulcro-incubator"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths ["src"]

  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [org.clojure/clojurescript "1.10.339" :scope "provided"]
                 [org.clojure/core.async "0.4.474" :scope "provided"]
                 [fulcrologic/fulcro "2.5.12" :scope "provided"]]

  :jar-exclusions [#"public/.*" #"^workspaces/.*" #"\.DS_Store"])
