(defproject icbl "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [compojure "1.3.1"]
                 [ring/ring-defaults "0.1.2"]
                 [postgresql/postgresql "9.1-901.jdbc4"]
                 [org.clojure/java.jdbc "0.2.3"]
                 [lib-noir "0.7.6"]
                 [selmer "0.5.4"]
                 [ring-server "0.3.1"]
                 [org.clojure/data.json "0.2.6"]
                 ;[clj-time "0.9.0"]
                 ;[clj-time "0.11.0"]
                 ;[com.draines/postal "1.11.3"]
                 ;[net.mikera/core.matrix "0.36.1"]
                 ;[net.mikera/vectorz-clj "0.31.0"]
                 ]
  :repl-options {:init-ns icbl.repl
                 :timeout 1200000}
  :jvm-opts ["-Xmx512M"]
  :plugins [[lein-ring "0.8.13"]]
  :ring {:handler icbl.handler/app
         :init icbl.handler/init
         :destroy icbl.handler/destroy
         :port 3500}
  :aot :all
  :profile
  {:production
   {:ring
    {:open-browser? false, :stacktraces? false, :auto-reload? false}}
  :dev {:dependencies [[ring/ring-devel "1.2.1"]
                        [ring-mock "0.1.5"]]}}
  :main icbl.handler)

