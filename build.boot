(def +project+ 'selecta)
(def +version+ "0.1.0-SNAPSHOT")

(set-env! :dependencies '[[org.clojure/clojure "1.9.0-alpha9" :scope "provided"]
                          [org.clojure/tools.reader "1.0.0-beta3"]
                          [adzerk/boot-test "1.1.2"]]
          :source-paths #{"src"}
          :resource-paths #{"src"})

(require '[adzerk.boot-test :refer :all])

(task-options!
 pom {:project +project+ :version +version+}
 jar {:main 'selecta.core :file "selecta.jar"}
 target {:dir #{"target"}})

(deftask deps [])

(deftask dev
  "Dev profile"
  []
  (set-env! :source-paths #(conj % "dev"))
  (set-env! :resource-paths #(-> %
                                 (disj "resources")
                                 (conj "dev-resources")))
  identity)

(deftask build
  []
  (comp (pom) (uber) (jar) (target)))
