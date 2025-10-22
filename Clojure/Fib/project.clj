; (defproject fib "0.1.0-SNAPSHOT"
;   :description "FIXME: write description"
;   :url "http://example.com/FIXME"
;   :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
;             :url "https://www.eclipse.org/legal/epl-2.0/"}
;   :dependencies [[org.clojure/clojure "1.11.1"]]
;   :main ^:skip-aot fib.core
;   :target-path "target/%s"
;   :profiles {:uberjar {:aot :all
;                        :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
;
(defproject fib "0.1.0-SNAPSHOT"
  :description "fib bench"
  :url "https://example.com"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[org.clojure/clojure "1.11.1"]]

  ;; IMPORTANT: no ^:skip-aot here
  :main fib.core

  :target-path "target/%s"

  :profiles
  {:uberjar {:aot [fib.core]                    ;; AOT only the entry ns
             :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                        "-Xms128m" "-Xmx128m"   ;; small fixed heap = faster start, stable runs
                        "-XX:+TieredCompilation"
                        "-XX:TieredStopAtLevel=1" ;; quick C1 only
                        "-XX:+UseSerialGC"]}})     ;; tiny GC, predictable start
