(defproject golfure "0.0.1-SNAPSHOT"
  :description "GolfScript implementation in Clojure"
  :url "https://www.github.com/alvaro-cuesta/golfure"
  
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/core.match "0.2.0-alpha9"]
                 [org.clojure/tools.cli "0.2.1"]]
  
  :main golfure.core)