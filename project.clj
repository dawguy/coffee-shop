(defproject coffee-shop "0.1.0-SNAPSHOT"
  :description "Coffee shop simulation using core async"
  :url "https://github.com/dawguy/coffee-shop"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.async "1.6.673"]]
  :repl-options {:init-ns coffee-shop.core})
