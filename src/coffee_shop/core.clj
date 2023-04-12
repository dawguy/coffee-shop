(ns coffee-shop.core
  (:require [clojure.core.async :refer :all]))


; Paths to do this.
;
; Pipeline
; Fan-out
; Fan-in
;
; Chained set of machines. All of each step sharing a set of in-out channels
; a -> b -> c -> done!
;
; Extra things to consider. How would programming a worker to move the items from step to step look like?
