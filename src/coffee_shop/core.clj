(ns coffee-shop.core
  (:require [clojure.core.async :refer [>! <! >!! <!! take! put! chan offer! poll!]]))

(def grind-time 100)
(def brew-time 250)


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

(defn create-grinder []
  (fn [in]
    (. Thread sleep grind-time)
    (assoc in :state "ground")))

(defn create-brewer []
  (fn [in]
    (. Thread sleep brew-time)
    (assoc in :state "fresh-coffee")))

(defn run-sim [n]
  (let [grinder (create-grinder)
        brewer (create-brewer)
        orders (clojure.core/take n (range))]
    (loop [[c & rem] orders
           coffees []]
      (if (nil? c)
               coffees
               (do
                 (if (= 0 (mod c 10)) (prn (str "Processing: " c)))
                 (recur rem (conj coffees (-> {:state "order-placed" :c c}
                                              (grinder)
                                              (brewer)))))))))
(run-sim 50)
