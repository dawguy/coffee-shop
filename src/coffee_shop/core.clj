(ns coffee-shop.core
  (:require [clojure.core.async :refer [go go-loop >! <! >!! <!! take! put! chan offer! poll! alts!]]))

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

(defn my-prn [i s]
  (if (= 0 (mod i 10))
    (println i s)))

(defn create-grinder [in-c out-c n]
  (go-loop [i 0]
    (let [in (<! in-c)]
      (my-prn i (str "Grinder " in))
      (. Thread sleep grind-time)
      (>! out-c (assoc in :state "ground"))
      (if (= i n)
        nil
        (recur (inc i)))
      )))

(defn create-brewer [in-c out-c n]
  (go-loop [i 0]
    (let [in (<! in-c)]
    (my-prn i (str "Brewer " in))
    (. Thread sleep brew-time)
    (>! out-c (assoc in :state "fresh-coffee"))
    (if (= i n)
      nil
      (recur (inc i)))
  )))

(defn create-table [in-c out-c n]
  (go-loop [i 0
            vals []]
    (let [[v c] (alts! [in-c (clojure.core.async/timeout 2000)])]
      (my-prn i (str "Table " i))
      (if (= c in-c)
        (if (= i n)
          (>! out-c vals)
          (recur (inc i) (conj vals v)))
        (>! out-c vals)
        ))))

(defn run-sim [n]
  (let [a-c (chan)
        b-c (chan)
        c-c (chan)
        d-c (chan)
        grinder (create-grinder a-c b-c (dec n))
        brewer (create-brewer b-c c-c (dec n))
        table (create-table c-c d-c n)
        orders (clojure.core/take n (range))]
    (go-loop [[c & rem] orders]
      (if (nil? c)
        (println (<! d-c))
        (do
                 (my-prn c (str "Processing: " c))
                 (>! a-c {:state "order-placed" :c c})
                 (recur rem))))))

(run-sim 50)
