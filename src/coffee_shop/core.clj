(ns coffee-shop.core
  (:require [clojure.core.async :as a]))

(def grind-time 100)
(def brew-time 250)
(def table-size 100)
(def timeout-length 2000)


; Paths to do this.
;
; Pipeline
; Fan-out
; Fan-in
;
; Chained set of machines. All of each step sharing a set of in-out a/channels
; a -> b -> c -> done!
;
; Extra things to consider. How would programming a worker to move the items from step to step look like?

(defn my-prn [i s]
  (if (= 0 (mod i 10))
    (println i s)))

(defn create-grinder [name]
  (let [in-c (a/chan)
        out-c (a/chan)
        f (fn [] "Starts the grinder"
          (a/go (loop [i 0]
                (let [[v c] (a/alts! in-c (a/timeout timeout-length))]
                  (if (= c in-c)
                    (do
                      (my-prn i (str "Grinder: " name " handled " v))
                      (. Thread sleep grind-time)
                      (a/>! out-c (assoc v :state "ground"))
                      (recur (inc i)))
                    (do
                      (println (str "Closing grinder: " name))
                      (a/close! in-c)
                      (a/close! out-c))
                    )))))]
    [f in-c out-c]))

(defn create-grinder [name]
  (let [in-c (a/chan)
        out-c (a/chan)
        f (fn [] "Starts the grinder"
            (a/go (loop [i 0]
                  (let [[v c] (a/alts! in-c (a/timeout timeout-length))]
                    (if (= c in-c)
                      (do
                        (my-prn i (str "Brewer: " name " handled " v))
                        (. Thread sleep brew-time)
                        (a/>! out-c (assoc v :state "fresh-coffee"))
                        (recur (inc i)))
                      (do
                        (println (str "Closing brewer: " name))
                        (a/close! in-c)
                        (a/close! out-c))
                      )))))]
    [f in-c out-c]))

(defn create-table [in-chans]
  [nil nil (a/merge in-chans table-size)])

(defn ->v [vals position]
  (mapv #(nth % position) vals)
)

(defn run-sim [n]
  (let [grinders (map #(create-grinder %) ["g-1" "g-2"])
        brewers (map #(create-grinder %) ["b-1" "b-2"])
        grinders-f (->v grinders 0)
        brewers-f (->v brewers 0)
        grinders-in (->v grinders 1)
        brewers-in (->v brewers 1)
        grinders-out (create-table (->v grinders 2))
        brewers-out (create-table (->v brewers 2))
        orders (clojure.core/take n (range))]
    (doseq [b brewers-f] (b))
    (doseq [g grinders-f] (g))
    (a/go-loop [[c & rem] orders]
      (if (nil? c)
        brewers-out
        (do
           (my-prn c (str "Processing: " c))
           (let [placed {:state "order-placed" :c c}
                [v port] (a/alts! (conj (mapv #(vector % placed) grinders-in)
                                        (a/timeout timeout-length)))]
            (if (= v true)
              (recur rem)
              (do (println "Timeout hit for input")
                  brewers-out))))))))

(a/go
  (let [out-c (run-sim 50)]
    (println (str "Output: " (a/<! out-c)))))
