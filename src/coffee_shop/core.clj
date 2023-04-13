(ns coffee-shop.core
  (:require [clojure.core.async :as a]))

(def grind-time 100)
(def brew-time 250)
(def table-size 500)
(def timeout-length 2000)
(def num-grinders 5)
(def num-brewers 10)
(def coffees-to-make 250)

(defn my-prn [i s]
  (if (= 0 (mod i 5))
    (println i s)))

(defn create-grinder [name]
  (let [in-c (a/chan)
        out-c (a/chan)
        f (fn [] "Starts the grinder"
            (println (str "Starting grinder " name))
            (a/go (loop [i 0]
                (let [[v c] (a/alts! [in-c (a/timeout timeout-length)])]
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

(defn create-brewer [name]
  (let [in-c (a/chan)
        out-c (a/chan)
        f (fn [] "Starts the brewer"
            (println (str "Starting brewer " name))
            (a/go (loop [i 0]
                  (let [[v c] (a/alts! [in-c (a/timeout timeout-length)])]
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
  (a/merge in-chans table-size))

(defn ->v [vals position]
  (mapv #(nth % position) vals)
)

(defn run-sim [n]
  (let [grinders (map #(create-grinder %) (mapv #(str "g-" %) (take num-grinders (range))))
        brewers (map #(create-brewer %) (mapv #(str "b-" %) (take num-brewers (range))))
        grinders-out (create-table (->v grinders 2))
        brewers-out (create-table (->v brewers 2))
        orders (clojure.core/take n (range))]
    (doseq [g (->v grinders 0)] (g))
    (doseq [b (->v brewers 0)] (b))
    (a/go-loop []
      (let [in-v (a/<! grinders-out)]
        (if (nil? in-v)
          brewers-out
          (let [[v port] (a/alts! (conj (mapv #(vector % in-v) (->v brewers 1))
                                    (a/timeout timeout-length)))]
          (if (= v true)
            (recur)
            (do (println "Timeout hit for brewer input")
                brewers-out))))))
    (a/go-loop [[c & rem] orders]
      (if (nil? c)
        brewers-out
        (do
           (my-prn c (str "Processing: " c))
           (let [placed {:state "order-placed" :c c}
                [v port] (a/alts! (conj (mapv #(vector % placed) (->v grinders 1))
                                        (a/timeout timeout-length)))]
            (if (= v true)
              (recur rem)
              (do (println "Timeout hit for input")
                  brewers-out))))))
    brewers-out))

(let [out-c (run-sim coffees-to-make)]
  (a/go-loop [coffees []]
    (let [[v port] (a/alts! [out-c (a/timeout (* 2 timeout-length))])]
      (if (nil? v)
        (do
          (prn coffees)
          coffees)
        (recur (conj coffees v))))))
