(ns coffee-shop.core
  (:require [clojure.core.async :as a]))

(def worker-time 250)
(def grind-time 400)
(def brew-time 1250)
(def time-variance 100)
(def table-size 2)
(def timeout-length 10000)
(def num-grinders 4)
(def num-brewers 4)
(def num-workers 2)
(def coffees-to-make 50)
(def debug true)

(defn my-prn [i s]
  (if (= 0 (mod i 5))
    (println i s))
)

(defn debug-print [s]
  (if debug
    (println s)))

(defn create-grinder [name]
  (let [in-c (a/chan)
        out-c (a/chan)]
    (println (str "Starting grinder " name))
    (a/go (loop [i 0]
            (let [[v c] (a/alts! [in-c (a/timeout timeout-length)])]
              (if (= c in-c)
                (do
                  (my-prn i (str "Grinder: " name " handled " v))
                  (. Thread sleep (+ grind-time (rand-int time-variance)))
                  (a/>! out-c (assoc v :state "ground"))
                  (recur (inc i)))
                (do
                  (debug-print (str "Closing grinder: " name))
                  (a/close! in-c)
                  (a/close! out-c))))))
    [in-c out-c]))

(defn create-brewer [name]
  (let [in-c (a/chan)
        out-c (a/chan)]
    (println (str "Starting brewer " name))
    (a/go (loop [i 0]
            (let [[v c] (a/alts! [in-c (a/timeout timeout-length)])]
              (if (= c in-c)
                (do
                  (my-prn i (str "Brewer: " name " handled " v))
                  (. Thread sleep (+ brew-time (rand-int time-variance)))
                  (a/>! out-c (assoc v :state "fresh-coffee"))
                  (recur (inc i)))
                (do
                  (debug-print (str "Closing brewer: " name))
                  (a/close! in-c)
                  (a/close! out-c))))))
    [in-c out-c]))

(defn v-to-ports [v ports] "Places value on an available port"
  (if (nil? v)
    nil
    (a/go
      (let [[pv port] (a/alts! (conj (mapv #(vector % v) ports)
                                    (a/timeout timeout-length)))]
        (if pv
          pv
          (do (println (str "Timeout hit for v to ports " v))
              nil))))))

(defn v-to-port [v port] "Places value on the port"
  (if (nil? v)
    nil
    (a/go
      (let [[pv port] (a/alts! (vector [port v]
                                      (a/timeout timeout-length)))]
        (if pv
          pv
          (do (println (str "Timeout hit for v to port " v))
              nil))))))

(defn next-stage [v tables]
  (debug-print (str "next-stage" v))
  (cond
    (= (:state v) "order-placed") (v-to-port (assoc v :state "grinders-table") (:rf-grinders-table tables))
    (= (:state v) "grinders-table") (v-to-ports v (:grinders-in tables))
    (= (:state v) "ground") (v-to-port (assoc v :state "brewing-table") (:rf-brewers-table tables))
    (= (:state v) "brewing-table") (v-to-ports v (:brewers-in tables))
    (= (:state v) "fresh-coffee") (v-to-port (assoc v :state "fresh-coffee") (:fresh-coffee-table tables))
    :else nil))

(defn create-worker [name tables] "Workers take from machines and place on tables"
  (println (str "Starting worker " name))
  (a/go (loop [i 0]
    (let [[v port] (a/alts! (vector (a/timeout timeout-length)
                                    (:brewers-out tables)
                                    (:rf-brewers-table tables)
                                    (:grinders-out tables)
                                    (:rf-grinders-table tables)
                                    (:registers-out tables))
                            {:priority true})]
      (. Thread sleep worker-time)
      (if (and v (next-stage v tables))
        (do
            (recur (inc i)))
        (do
          (debug-print (str "Closing worker: " name))))))))

(defn orders-to-worker-queue [orders register-out]
  (a/go-loop [[c & rem] orders]
    (if (nil? c)
      (do
        (println (str "All orders placed!"))
        nil)
      (do
        (my-prn c (str "Processing: " c))
        (let [order {:state "order-placed" :c c}]
          (a/>! register-out order)
          (recur rem))))))

(defn run-sim [n]
  (let [grinders (mapv #(create-grinder %) (mapv #(str "GRINDER-" %) (take num-grinders (range))))
        brewers (mapv #(create-brewer %) (mapv #(str "BREWER-" %) (take num-brewers (range))))
        tables {:rf-grinders-table (a/chan table-size)
                :rf-brewers-table (a/chan table-size)
                :fresh-coffee-table (a/chan coffees-to-make)}
        machine-outputs {:registers-out (a/chan table-size)                        ; used for placing orders. not implemented
                         :grinders-out (a/merge (mapv second grinders))
                         :brewers-out  (a/merge (mapv second brewers))
                         :grinders-in (mapv first grinders)
                         :brewers-in (mapv first brewers)}
        workers (mapv #(create-worker % (merge tables machine-outputs))
                      (mapv #(str "WORKER-" %) (take num-workers (range))))
        orders (clojure.core/take n (range))]
    (orders-to-worker-queue orders (:registers-out machine-outputs))
    (:fresh-coffee-table tables)))

(comment
  (let [out-c (run-sim coffees-to-make)]
    (a/go-loop [coffees []]
      (if (= (count coffees) coffees-to-make)
        (prn (str "COFFEE DONE:::" coffees))
        (let [[v port] (a/alts! [out-c (a/timeout (* 3 timeout-length))])]
          (if (nil? v)
            (do
              (prn (str "COFFEE TIMEOUT:::" coffees))
              coffees)
            (recur (conj coffees v)))))))
  ,)
