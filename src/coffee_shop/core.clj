(ns coffee-shop.core
  (:require [clojure.core.async :as a]))

(def worker-time 25)
(def grind-time 100)
(def brew-time 250)
(def time-variance 25)
(def table-size 500)
(def timeout-length 2000)
(def num-grinders 2)
(def num-brewers 2)
(def num-workers 1)
(def coffees-to-make 50)

(defn my-prn [i s]
  (if (= 0 (mod i 5))
    (println i s)))

(defn create-grinder [name worker-queue]
  (let [in-c (a/chan)]
    (println (str "Starting grinder " name))
    (a/go (loop [i 0]
            (let [[v c] (a/alts! [in-c (a/timeout timeout-length)])]
              (if (= c in-c)
                (do
                  (my-prn i (str "Grinder: " name " handled " v))
                  (. Thread sleep (+ grind-time (rand-int time-variance)))
                  (a/>! worker-queue (assoc v :state "ground"))
                  (recur (inc i)))
                (do
                  (println (str "Closing grinder: " name))
                  (a/close! in-c))))))
    [in-c worker-queue]))

(defn create-brewer [name worker-queue]
  (let [in-c (a/chan)]
    (println (str "Starting brewer " name))
    (a/go (loop [i 0]
            (let [[v c] (a/alts! [in-c (a/timeout timeout-length)])]
              (if (= c in-c)
                (do
                  (my-prn i (str "Brewer: " name " handled " v))
                  (. Thread sleep (+ brew-time (rand-int time-variance)))
                  (a/>! worker-queue (assoc v :state "fresh-coffee"))
                  (recur (inc i)))
                (do
                  (println (str "Closing brewer: " name))
                  (a/close! in-c))))))
    [in-c worker-queue]))

(defn next-stage [v tables]
  (cond
    (= (:state v) "order-placed") (:rf-grinders-table tables)
    (= (:state v) "ground") (:rf-brewers-table tables)
    (= (:state v) "fresh-coffee") (:fresh-coffee-table tables)
    :else nil))

(defn create-worker [name worker-queue tables fired-mult] "Workers take from machines and place on tables"
  (println (str "Starting worker " name))
  (let [fired-c (a/tap fired-mult (a/chan))]
    (a/go (loop [i 0]
            (let [fired-v (a/poll! fired-c)]
              (if (nil? fired-v)
                (let [[v port] (a/alts! (vector worker-queue (a/timeout timeout-length)))]
                  (if v
                    (let [table-c (next-stage v tables)]
                      (my-prn i (str "Worker: " name " handled " v))
                      (. Thread sleep (+ worker-time (rand-int time-variance)))
                      (a/>! table-c (assoc v :state "fresh-coffee"))
                      (recur (inc i)))
                    (do
                      (println (str "Closing worker: " name))
                      (a/untap fired-mult fired-c)
                    ))
                  )
                (if (= fired-v name)
                  (do
                    (println (str "FIRING worker: " name))
                    (a/untap fired-mult fired-c))
                  (recur i))))))))

(defn table-to-ports [table-c ports] "Tables place on machines"
  (a/go-loop []
    (let [in-v (a/<! table-c)]
      (if (nil? in-v)
        nil
        (let [[v port] (a/alts! (conj (mapv #(vector % in-v) ports)
                                      (a/timeout timeout-length)))]
          (if (= v true)
            (recur)
            (do (println "Timeout hit for table")
                nil)))))))

(defn orders-to-worker-queue [orders worker-queue]
  (a/go-loop [[c & rem] orders]
    (if (nil? c)
      (do
        (println (str "All orders placed!"))
        nil)
      (do
        (my-prn c (str "Processing: " c))
        (let [order {:state "order-placed" :c c}]
          (a/>! worker-queue order)
          (recur rem))))))

(defn run-sim [n]
  (let [fired-mult (a/mult (a/chan))
        tables {:worker-queue (a/chan table-size)
                :rf-grinders-table (a/chan table-size)
                :rf-brewers-table (a/chan table-size)
                :fresh-coffee-table (a/chan table-size)}
        grinder-ports (mapv #(create-grinder % (:worker-queue tables)) (mapv #(str "GRINDER-" %) (take num-grinders (range))))
        brewer-ports (mapv #(create-brewer % (:worker-queue tables)) (mapv #(str "BREWER-" %) (take num-brewers (range))))
        workers (mapv #(create-worker % (:worker-queue tables) tables fired-mult) (mapv #(str "WORKER-" %) (take num-workers (range))))
        orders (clojure.core/take n (range))]
    (table-to-ports (:rf-grinders-table tables) (map first grinder-ports))
    (table-to-ports (:rf-brewers-table tables) (map first brewer-ports))
    (orders-to-worker-queue orders (:worker-queue tables))
    (:fresh-coffee-table tables)))

(let [out-c (run-sim coffees-to-make)]
  (a/go-loop [coffees []]
    (let [[v port] (a/alts! [out-c (a/timeout (* 2 timeout-length))])]
      (if (nil? v)
        (do
          (prn coffees)
          coffees)
        (recur (conj coffees v))))))
