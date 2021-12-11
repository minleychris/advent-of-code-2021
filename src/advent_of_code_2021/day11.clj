(ns advent-of-code-2021.day11)

(defn nsinc [i]
  (if (nil? i)
    nil
    (inc i)))

(defn do-flash
  ([grid row i]
   (if (or (keyword? (get-in grid [row i]))
           (< (get-in grid [row i]) 10))
     grid
     (let [grid (assoc-in grid [row i] :f)
           grid (do-flash grid row (dec i) inc)
           grid (do-flash grid row (inc i) inc)
           grid (do-flash grid (inc row) i inc)
           grid (do-flash grid (inc row) (dec i) inc)
           grid (do-flash grid (inc row) (inc i) inc)
           grid (do-flash grid (dec row) i inc)
           grid (do-flash grid (dec row) (dec i) inc)
           grid (do-flash grid (dec row) (inc i) inc)]
       grid)))
  ([grid row i fn]
   (if (and (and (>= row 0) (< row (count grid)))
            (and (>= i 0) (< i (count (first grid))))
            (not (keyword? (get-in grid [row i]))))
    (let [grid (update-in grid [row i] fn)]
         (do-flash grid row i))
     grid)))

(defn do-flashes [grid]
  (loop [grid grid
         row 0
         i 0]
    (if (<= (count grid) row)
      grid
      (let [grid (do-flash grid row i)
            row (if (= (count (first grid)) (inc i))
                  (inc row)
                  row)
            i (if (= (count (first grid)) (inc i))
                0
                (inc i))]
        (recur grid row i)))))

(defn reset-fs [cnt grid]
  [(map #(map (fn [e] (if (keyword? e) 0 e)) %) grid)
   (reduce + cnt (map count (map #(filter keyword? %) grid)))])

(defn next-step [grid count]
  (->> grid
       (mapv #(mapv inc %))
       do-flashes
       (reset-fs count)))

(defn steps-v1 [grid steps]
  (reduce (fn [[grid count] _]
            (next-step grid count))
          [grid 0]
          (range 0 steps)))

(defn steps-v2 [grid]
  (loop [grid grid
         step 0
         cnt 0]
    (let [_ (println step cnt)]
         (if (= (* (count grid) (count (first grid))) cnt)
           step
           (let [[grid cnt] (next-step grid 0)]
             (recur grid (inc step) cnt))))))

(def test-input (mapv (fn [e] (mapv #(. Integer parseInt  %) e)) (mapv #(clojure.string/split % #"") (clojure.string/split-lines
"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"))))

(def small-test-input  (mapv (fn [e] (mapv #(. Integer parseInt  %) e)) (mapv #(clojure.string/split % #"") (clojure.string/split-lines
"11111
19991
19191
19991
11111"))))


(def final-input  (mapv (fn [e] (mapv #(. Integer parseInt  %) e)) (mapv #(clojure.string/split % #"") (clojure.string/split-lines
"2566885432
3857414357
6761543247
5477332114
3731585385
1716783173
1277321612
3371176148
1162578285
6144726367"))))