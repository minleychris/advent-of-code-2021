(ns advent-of-code-2021.day6)

(defn lantern-fish-next-day [state]
  (flatten
   (map (fn [fish]
          (if (= 0 fish)
            [6 8]
            (dec fish)))
        state)))

(defn lantern-fish [initial-state days]
  (loop [state initial-state
         day 0]
    (if (= day days)
      state
      (recur (lantern-fish-next-day state) (inc day)))))

(defn create-cache [block-size]
  (into {}
        (map #(vector (first %) (frequencies (lantern-fish % block-size)))
             (map vector (range 0 9)))))

(defn next-block [state-freq memo]
  (reduce (fn [freqs [fish count]]
            (->> fish
                 memo
                 (map (fn [[k v]] [k (* count v)]))
                 (reduce (fn [freqs [fish count]]
                           (update freqs fish #(+ count %)))
                         freqs)))
          (zipmap (range 0 9) (repeat 0))
          state-freq))

(defn count-final-fish [state memo blocks]
  (loop [state-freq (frequencies state)
         block 0]
    (if (= block blocks)
      (apply + (vals state-freq))
      (recur (next-block state-freq memo) (inc block)))))

(defn lantern-fish-blocks [initial-state block-size blocks]
  (let [memo (create-cache block-size)] 
    (count-final-fish initial-state memo blocks)))



(def small-input [3 4 3 1 2])

(def big-input [2,4,1,5,1,3,1,1,5,2,2,5,4,2,1,2,5,3,2,4,1,3,5,3,1,3,1,3,5,4,1,1,1,1,5,1,2,5,5,5,2,3,4,1,1,1,2,1,4,1,3,2,1,4,3,1,4,1,5,4,5,1,4,1,2,2,3,1,1,1,2,5,1,1,1,2,1,1,2,2,1,4,3,3,1,1,1,2,1,2,5,4,1,4,3,1,5,5,1,3,1,5,1,5,2,4,5,1,2,1,1,5,4,1,1,4,5,3,1,4,5,1,3,2,2,1,1,1,4,5,2,2,5,1,4,5,2,1,1,5,3,1,1,1,3,1,2,3,3,1,4,3,1,2,3,1,4,2,1,2,5,4,2,5,4,1,1,2,1,2,4,3,3,1,1,5,1,1,1,1,1,3,1,4,1,4,1,2,3,5,1,2,5,4,5,4,1,3,1,4,3,1,2,2,2,1,5,1,1,1,3,2,1,3,5,2,1,1,4,4,3,5,3,5,1,4,3,1,3,5,1,3,4,1,2,5,2,1,5,4,3,4,1,3,3,5,1,1,3,5,3,3,4,3,5,5,1,4,1,1,3,5,5,1,5,4,4,1,3,1,1,1,1,3,2,1,2,3,1,5,1,1,1,4,3,1,1,1,1,1,1,1,1,1,2,1,1,2,5,3
])