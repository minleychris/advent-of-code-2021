(ns advent-of-code-2021.day21)

(defn dice-rolls [last-dice-roll]  
  (drop 1 (reduce (fn [rolls _]
                    (let [roll (mod (+ 1 (last rolls)) 100)]
                      (conj rolls (if (= 0 roll) 100 roll))))
                  [last-dice-roll]
                  (range 3))))

(defn play [score position dice-rolls]
  (let [rolls-total (apply + dice-rolls)
        position (if (= (mod (+ rolls-total position) 10) 0)
                   10
                   (mod (+ rolls-total position) 10))]
    [(+ score position) position]))

(defn play-deterministic [player-1-start player-2-start]
  (loop [last-dice-roll 100
         player-1-score 0
         player-1-position player-1-start
         player-2-score 0
         player-2-position player-2-start
         last-player 2
         n 0]
    (let [_ (println player-1-position player-1-score player-2-position player-2-score)])
    (if (or (>= player-1-score 1000)
            (>= player-2-score 1000)
            (>= n 10000))
      {:player-1-score player-1-score
       :player-2-score player-2-score
       :n n}
      (let [dice-rolls (dice-rolls last-dice-roll)
            [player-1-score player-1-position] (if (= last-player 2)
                                                 (play player-1-score player-1-position dice-rolls)
                                                 [player-1-score player-1-position])
            [player-2-score player-2-position] (if (= last-player 1)
                                                 (play player-2-score player-2-position dice-rolls)
                                                 [player-2-score player-2-position])
            last-player (if (= last-player 1) 2 1)]
        (recur (last dice-rolls)
               player-1-score
               player-1-position
               player-2-score
               player-2-position
               last-player
               (inc n))))))