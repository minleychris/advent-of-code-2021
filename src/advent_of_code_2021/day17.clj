(ns advent-of-code-2021.day17)

(defn next-step [[x y] [xv yv]]
  [[(+ x xv) (+ y yv)]
   [(cond
     (> xv 0) (dec xv)
     (< xv 0) (inc xv)
     (= xv 0) 0)
    (dec yv)]])

(defn hits-target? [[xvi yvi] [tx1 tx2 ty1 ty2]]
  (loop [[x y] [0 0]
         [xv yv] [xvi yvi]
         max-y 0
         i 0]
    (let [
          
          ;; _ (println x y xv yv)
          ]
      (if (or (> i 500)
              (or (< tx2 x)
                   (> ty2 y))
              (and (>= tx2 x tx1)
                   (>= ty1 y ty2)))
        [(cond (> i 500) :waiting
               (< tx2 x) :long
               (> tx1 x) :short
               (> ty2 y) :low
               (< ty1 y) :high
               (and (>= tx2 x tx1)
                    (>= ty1 y ty2)) (let [_ (println xvi yvi max-y)] :target)
               :else (let [_ (println xvi yvi)] :Huh?))
         max-y]
        (let [[[x y] [xv yv]] (next-step [x y] [xv yv])]
          (recur [x y] [xv yv] (if (< y max-y) max-y y)(inc i))))))
  )

(defn try-velocities [x1 x2 y1 y2 target]
  ;; (reverse
  ;;  (sort-by second
            (map #(vector (first %) (count (second %)))
                    (group-by first (for [x (range x1 x2)
                                     y (range y1 y2)]
                                 (hits-target? [x y] target)))))
;; )))

(def test-target [20 30
                  -5 -10])

(def actual-target [241 273
                    -63 -97])