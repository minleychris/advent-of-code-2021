(ns advent-of-code-2021.day18)

(defn add-in [n before after]
  (let [x-position (loop [i 0] (if (or (= i (count n))
                                       (= :X (get n i)))
                                 i (recur (inc i))))
        ;; _ (println (count n) before after x-position)
        n (loop [i x-position] (if (or (= i -1)
                                       (number? (get n i)))
                                 (if (= i -1)
                                   n
                                   (update n i (partial + before)))
                                 (recur (dec i))))
        n (loop [i x-position] (if (or (= i (count n))
                                       (number? (get n i)))
                                 (if (= i (count n))
                                   n
                                   (update n i (partial + after)))
                                 (recur (inc i))))
        n (mapv #(if (= :X %) 0 %) n)]
    n))

(defn explode [n]
  (let [[to-add result] (loop [n n
                               result []
                               depth 0
                               exploded? false
                               to-add []]
                          (if (empty? n)
                            [to-add result]
                            (let [c (first n)
                                  n (rest n)
                                  depth (cond
                                          (= c \[) (inc depth)
                                          :else depth)
                                  result (if (and (> depth 4)
                                                  (not exploded?))
                                           (if (empty? to-add) (conj result :X) result)
                                           (conj result c))
                                  to-add (if (and (> depth 4))
                                           (conj to-add c)
                                           to-add)
                                  depth (cond
                                          (= c \]) (dec depth)
                                          :else depth)
                                  exploded? (if (and (<= depth 4)
                                                     (not (empty? to-add)))
                                              true
                                              exploded?)]
                              (recur n result depth exploded? to-add))))
        parsed (clojure.edn/read-string (apply str to-add))]
    (if (not (empty? to-add))
      (add-in result (first parsed) (second parsed))
      result)))




(defn split [n]
  (second (reduce (fn [[splitted n] el]
                    (if (and (number? el)
                             (>= el 10)
                             (not splitted))
                      [true (conj n \[ (int (Math/floor (/ el 2))) \, (int (Math/ceil (/ el 2))) \])]
                      [splitted (conj n el)]))
                  [false []]
                  n)))

(defn parse-numbers [n]
  (reduce (fn [n el]
            (if (and (not (keyword? el))
                     (>= 57 (int el) 48))
              (if (number? (last n))
                (assoc n
                       (dec (count n))
                       (. Integer parseInt (str (last n) el)))
                (conj n (. Integer parseInt (str el))))
              (conj n el)))
          []
          n))

(defn reduce-number [n]
  (loop [n (parse-numbers (into [] n))
         i 0]
    (let [new (let [exploded (explode n)
                    ;; _ (println exploded)
                    ]
                (if (= exploded n)
                  (let [splited (split n)]
                    (if (= splited n)
                      n
                      splited))
                  exploded))]
      (let [_ :a #_(println new)]
        (if (or (= n new)
                (> i 2000)
                (nil? new))
          n
          (recur new (inc i)))))))

(defn add [a b]
  (reduce-number (str "[" a "," b "]")))

(defn add-list [list]
  (reduce #(apply str (add %1 %2)) list))

(defn magnitude* [n]
  (if (number? n)
    n
    (+ (* 3 (magnitude* (first n)))
       (* 2 (magnitude* (second n))))))

(defn magnitude [n-str]
  (let [n (clojure.edn/read-string n-str)]
    (magnitude* n)))

(defn bigest-mag [list]
  (reverse (sort (for [x list
                       y list]
                   (if (= x y)
                     nil
                     (magnitude (apply str (add x y))))))))