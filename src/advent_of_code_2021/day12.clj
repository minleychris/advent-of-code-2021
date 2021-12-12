(ns advent-of-code-2021.day12)

(defn process-input [input]
  (->> input
       (clojure.string/split-lines)
       (map #(clojure.string/split % #"-"))))

(defn routes-from [routes]
  (->> routes
       (concat routes (map reverse routes))
       (group-by first)
       (map (fn [[k v]]
              [k (mapv second v)]))
       (map (fn [[k v]]
              [k (distinct v)]))
       (map (fn [[k v]]
              [k (filter #(not= "start" %) v)]))
       (into {})
       (#(dissoc % "end"))))

(defn big-caves [routes]
  (->> routes
       (apply concat)
       distinct
       (map #(re-find #"[A-Z]*" %))
       (filter (complement empty?))
       vec))

(defn all-routes-v1 [routes]
  (loop [rf (routes-from routes)
         bc (big-caves routes)
         new-routes [["start"]]
         routes []
         i 0]
    (if (or (empty? new-routes) (> i 100))
      routes
      (let [new-routes (reduce (fn [routes route]
                                 (let [next-steps (get rf (last route))
                                       visited (vec (remove #(some (set bc) [%]) route))
                                       next-steps (remove #(some (set visited) [%]) next-steps)]
                                   (into routes (map #(conj route %) next-steps))))
                               []
                               new-routes)]
        (recur rf
               bc
               (filter #(not= "end" (last %)) new-routes)
               (into routes (filter #(= "end" (last %)) new-routes))
               (inc i))))))

(defn make-new-routes [rf bc new-routes]  
  (loop [routes new-routes
         new-routes []]
    (if (empty? routes)
      new-routes
      (recur (rest routes)
             (into new-routes
                     (let [route (first routes)
                           next-steps (get rf (last route))
                           visited (vec (remove #(some (set bc) [%]) route))
                           v-freq (frequencies visited)
                           has-visited-small-twice (= 2 (first (reverse (sort (vals v-freq)))))
                           next-steps (if has-visited-small-twice
                                        (remove #(some (set visited) [%]) next-steps)
                                        next-steps)]
                       (map #(conj route %) next-steps)))))))

(defn all-routes-v2 [routes]
  (loop [rf (routes-from routes)
         bc (big-caves routes)
         new-routes [["start"]]
         routes []
         i 0]
    (if (or (empty? new-routes) (> i 20))
      routes
      (let [
            new-routes (make-new-routes rf bc new-routes)]
        (recur rf
               bc
               (filter #(not= "end" (last %)) new-routes)
               (into routes (filter #(= "end" (last %)) new-routes))
               (inc i))))))

(def very-small-input
"start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def quite-small-input
"dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(def small-input
"fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

(def big-input
"start-YA
ps-yq
zt-mu
JS-yi
yq-VJ
QT-ps
start-yq
YA-yi
start-nf
nf-YA
nf-JS
JS-ez
yq-JS
ps-JS
ps-yi
yq-nf
QT-yi
end-QT
nf-yi
zt-QT
end-ez
yq-YA
end-JS")