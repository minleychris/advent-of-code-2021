(ns advent-of-code-2021.day14)

(defn process-input [input]
  (->> input
       clojure.string/split-lines
       (map #(clojure.string/split % #" -> "))
       (map (fn [[a b]] (vector a (first b))))
       (into {})))

(defn insert-between [seed pairs]
  (reduce (fn [result c]
            (into result [(get pairs (str (last result) c)) c]))
          (vec (take 1 seed))
          (drop 1 seed)))

(defn poly [seed pairs n]
  (loop [pairs pairs
         seed seed
         i 0]
    (if (= i n)
      seed
      (recur pairs (insert-between seed pairs) (inc i)))))

(defn output-v1 [seed input n]
  (->> (poly (into [] seed) (process-input input) n)
       frequencies
       (sort-by second)))





(defn process-input2 [input]
  (->> input
       clojure.string/split-lines
       (map #(clojure.string/split % #" -> "))
       (map (fn [[a b]] (vector (vec a) (first b))))
       (into {})))

(defn seed-bigrams [seed bigrams]
  (reduce (fn [bigrams pair]
            (update bigrams pair inc))
          bigrams
          (map vector (drop-last seed) (rest seed))))

(defn bigram-to-char-freqs [bigrams seed]
  (update (reduce (fn [freqs [[a b] f]]
                    (update freqs b #(+ f %)))
                  (zipmap (distinct (apply concat (keys bigrams))) (repeat 0))
                  bigrams)
          (first seed) inc))

(defn next-step [bigrams pairs]
  (reduce (fn [bigrams [[a b] f]]
            (let [x (get pairs [a b])]
              (-> (update bigrams [a x] #(+ f %))
                  (update [x b] #(+ f %)))))
          (zipmap (keys bigrams) (repeat 0))
          bigrams))

(defn output-v2 [seed input n]
  (->> (loop [pairs (process-input2 input)
              bigrams (seed-bigrams seed
                                    (zipmap (map vec (keys pairs)) (repeat 0)))
              i 0]
         (if (= i n)
           (bigram-to-char-freqs bigrams seed)
           (recur pairs
                  (next-step bigrams pairs)
                  (inc i))))
       (sort-by second))
  )

(def small-input
"CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(def small-input-seed "NNCB")

(def big-input
"VS -> B
SV -> C
PP -> N
NS -> N
BC -> N
PB -> F
BK -> P
NV -> V
KF -> C
KS -> C
PV -> N
NF -> S
PK -> F
SC -> F
KN -> K
PN -> K
OH -> F
PS -> P
FN -> O
OP -> B
FO -> C
HS -> F
VO -> C
OS -> B
PF -> V
SB -> V
KO -> O
SK -> N
KB -> F
KH -> C
CC -> B
CS -> C
OF -> C
FS -> B
FP -> H
VN -> O
NB -> N
BS -> H
PC -> H
OO -> F
BF -> O
HC -> P
BH -> S
NP -> P
FB -> C
CB -> H
BO -> C
NN -> V
SF -> N
FC -> F
KK -> C
CN -> N
BV -> F
FK -> C
CF -> F
VV -> B
VF -> S
CK -> C
OV -> P
NC -> N
SS -> F
NK -> V
HN -> O
ON -> P
FH -> O
OB -> H
SH -> H
NH -> V
FF -> B
HP -> B
PO -> P
HB -> H
CH -> N
SN -> P
HK -> P
FV -> H
SO -> O
VH -> V
BP -> V
CV -> P
KP -> K
VB -> N
HV -> K
SP -> N
HO -> P
CP -> H
VC -> N
CO -> S
BN -> H
NO -> B
HF -> O
VP -> K
KV -> H
KC -> F
HH -> C
BB -> K
VK -> P
OK -> C
OC -> C
PH -> H")

(def big-input-seed "FNFPPNKPPHSOKFFHOFOC")