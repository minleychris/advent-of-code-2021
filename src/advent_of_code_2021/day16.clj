(ns advent-of-code-2021.day16)

(defn hex-to-binary [hex]
  (reduce (fn [binary hex]
            (into binary
                 (case hex
                   \0 "0000"
                   \1 "0001"
                   \2 "0010"
                   \3 "0011"
                   \4 "0100"
                   \5 "0101"
                   \6 "0110"
                   \7 "0111"
                   \8 "1000"
                   \9 "1001"
                   \A "1010"
                   \B "1011"
                   \C "1100"
                   \D "1101"
                   \E "1110"
                   \F "1111")))
          []
          hex))

(defn get-version [stream]
  [(take 3 stream)
   (drop 3 stream)])

(defn get-type [stream]
  [(take 3 stream)
   (drop 3 stream)])

(defn remove-padding [stream]
  (let []
    (loop [stream stream]
      (if (or (not (seq stream)) (not= (take 4 stream) (repeat 4 \0)))
        stream
        (recur (drop 4 stream))))))

(defn get-content [stream]
  (loop [content []
         end false
         stream stream]
    (if end
      [content stream]
      (recur (conj content {:content (drop 1 (take 5 stream))
                            :literal (take 5 stream)})
             (= (first stream) \0)
             (drop 5 stream)))))

(defn is-literal? [type]
  (= type [\1 \0 \0]))

(defn literal-packet [version type stream]
  (let [[content stream] (get-content stream)]
    [{:version version
      :type type
      :content content}
     stream]))

(defn get-bit-length [stream]
  [(take 15 stream)
   (drop 15 stream)])

(defn get-subpacket-number [stream]
  [(take 11 stream)
   (drop 11 stream)])

;; convets a binary number represented as a vector of 1s and 0s to an integer
(defn b2i [b]
  (reduce +
          (map * (reverse (map #(- % 48) (map int b))) (iterate (partial * 2) 1))))

(defn next-packets [stream])

(defn get-subpackets-length [length original-stream]
  (let [dlength (b2i length)]
    (loop [stream (take dlength original-stream)
           packets []]
      (if (not (seq stream))
        [packets (drop dlength original-stream)]
        (let [[new-packet stream] (next-packets stream)]
          (recur stream
                 (conj packets new-packet)))))))

(defn get-subpackets-count [count stream]
  (let [dlength (b2i count)]
    (reduce (fn [[packets stream] _]
              (let [[packet stream] (next-packets stream)]
                [(conj packets packet) stream]))
            [[] stream]
            (range 0 dlength))))

(defn bit-length-subpacket-operator [version type length-type-id stream]
  (let [[length stream] (get-bit-length stream)
        [subpackets stream] (get-subpackets-length length stream)]
    
    [{:version version
      :type type
      :length-type-id length-type-id
      :length length
      :subpackets subpackets}
     stream]))

(defn number-of-packets-subpacket-operator [version type length-type-id stream]
  (let [[subpacket-number stream] (get-subpacket-number stream)
        [subpackets stream] (get-subpackets-count subpacket-number stream)]
    
    [{:version version
      :type type
      :length-type-id length-type-id
      :subpacket-number subpacket-number
      :subpackets subpackets}
     stream]))

(defn get-length-type-id [stream]
  [(take 1 stream)
   (drop 1 stream)])

(defn operator-packet [version type stream]
  (let [[length-type-id stream] (get-length-type-id stream)]
    (if (= [\0] length-type-id)
      (bit-length-subpacket-operator version type length-type-id stream)
      (number-of-packets-subpacket-operator version type length-type-id stream))))

(defn next-packets [stream]
  (let [[version stream] (get-version stream)
        [type stream] (get-type stream)]
    (if (is-literal? type)
      (literal-packet version type stream)
      (operator-packet version type stream))))

(defn parse-stream [hex]
  (loop [stream (hex-to-binary hex)
         packets []]
    (let [stream (drop (rem (count stream) 4) stream)
          stream (remove-padding stream)
          _ (println stream)]
      (if (empty? stream)
        packets
        (let [
              [new-packets stream] (next-packets stream)]
          (recur stream
                 (conj packets new-packets)))))))

(defn sum-versions [packets]
  (loop [packets packets
         sum 0]
    (let [
        ;;   _ (println packets)
          ]
      (if (not (seq packets))
        sum
        (let [subpackets (:subpackets (first packets))]
          (recur (if subpackets
                   (into (rest packets) subpackets)
                   (rest packets))
                 (+ sum (b2i (:version (first packets)))))
          )))))


(defn calculate-value [packet]
  (case (b2i (:type packet))
    4 (b2i (reduce #(into %1 (:content %2)) [] (:content packet)))

    0 (apply + (map calculate-value (:subpackets packet)))
    1 (apply * (map calculate-value (:subpackets packet)))
    2 (apply min (map calculate-value (:subpackets packet)))
    3 (apply max (map calculate-value (:subpackets packet)))
    
    5 (if (apply > (map calculate-value (:subpackets packet))) 1 0)
    6 (if (apply < (map calculate-value (:subpackets packet))) 1 0)
    7 (if (apply = (map calculate-value (:subpackets packet))) 1 0)))

;;(0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 0 0 0 1 0 0 0 1 0 1 0 1 0 1 1 0 0 0 1 0 1 1 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 1 1 0 0 0 1 1 1 0 0 0 1 1 0 1 0 0)
;; V V V T T T I L L L L L L L L L L L|V V V T T T I L L L L L L L L L L L L L L L|V V V T T T A A A A A-V V V T T T A A A A A|V V V T T T I L L L L L L L L L L L|V V V T T T A A A A A|V V V T T T A A A A A X X
(def small-input
"D2FE2838006F45291200EE00D40C8230608A004A801A8002F478620080001611562C8802118E34C0015000016115A2E0802F182340A0016C880162017C3686B18A3D4780")

(def big-input "220D4B80491FE6FBDCDA61F23F1D9B763004A7C128012F9DA88CE27B000B30F4804D49CD515380352100763DC5E8EC000844338B10B667A1E60094B7BE8D600ACE774DF39DD364979F67A9AC0D1802B2A41401354F6BF1DC0627B15EC5CCC01694F5BABFC00964E93C95CF080263F0046741A740A76B704300824926693274BE7CC880267D00464852484A5F74520005D65A1EAD2334A700BA4EA41256E4BBBD8DC0999FC3A97286C20164B4FF14A93FD2947494E683E752E49B2737DF7C4080181973496509A5B9A8D37B7C300434016920D9EAEF16AEC0A4AB7DF5B1C01C933B9AAF19E1818027A00A80021F1FA0E43400043E174638572B984B066401D3E802735A4A9ECE371789685AB3E0E800725333EFFBB4B8D131A9F39ED413A1720058F339EE32052D48EC4E5EC3A6006CC2B4BE6FF3F40017A0E4D522226009CA676A7600980021F1921446700042A23C368B713CC015E007324A38DF30BB30533D001200F3E7AC33A00A4F73149558E7B98A4AACC402660803D1EA1045C1006E2CC668EC200F4568A5104802B7D004A53819327531FE607E118803B260F371D02CAEA3486050004EE3006A1E463858600F46D8531E08010987B1BE251002013445345C600B4F67617400D14F61867B39AA38018F8C05E430163C6004980126005B801CC0417080106005000CB4002D7A801AA0062007BC0019608018A004A002B880057CEF5604016827238DFDCC8048B9AF135802400087C32893120401C8D90463E280513D62991EE5CA543A6B75892CB639D503004F00353100662FC498AA00084C6485B1D25044C0139975D004A5EB5E52AC7233294006867F9EE6BA2115E47D7867458401424E354B36CDAFCAB34CBC2008BF2F2BA5CC646E57D4C62E41279E7F37961ACC015B005A5EFF884CBDFF10F9BFF438C014A007D67AE0529DED3901D9CD50B5C0108B13BAFD6070")