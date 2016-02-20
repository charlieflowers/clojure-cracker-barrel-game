(ns fwpd.core)

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn string-to-array
  [string]
  (map #(clojure.string/split % #",") (clojure.string/split string #"\r\n")))

(defn mapify
  [rows]
  (map (fn [[name index] [name-key index-key]]
         {name-key name index-key (convert index-key index)})
       rows
       (repeat vamp-keys)))

(defn glitter-filter
  [min-glitter records]
  (filter #(>= (:glitter-index %) min-glitter) records))

(defn triangle-nums
  ([] (triangle-nums 1 2))
  ([current-val next-inc]
    (lazy-seq (cons current-val (triangle-nums (+ current-val next-inc) (inc next-inc))))))

(defn row-end
  [row-num]
  (last (take row-num (triangle-nums))))

(defn row-start
  [row-num]
  (if (= row-num 1) 1 (inc (row-end (dec row-num)))))

(defn row-range
  [row-num]
  (let [start-cell (row-start row-num) end-cell (row-end row-num)]
    (range start-cell (inc end-cell))))

(defn make-cell
  [cell-num]
  {:cell-num cell-num :is-pegged true })

(defn make-row
  [row-num]
  (map (fn [cell-num] (make-cell cell-num)) (row-range row-num)))

(defn range-from-1
  [count]
  (range 1 (inc count)))

(defn make-board
  [num-rows]
  (reduce (fn [acc rownum] (assoc acc rownum (make-row rownum))) {} (range-from-1 num-rows)))

(defn row-num
  [cell-num]
  (inc (count (take-while (fn [n] (< n cell-num)) (triangle-nums)))))

(defn row-pos
  [cell-num]
  (reduce (fn [acc n] (if (= n cell-num) (reduced acc) (inc acc))) 1 (row-range (row-num cell-num))))

(defn cell-at
  [rownum rowpos]
  (let [range (row-range rownum)]
    (cond
      (<= rowpos 0) nil
      (> rowpos (count range)) nil
      :else (nth (row-range rownum) (dec rowpos)))))

(defn upper-neighbors
  [cell-num]
  (let [row-above (dec (row-num cell-num)) my-row-pos (row-pos cell-num)]
    (remove nil? [(cell-at row-above (dec my-row-pos)) (cell-at row-above my-row-pos)])))
