(ns fwpd.core)
(use '[clojure.string :only (join split)])

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
  {:cell-num cell-num :is-pegged true})

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
  (cond
    (<= rownum 0) {:cellnum nil :valid false :reason :off-top-of-board}
    (<= rowpos 0) {:cellnum nil :valid false :reason :off-left-side-of-board}
    (> rowpos (count (row-range rownum))) {:cellnum nil :valid false :reason :off-right-side-of-board}
    :else {:cellnum (nth (row-range rownum) (dec rowpos)) :valid true :reason nil}))

;(defn upper-neighbors
;  [cell-num]
;  (let [row-above (dec (row-num cell-num)) my-row-pos (row-pos cell-num)]
;    (remove nil? [(cell-at row-above (dec my-row-pos)) (cell-at row-above my-row-pos)])))

(defn cellnum->letter
  [cell-num]
  (char (+ 96 cell-num)))

(defn letter->cellnum
  [letter]
  (- (int letter) 96))

(defn prefix-spaces
  [max-row-num row-num]
  (apply str (repeat (cond
                       (<= row-num max-row-num) 0
                       :else
                       (* (- row-num max-row-num) 2)) " ")))

(defn render-peg
  [cell]
  (cond
    (:is-pegged cell) "0"
    :else "-"))

(defn render-cells
  [cells]
  (map (fn [c] (str (cellnum->letter (:cell-num c)) (render-peg c))) cells))

(defn render-row
  [row-num max-row-num row]
  (str (prefix-spaces row-num max-row-num) (join "  " (render-cells row))))

(defn render-board-to-seq
  [board]
  (let [row-count (count (keys board))]
    (map (fn [row-num] (render-row row-num row-count (board row-num))) (range-from-1 row-count))))

(defn render-board-to-console
  [board]
  (doseq [row (render-board-to-seq board)]
    (println row)))

(defn apply-delta
  [cellnum row-delta rowpos-delta]
  (let [start-row (row-num cellnum) start-rowpos (row-pos cellnum)]
    {:new-row (+ start-row row-delta) :new-rowpos (+ start-rowpos rowpos-delta)}))

(defn neighbor-candidates
  [cellnum maxrow]
  (let [candidates
        [{:desc :left :row-delta 0 :rowpos-delta -1}
         {:desc :right :row-delta 0 :rowpos-delta 1}
         {:desc :upper-left :row-delta -1 :rowpos-delta -1}
         {:desc :upper-right :row-delta -1 :rowpos-delta 0}
         {:desc :lower-left :row-delta 1 :rowpos-delta 0}
         {:desc :lower-right :row-delta 1 :rowpos-delta 1}]]
    (map (fn [c]
           (let [{rownum :new-row rowpos :new-rowpos} (apply-delta cellnum (:row-delta c) (:rowpos-delta c))
                 neighbor (cell-at rownum rowpos)
                 nmap (if (> rownum maxrow)
                        (assoc neighbor :valid false :reason :off-bottom-of-board)
                        neighbor)]
             (merge nmap {:rownum rownum :rowpos rowpos} c))) candidates)))


