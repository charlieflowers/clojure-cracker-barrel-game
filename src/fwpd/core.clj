(ns fwpd.core)
(use '[clojure.string :only (join split)])
(use '[clojure.set :only (rename-keys)])

(def directions
  {:west      {:row-delta 0 :rowpos-delta -1}
   :east      {:row-delta 0 :rowpos-delta 1}
   :northwest {:row-delta -1 :rowpos-delta -1}
   :northeast {:row-delta -1 :rowpos-delta 0}
   :southwest {:row-delta 1 :rowpos-delta 0}
   :southeast {:row-delta 1 :rowpos-delta 1}})

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

;(defn make-cell
;  [cell-num]
;  {:cell-num cell-num :is-pegged true})
;
;(defn make-row
;  [row-num]
;  (map (fn [cell-num] (make-cell cell-num)) (row-range row-num)))

(defn range-from-1
  [count]
  (range 1 (inc count)))

;(defn make-board
;  [num-rows]
;  (reduce (fn [acc rownum] (assoc acc rownum (make-row rownum))) {} (range-from-1 num-rows)))

(defn make-board
  [num-rows]
  (reduce (fn [acc cellnum] (assoc acc cellnum true)) (sorted-map) (range-from-1 (row-end num-rows))))

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
  [is-pegged]
  (cond
    is-pegged "0"
    :else "-"))

;(defn render-cells
;  [cells]
;  (map (fn [c] (str (cellnum->letter (:cell-num c)) (render-peg c))) cells))

;(defn render-row
;  [row-num max-row-num row]
;  (str (prefix-spaces row-num max-row-num) (join "  " (render-cells row))))

(defn render-cells
  [row-num board]
  (map (fn [cellnum] (str (cellnum->letter cellnum) (render-peg (board cellnum)))) (row-range row-num)))

(defn render-row
  [row-num max-row-num board]
  (str (prefix-spaces row-num max-row-num) (join "  " (render-cells row-num board))))

;(defn render-board-to-seq
;  [board]
;  (let [row-count (count (keys board))]
;    (map (fn [row-num] (render-row row-num row-count (board row-num))) (range-from-1 row-count))))

(defn board-max-row
  [board]
  (row-num (apply max (keys board))))

(defn render-board-to-seq
  [board]
  (let [max-cell (board-max-row board) row-count (row-num max-cell)]
    (map (fn [row-num] (render-row row-num row-count board)) (range-from-1 row-count))))

(defn render-board-to-console
  [board]
  (doseq [row (render-board-to-seq board)]
    (println row)))

(defn delta->coords
  [cellnum row-delta rowpos-delta]
  (let [start-row (row-num cellnum) start-rowpos (row-pos cellnum)]
    {:new-row (+ start-row row-delta) :new-rowpos (+ start-rowpos rowpos-delta)}))

(defn direction->coords
  [cellnum direction]
  (let [{:keys [row-delta rowpos-delta]} (directions direction)]
    (delta->coords cellnum row-delta rowpos-delta)))

(defn scalp-at-bottom-of-board
  [rownum maxrow neighbor]
  (if (> rownum maxrow)
    (assoc neighbor :valid false :reason :off-bottom-of-board)
    neighbor))

(defn apply-direction
  [cellnum d maxrow]
  (let [{rownum :new-row rowpos :new-rowpos} (direction->coords cellnum d)
        destination (scalp-at-bottom-of-board rownum maxrow (cell-at rownum rowpos))]
    {:startcell cellnum :direction {d (directions d)} :destination (merge destination {:rownum rownum :rowpos rowpos})}))

(defn neighbor-candidates
  [cellnum maxrow]
  (map (fn [direction] (rename-keys (apply-direction cellnum direction maxrow) {:destination :neighbor})) (keys directions)))

(defn raw-move-candidates
  [cellnum board]
  (let [maxrow (board-max-row board)
        the-neighbor-candidates (neighbor-candidates cellnum maxrow)]
    (map (fn [{:keys [direction neighbor] :as nc}]
           (cond
             (not (neighbor :valid)) (assoc nc :target {:cellnum nil :valid false :reason :neighbor-is-invalid})
             :else (assoc nc :target (:destination (apply-direction (:cellnum neighbor) (first (keys direction)) maxrow))))) the-neighbor-candidates)))

(defn pegged-move-candidates
  [board raw-move-candidates]
  (map (fn [{:keys [neighbor target] :as rmc}]
         (if (and (:valid neighbor) (:valid target))
           (assoc-in (assoc-in rmc [:neighbor :is-pegged] (board (neighbor :cellnum))) [:target :is-pegged] (board (target :cellnum)))
           rmc)) raw-move-candidates))

(defn final-move-candidates
  [pegged-move-candidates]
  (map (fn [{:keys [neighbor target] :as pmc}]
         (cond
           (not (:valid neighbor)) (assoc pmc :move {:valid false :reason :bad-neighbor})
           (not (:valid target)) (assoc pmc :move {:valid false :reason :bad-target})
           (not (:is-pegged neighbor)) (assoc pmc :move {:valid false :reason :cant-jump-an-empty-cell})
           (:is-pegged target) (assoc pmc :move {:valid false :reason :cant-jump-to-a-cell-that-is-occupied})
           :else (assoc pmc :move {:valid true :reason nil}))) pegged-move-candidates))

(defn board-move-candidates
  [board]
  (flatten (map (fn [cellnum] (final-move-candidates (pegged-move-candidates board (raw-move-candidates cellnum board)))) (keys board))))

(defn legal-moves
  [board]
  (filter (fn [mc] (get-in mc [:move :valid])) (board-move-candidates board)))

(defn all-full?
  [board]
  (every? identity (vals board)))

(defn any-moves?
  [board]
  (some identity (legal-moves board)))

(defn status
  [board]
  (render-board-to-console board)
  (cond
    (all-full? board) (println "You need to remove a peg")
    (not (any-moves? board)) (println "Game over, no moves!")
    :else (println "still got life left in ya, keep going"))
  board)

(defn remove-first-peg
  [board peg]
  (if (all-full? board)
    (assoc board (letter->cellnum peg) false)
    board))

(defn moves
  [board]
  (let [mvs (legal-moves board)]
    (if (empty? mvs)
      (println "Sorry, you don't have any more moves!")
      (doseq [option (map (fn [lm nbr] [ nbr (cellnum->letter (:startcell lm)) (cellnum->letter (get-in lm [:target :cellnum]))]) (legal-moves board) (iterate inc 1))]
        (println (first option) ": from " (second option) " to " (last option)))))
  board)




