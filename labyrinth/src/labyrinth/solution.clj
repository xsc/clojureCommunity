(ns labyrinth.solution)

(defn tile
  [labyrinth [x y]]
  (get-in labyrinth [y x]))

(defn width
  [labyrinth]
  (count (first labyrinth)))

(defn height
  [labyrinth]
  (count labyrinth))

(defn starting-position
  [labyrinth]
  (first
    (for [x (range (width labyrinth))
          y (range (height labyrinth))
          :let [t (tile labyrinth [x y])]
          :when (= t 1)]
      [x y])))

(defn neighbors
  [labyrinth [x y]]
  (let [w (width labyrinth)
        h (height labyrinth)]
    (->> (vector
           [x (dec y)]
           [x (inc y)]
           [(dec x) y]
           [(inc x) y])
         (filter
           (fn [[x' y']]
             (and (< -1 x' w)
                  (< -1 y' h)))))))

(defn possible-moves
  [labyrinth position]
  (->> (neighbors labyrinth position)
       (filter #(contains? #{:Exit :_} (tile labyrinth %)))))

(defn move-to
  [labyrinth [x y]]
  (update labyrinth
          y
          (fn [row]
            (assoc row x 1))))

(defn find-exit
  [labyrinth]
  (first
    (for [x (range 0 (width labyrinth))
          y (range 0 (height labyrinth))
          :when (= (tile labyrinth [x y]) :Exit)]
      [x y])))

(defn distance
  [[x0 y0] [x1 y1]]
  (let [delta-x (- x0 x1)
        delta-y (- y0 y1)]
    (+ (* delta-x delta-x) (* delta-y delta-y))))

(defn solve-from
  [labyrinth exit-position position]
  (let [possible   (->> (possible-moves labyrinth position)
                        (sort-by #(distance exit-position %)))
        maybe-exit (first (filter #(= :Exit (tile labyrinth %)) possible))]
    (cond (empty? possible)
          nil

          maybe-exit
          (move-to labyrinth maybe-exit)

          :else
          (some
            (fn [selected-move]
              (solve-from
                (move-to labyrinth selected-move)
                exit-position
                selected-move))
            possible))))

(defn solve
  [labyrinth]
  (if-let [start (starting-position labyrinth)]
    (or (solve-from
          labyrinth
          (find-exit labyrinth)
          start)
        ["unable to exit"])
    labyrinth))
