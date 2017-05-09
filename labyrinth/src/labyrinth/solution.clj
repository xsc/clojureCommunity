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

(defn solve
  [labyrinth]
  (loop [position  (starting-position labyrinth)
         labyrinth labyrinth]
    (let [possible   (possible-moves labyrinth position)
          maybe-exit (first (filter #(= :Exit (tile labyrinth %)) possible))]
      (cond (empty? possible)
            nil

            maybe-exit
            (move-to labyrinth maybe-exit)

            :else
            (let [next-position (rand-nth possible)]
              (recur next-position (move-to labyrinth next-position)))))))
