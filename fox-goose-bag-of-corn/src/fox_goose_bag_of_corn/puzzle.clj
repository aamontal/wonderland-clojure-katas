(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn safe-side [pos]
  (let [set_pos (set pos)]
    (cond
      (<= (count pos) 1) true
      (contains? set_pos :you) true
      (contains? set_pos :boat) true
      (= set_pos #{:fox :corn}) true
      :else false)))

(defn wellformed-position [pos]
  (= (list :boat :corn :fox :goose :you) (sort (vec (flatten pos)))))

(def transitions
  ( hash-map
    #{} [#{} #{:you :corn} #{:you :fox} #{:you :goose}],
    #{:fox}   [#{:fox} #{:you :corn :fox} #{:you :fox} #{:you :goose :fox}],
    #{:corn}  [#{:corn} #{:you :corn :fox} #{:you :corn} #{:you :goose :fox}],
    #{:goose} [#{:goose} #{:you :goose :fox} #{:you :goose} #{:you :corn :goose}],
    #{:you}   [#{}],
    #{:fox :goose}  [#{:fox :goose} #{:you :fox :goose} #{:you :fox :goose :corn}],
    #{:fox :corn}   [#{:fox :corn} #{:you :fox :corn} #{:you :fox :goose :corn}],
    #{:goose :corn} [#{:goose :corn} #{:you :goose :corn} #{:you :fox :goose :corn}],
    #{:you :goose}  [#{} #{:goose}],
    #{:you :corn}   [#{} #{:corn}],
    #{:you :fox}    [#{} #{:fox}],
    #{:fox :goose :corn}  [#{:fox :goose :corn} #{:you :fox :goose :corn}],
    #{:you :fox :corn}    [#{:fox :corn} #{:fox} #{:corn}],
    #{:you :fox :goose}   [#{:fox :goose} #{:fox} #{:goose}],
    #{:you :goose :corn}  [#{:goose :corn} #{:goose} #{:corn}],
    #{:you :fox :goose :corn} [#{:fox :goose :corn} #{:goose :corn} #{:fox :corn} #{:fox :goose}],
    #{:boat}      [#{:boat :you} #{:boat :you :fox} #{:boat :you :corn} #{:boat :you :goose}],
    #{:boat :you} [#{:boat}],
    #{:boat :you :fox} [#{:boat}],
    #{:boat :you :corn} [#{:boat}],
    #{:boat :you :goose} [#{:boat}],
  )
)

(defn all-safe-positions [pos]
  (map
    (fn [p] (map vec (filter safe-side (get transitions (set p)))))
    pos))

(defn all-next-positions [positions]
  (let [leftside  (first positions)
        middle    (second positions)
        rightside (last positions) ]
    (for [
        l leftside
        m middle
        r rightside
        :let [pos [l m r]]
        :when (wellformed-position pos)
      ] pos)))

(def end-state
  [#{} #{:boat} #{:fox :goose :corn :you}])

(defn next-possible-positions [pos]
  (->> pos
      all-safe-positions
      all-next-positions))

(defn path-to-set [pos]
  (map (fn [p] (set p)) pos))

(defn dfs [root visited path-to]
  (cond
    (= (path-to-set root) end-state) (cons root path-to)
    :else
      (let [ neighbors (next-possible-positions root)
             new-neighbors (filter (fn [n] (not (contains? (set visited) (path-to-set n)))) neighbors) ]
        (mapcat
          (fn [n] (dfs n (conj visited (path-to-set root)) (concat [root] path-to)))
          new-neighbors ))))

(defn river-crossing-plan []
  (reverse (dfs (first start-pos) #{} [])))
