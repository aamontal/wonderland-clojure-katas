(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn shuffle-cards [cards]
  (shuffle cards))

(defn deal-cards [cards]
  (let [shuffed (shuffle cards)]
    [
      (subvec shuffed, 0, (/ (count cards) 2))
      (subvec shuffed, (/ (count cards) 2), (count cards))
    ])
)

(defn rank [card]
  (second card))

(defn suit [card]
  (first card))

(defn rank-score [card]
  (.indexOf ranks (rank card)))

(defn suit-score [card]
  (.indexOf suits (suit card)))

(defn handle-rank-tie [player1-card player2-card]
  (if (> (suit-score player1-card) (suit-score player2-card))
    :player_1
    :player_2
  ))

(defn play-round [player1-card player2-card]
  (cond
    (= (rank-score player1-card) (rank-score player2-card))
      (handle-rank-tie player1-card player2-card)
    (> (rank-score player1-card) (rank-score player2-card))
      :player_1
    :else
      :player_2
  ))

(defn update-winner-deck [deck card1 card2]
    (vec (concat (rest deck) [card1 card2])))

(defn update-loser-deck [deck]
  (rest deck))

(defn play-game [player1-cards player2-cards round]
  (println "round:" round)
  (cond
    (= player1-cards []) :player_2
    (= player2-cards []) :player_1
    :else
      (let [
        player1-card (first player1-cards)
        player2-card (first player2-cards)
        winner (play-round player1-card player2-card) ]
        (println "1 card:" player1-card, "2 card: " player2-card " winner " winner)
        (if (= winner :player_1)
          (play-game (update-winner-deck player1-cards player2-card player1-card)
                     (update-loser-deck player2-cards)
                     (+ round 1))
          (play-game (update-loser-deck player1-cards)
                     (update-winner-deck player2-cards player1-card player2-card)
                     (+ round 1)) ))))
