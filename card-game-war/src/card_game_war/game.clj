(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [:2 :3 :4 :5 :6 :7 :8 :9 :10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn index-of
  [e coll]
  (first (keep-indexed #(if (= e %2) %1) coll)))

(defn rank-value [[_ rank]] (index-of rank ranks))
(defn suit-value [[suit _]] (index-of suit suits))

(defn deal-cards [] (partition 26 (shuffle cards)))

(defn play-round [player1-card player2-card]
  (let [rank-result (compare
                      (rank-value player1-card)
                      (rank-value player2-card))
        suit-result (compare
                      (suit-value player1-card)
                      (suit-value player2-card))]

    (if (zero? rank-result)
      (if (pos? suit-result)
        :player1
        :player2)
      (if (pos? rank-result)
        :player1
        :player2))))

(defn play-game [player1-cards player2-cards])
