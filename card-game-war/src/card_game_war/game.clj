(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])

(defn index-of
  [e coll]
  (first (keep-indexed #(if (= e %2) %1) coll)))

(defn rank-value [[_ rank]] (index-of rank ranks))
(defn suit-value [[suit _]] (index-of suit suits))

(defn deal-cards [cards]
  (partition (/ (count cards) 2) cards))

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

(defn play-game [player1-cards
                 player2-cards]
  (loop [p1-queue (reduce conj clojure.lang.PersistentQueue/EMPTY player1-cards)
         p2-queue (reduce conj clojure.lang.PersistentQueue/EMPTY player2-cards)]
    (cond (nil? (seq p1-queue)) :player2
          (nil? (seq p2-queue)) :player1
          :else
          (let [p1-card (peek p1-queue)
                p2-card (peek p2-queue)
                round-winner (play-round p1-card p2-card)]
            (if (= round-winner :player1)
              (recur (conj (pop p1-queue) p1-card p2-card) (pop p2-queue))
              (recur (pop p1-queue) (conj (pop p2-queue) p1-card p2-card)))))))
