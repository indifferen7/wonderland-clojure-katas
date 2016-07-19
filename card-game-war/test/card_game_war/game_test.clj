(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= :player2 (play-round [:diamond 3] [:club :jack])))
    (is (= :player1 (play-round [:diamond 3] [:club 2]))))
  (testing "queens are higher rank than jacks"
    (is (> (rank-value [:diamond :queen]) (rank-value [:diamond :jack]))))
  (testing "kings are higher rank than queens"
    (is (> (rank-value [:diamond :king]) (rank-value [:diamond :queen]))))
  (testing "aces are higher rank than kings"
    (is (> (rank-value [:diamond :ace]) (rank-value [:diamond :king]))))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= :player1 (play-round [:club 3] [:spade 3]))))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= :player1 (play-round [:diamond 3] [:club 3]))))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= :player1 (play-round [:heart 3] [:diamond 3])))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"))

