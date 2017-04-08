(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= :player_1 (play-round [:heart 9] [:club 8])))
    (is (= :player_2 (play-round [:heart 8] [:club 9])))
  )
  (testing "queens are higher rank than jacks"
    (is (= :player_1 (play-round [:heart :queen] [:heart :jack])))
    (is (= :player_2 (play-round [:heart :jack] [:club :queen])))
  )
  (testing "kings are higher rank than queens"
    (is (= :player_1 (play-round [:heart :king] [:heart :queen])))
    (is (= :player_2 (play-round [:heart :queen] [:club :king])))
  )
  (testing "aces are higher rank than kings"
    (is (= :player_1 (play-round [:heart :ace] [:heart :king])))
    (is (= :player_2 (play-round [:heart :king] [:heart :ace])))
  )
  (testing "if the ranks are equal, clubs beat spades"
    (is (= :player_1 (play-round [:club :ace] [:spade :ace])))
    (is (= :player_2 (play-round [:spade :ace] [:club :ace])))
  )
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= :player_1 (play-round [:diamond :ace] [:club :ace])))
    (is (= :player_2 (play-round [:club :ace] [:diamond :ace])))
  )
  (testing "if the ranks are equal, hearts beat diamonds")
    (is (= :player_1 (play-round [:heart :ace] [:diamond :ace])))
    (is (= :player_2 (play-round [:diamond :ace] [:heart :ace])))
  )

(deftest test-play-game
  (testing "the player loses when they run out of cards")
    (is (= :player_1 (play-game [[:heart :ace]] [[:diamond :ace]])))
  )
