(ns app.state-machine
  (:use [app.util]
        [app.deck]
        [app.strategy])
  (:require [clojure.core.match :refer [match]]))

(defn big [bs]
  (BigDecimal. (str bs)))

(defn big+ [s1 s2]
  (str (+ (big s1) (big s2))))

(defn initialize-new-deck [state]
  (let [new-deck             (shuffle (create-deck))
        [hand opponent-hand] (split-at 5 (take 10 new-deck))]
    (override state
              {:deck           (drop 10 new-deck)
               :discard        []
               :hand           (sort-deck hand)
               :opponent-hand  (sort-deck opponent-hand)
               :selected-cards (pick-good-cards hand)})))

(defn calculate-total-pot [state]
  (let [pot          (or (:pot @state) "0")
        opponent-pot (or (:opponent-pot @state) "0")]
    (str (+ (big pot) (big opponent-pot)))))

(defn calculate-min-to-check [state]
  (let [pot          (or (:pot @state) "0")
        opponent-pot (or (:opponent-pot @state) "0")]
    (if (not (>= (big pot) (big opponent-pot)))
      (str (- (big opponent-pot) (big pot)))
      "0")))

(defn- transfer-sum-from-money-to-pot [state sum]
  (let [{:keys [pot money]} @state]
    [(str (+ (big pot) (big sum))) (str (- (big money) (big sum)))]))

(defn- calculate-raise [state]
  (let [{:keys [min-raise]} @state
        min-to-check        (calculate-min-to-check state)]
    (big+ min-to-check min-raise)))

(defn- opponent-as-player [state]
  (let [{:keys [hand opponent-hand opponent-money money opponent-pot pot]} @state]
    (atom
      (merge @state
             {:money          opponent-money
              :pot            opponent-pot
              :opponent-money money
              :opponent-pot   pot
              :opponent-hand  hand
              :hand           opponent-hand}))))



(defn- make-opponent-round [{:keys [poker-state opponent-pot pot opponent-money min-raise opponent-hand]}
                            state]
  (let [call-if-raise (fn [s] (if (= :raise s)
                                :call
                                s))
         strategy                      (cond-> (fearless-strategy opponent-hand)
                                        (= :player-call poker-state) call-if-raise)
        inverse-state                 (opponent-as-player state)
        [opponent-pot opponent-money] (case strategy
                                        :raise (transfer-sum-from-money-to-pot
                                                inverse-state
                                                (calculate-raise inverse-state))
                                        :call  (transfer-sum-from-money-to-pot
                                                inverse-state
                                                (calculate-min-to-check inverse-state))
                                        [opponent-pot opponent-money])]
    (prn "Opponent decided to " strategy)
    (override state
              {:opponent-pot   opponent-pot
               :opponent-money opponent-money
               :poker-state    poker-state})))

(defn call [state]
  (override state
            {:poker-state :player-call})
  (make-opponent-round @state state)
  (when-not (= :opponent-fold (:poker-state @state))
    (let [{:keys [hand opponent-hand]} @state

          ]
      (case (better-hand? (calculate-hand hand) (calculate-hand opponent-hand))
        :yes (override state
                       {:poker-state :victory})
        :no  (override state
                       {:poker-state :loss})
        :tie (override state
                       {:poker-state :tie})))))

(defn draw [state]
  (let [{:keys [deck hand opponent-hand selected-cards discard]} @state
        cards-left                                               (count deck)
        [discard deck]                                           (if (< 10 cards-left)
                                                                   [discard deck]
                                                                   [[] (shuffle (concat discard deck))])
        opponent-picked-cards                                    (pick-good-cards opponent-hand)
        opponent-draw                                            (- 5 (count opponent-picked-cards))
        player-draw                                              (- 5 (count selected-cards))
        number-of-draws                                          (+ player-draw opponent-draw)
        [new-hand new-opponent-hand]                             (split-at player-draw (take number-of-draws deck))
        new-player-hand                                          (concat new-hand selected-cards)]
    (override state
              {:deck           (drop number-of-draws deck)
               :discard        (concat discard (take number-of-draws deck))
               :hand           (sort-deck new-player-hand)
               :poker-state    :call
               :opponent-hand  (sort-deck (concat new-opponent-hand opponent-picked-cards))
               :selected-cards (pick-good-cards new-player-hand)})
    (make-opponent-round @state state)))

(defn raise [state]
  (let [raise        (:min-raise @state)
        pot          (:pot @state)
        money        (:money @state)
        opponent-pot (:opponent-pot @state)]
    (override state
              {:pot   (str (+ (big raise) (big pot)))
               :money (str (- (big money) (big raise)))})
    (make-opponent-round @state state)))

(def chips ["25" "50" "100" "500" "1000" "5000" "10000"])

(defn split->chips [pot]
  (let [fitting-chips (fn [left]
                        (if-let [maximum-chip (last
                                               (filter
                                                (fn [chip]
                                                  (<= (big chip) (big left)))
                                                chips))]
                          (cons maximum-chip (take-while #(not= % maximum-chip) chips))))
        chip-away     (fn [start]
                        (loop [chips []
                               left  start]
                          (if-let [fitting (seq (fitting-chips left))]
                            (let [new-chip (rand-nth fitting)
                                  new-left (str (.subtract (big left) (big new-chip)))]
                              (recur (cons new-chip chips) new-left))
                            chips)))]
    (chip-away pot)))

(defn initialize-game [state]
  (override state
            {:next-round-open-by :player
             :min-bet            "50"
             :min-raise          "100"
             :money              "500"
             :opponent-money     "500"}))

(defn pre-initialize-round [{:keys [opponent-pot pot money opponent-money poker-state]} state]
  (let [total-pot                              (calculate-total-pot state)
        [money opponent-money]                 (match poker-state
                                                      :fold          [money (str (+ (big total-pot) (big opponent-money)))]
                                                      :opponent-fold [(str (+ (big total-pot) (big money))) opponent-money]
                                                      :victory       [(str (+ (big total-pot) (big money))) opponent-money]
                                                      :loss          [money (str (+ (big total-pot) (big opponent-money)))]
                                                      :tie
                                                                     [(str (+ (big pot) (big money)))
                                                                      (str (+ (big opponent-pot) (big opponent-money)))]
                                                      _ [money opponent-money])]
    (-> state
        (initialize-new-deck)
        (override
         {:money          money
          :opponent-money opponent-money}))))

(defn initialize-round [state]
  (pre-initialize-round @state state)
  (let [{:keys [next-round-open-by min-raise]} @state
        {:keys [min-bet]}                      @state
        {:keys [money opponent-money]}         @state
        {:keys [poker-state pot opponent-pot]} @state
        {:keys [pot opponent-pot]}             @state
        open-by-player?                        (= :player next-round-open-by)
        [pot opponent-pot]                     (if open-by-player?
                                                 [min-bet "0"]
                                                 ["0" min-bet])
        dec-raise                              (fn [m]
                                                 (str (- (big m) (big min-bet))))
        [money opponent-money]                 (if open-by-player?
                                                 [(dec-raise money) opponent-money]
                                                 [money (dec-raise opponent-money)])]
    (override state
              {:pot                pot
               :next-round-open-by (if open-by-player?
                                     :opponent
                                     :player)
               :money              money
               :opponent-money     opponent-money
               :poker-state        :draw
               :opponent-pot       opponent-pot})))

(defn fold-cards [state]
  (-> state (override {:poker-state :fold}) (initialize-round)))

(defn initialize [state]
  (-> state
      (initialize-game)
      (initialize-round)
      (initialize-new-deck)))