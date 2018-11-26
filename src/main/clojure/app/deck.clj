(ns app.deck
  (:require [clojure.pprint :refer [pprint]]))


(def suits [:spade :heart :diamond :club])
(def ranks [:2 :3 :4 :5 :6 :7 :8 :9 :10 :jack :queen :king :ace])
(def hand-rankings [:high-card :pair :two-pair :three-of-kind :straight :flush :full-house :four-of-kind :straight-flush :royal-flush])

(defn sort-ranks [ranks]
  (sort-by #(.indexOf ranks %) ranks))

(defn sort-deck [deck]
  (sort-by
   (juxt
    #(.indexOf ranks (second %))
    #(.indexOf suits (first %)))
   deck))

(defn create-deck []
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn calculate-hand [hand]
  (let [map-kv                  (fn [f coll] (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))
        ssecond                 #(second (second %))
        flat-once               (fn [v] (mapcat identity v))
        upgrade->basic-pairs    (fn [cards]
                                  (case (count cards)
                                    1 [:high-card cards]
                                    2 [:pair cards]
                                    3 [:three-of-kind cards]
                                    4 [:four-of-kind cards]))
        pair-and-cards          (map-kv #(map ssecond %)
                                        (group-by (fn [[a b]] (first b))
                                                  (map-kv upgrade->basic-pairs
                                                          (group-by second hand))))
        upgrade->high-cards     (fn [k v]
                                  (let [rank-of-cards  (map second v)
                                        ace?           (seq (filter #(= :ace %) rank-of-cards))
                                        rank-values    (sort (map #(.indexOf ranks %) rank-of-cards))
                                        incrementing?  (apply = (map - rank-values (range)))
                                        suits-of-cards (frequencies (map first v))
                                        single-suit?   (some #(= 5 %) (vals suits-of-cards))]
                                    (case [incrementing? single-suit?]
                                      [true false] [:straight v]
                                      [false true] [:flush v]
                                      [true true]  [(if ace?
                                                      :royal-flush
                                                      :straight-flush)
                                                    v]
                                      [k v])))
        upgrade->advanced-pairs ((fn [coll]
                                   (reduce-kv
                                    (fn [m k v]
                                      (let [[new-k new-v]
                                            (case [k (count v)]
                                              [:pair 2]      [:two-pair (flat-once v)]
                                              [:high-card 5] (upgrade->high-cards k (flat-once v))
                                              [k (flat-once v)])]
                                        (assoc m new-k new-v)))
                                    (empty coll) coll))
                                  pair-and-cards)
        upgrade->full-house     (fn [{:keys [pair three-of-kind] :as all}]
                                  (if (every? some? [pair three-of-kind])
                                    {:full-house (mapcat identity (vals all))}
                                    all))]
    (upgrade->full-house upgrade->advanced-pairs)))

(defn rare-hand? [hand]
  (let [ranks (-> (calculate-hand hand)
                  (dissoc :high-card)
                  (dissoc :pair))]
    (not-empty ranks)))

(defn pick-good-cards [hand]
  (let [score      (calculate-hand hand)
        good-cards (mapcat identity (vals (dissoc score :high-card)))]
    (seq good-cards)))

(defn better-hand? [better-hand worse-hand]
  (if (and
       (= 0 (count (keys better-hand)))
       (= 0 (count (keys worse-hand))))
    (do
      (prn "Tie!")
      :tie)
    (if (= 0 (count (keys better-hand)))
      (do
        (prn "Opponent won with something:" worse-hand)
        :no)
      (let [better-rankings          (sort (map #(.indexOf hand-rankings %) (keys better-hand)))
            worse-rankings           (sort (map #(.indexOf hand-rankings %) (keys worse-hand)))
            rank-key                 (fn [rank-index]
                                       (get hand-rankings rank-index))
            highest-rank             (fn [hand rank-index]
                                       (let [cards (get hand (rank-key rank-index))
                                             ranks (flatten (sort-ranks (map second cards)))]
                                         (last ranks)))
            [best-better best-worse] [(last better-rankings) (last worse-rankings)]]
        (cond
          (> best-better best-worse) (do
                                       (prn "Player won with bigger ranking")
                                       :yes)
          (= best-better best-worse) (do
                                       (let [better-high (.indexOf ranks (highest-rank better-hand best-better))
                                             worse-high  (.indexOf ranks (highest-rank worse-hand best-worse))]
                                         (cond
                                           (> better-high worse-high) (do
                                                                        (prn "Player won with bigger high")
                                                                        :yes)
                                           (= better-high worse-high) (do
                                                                        (better-hand? (dissoc better-hand (rank-key best-better))
                                                                                      (dissoc worse-hand (rank-key best-worse))))
                                           :else                      (do
                                                                        (prn "Opponent won with bigger high")
                                                                        :no))))
          :else                      (do
                                       (prn "Opponent won with better rank")
                                       :no))))))

