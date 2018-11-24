(ns app.deck
  (:require [clojure.pprint :refer [pprint]]))


(def suits [:spade :heart :diamond :club])
(def ranks [:2 :3 :4 :5 :6 :7 :8 :9 :10 :jack :queen :king :ace])


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
                                  (prn "-- upping 5 high cards --")
                                  (let [rank-of-cards  (map #(second (first %)) v)
                                        ace?           (seq (filter #(= :ace %) rank-of-cards))
                                        rank-values    (sort (map #(.indexOf ranks %) rank-of-cards))
                                        incrementing?  (apply = (map - rank-values (range)))
                                        suits-of-cards (frequencies (map ffirst v))
                                        single-suit?   (= 5 (count (keys suits-of-cards)))]
                                    (case [incrementing? single-suit?]
                                      [true false] :straight
                                      [false true] :flush
                                      [true true]  (if ace?
                                                     :royal-flush
                                                     :straight-flush)
                                      k)))
        upgrade->advanced-pairs ((fn [coll]
                                   (reduce-kv
                                    (fn [m k v]
                                      (let [new-k
                                            (case [k (count v)]
                                              [:pair 2]      :two-pairs
                                              [:high-card 5] (upgrade->high-cards k v)
                                              k)]
                                        (assoc m new-k v)))
                                    (empty coll) coll))
                                  pair-and-cards)
        upgrade->full-house     (fn [{:keys [pair three-of-kind] :as all}]
                                  (if (every? some? [pair three-of-kind])
                                    {:full-house (mapcat identity (vals all))}
                                    all))]


    (pprint (upgrade->full-house upgrade->advanced-pairs))))


