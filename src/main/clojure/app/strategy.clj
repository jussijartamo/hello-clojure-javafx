(ns app.strategy
  (:use [app.util]
        [app.deck]
        ;[app.state-machine]
        ))

(defn fearless-strategy [hand]
  (if-let [rare? (rare-hand? hand)]
    (do
      (prn "fearless opponent is having rare hand! Raise if possible! " (keys rare?))
      :raise)
    (do
      (prn "fearless opponent is having weak hand! call if possible!")
      :call)))