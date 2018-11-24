(ns app.util)

(defn event-fx [f]
  (proxy [javafx.event.EventHandler] []
         (handle [event]
                 (f event))))

(defmacro fx
  [binding & body]
  `(event-fx
    (fn [& ~binding]
      ~@body)))


(defn override
  ([state path value & [a b & more]]
   (if (seq more)
     (-> state
         (override path value)
         (override a b more))
     (-> state
         (override path value)
         (override a b))))
  ([state path value]
   (if (keyword? path)
     (swap! state #(assoc % path value))
     (swap! state #(assoc-in % path value)))
   state))