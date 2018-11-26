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

(defn- vectorize [maybe-vec]
  (remove nil?
  (if (or
       (seq? maybe-vec)
       (vector? maybe-vec))
    maybe-vec
    [maybe-vec])))

(defn children [node childrens]
  (.addAll (.getChildren node) (vectorize childrens))
  node)

(defn set-children [node childrens]
  (.clear (.getChildren node))
  (children node childrens))

(defn css-class [node css-class]
  (.addAll (.getStyleClass node) (vectorize css-class))
  node)

(defn upsert [state path f]
  (swap! state
         #(if (keyword? path)
           (update % path f)
           (update-in % path f)))
  state)

(defn override
  ([state m]
   (swap! state
          (fn [old-state]
            (merge
             old-state
             m)))
   state))
