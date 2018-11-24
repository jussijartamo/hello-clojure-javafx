(ns app.core
  (:gen-class
    :extends javafx.application.Application)

  (:require
    [app.util :refer [fx]]
    [app.game :as game]
    [ns-tracker.core :refer [ns-tracker]]
    [clojure-watch.core :refer [start-watch]])

  (:import javafx.application.Application
           javafx.application.Platform
           javafx.stage.Stage))

(defn- reloader []
  (let [modified-namespaces (ns-tracker ["src"])
        load-queue          (java.util.concurrent.LinkedBlockingQueue.)]
    (fn []
      (locking load-queue
        (doseq [ns-sym (modified-namespaces)]
          (.offer load-queue ns-sym))
        (loop []
          (when-let [ns-sym (.peek load-queue)]
            (try
              (require
               ns-sym
               :reload)
              (.remove load-queue)
              (catch Throwable t (prn t)))
            (recur)))))))

(defn -start [this stage]
  (let [reload!    (reloader)
        update-fn  (fn [& _]
                     (reload!)
                     (try
                       (let [new-scene (game/scene)]
                         (Platform/runLater (fn [] (.setScene stage new-scene))))
                       (catch Throwable t (prn t))))]
    (doto stage
          (.setOnCloseRequest (fx [_] (System/exit 0)))
          (.setScene (game/scene))
          (.setTitle "Hello Clojure JavaFX")
          (.show))
    (start-watch
     [{:path        "src"
       :event-types [:create :modify :delete]
       :callback    update-fn
       :options     {:recursive true}}])))

(defn -main [& args]
  (Application/launch app.core args))