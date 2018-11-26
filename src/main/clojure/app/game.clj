(ns app.game
  (:use [app.util]
        [app.state-machine]
        [app.gfx.poker]
        [app.gfx.anims]
        [app.deck])
  (:require [clojure.core.match :refer [match]])
  (:import javafx.application.Application
           javafx.application.Platform
           [javafx.event ActionEvent]
           javafx.scene.Scene
           javafx.scene.paint.Color
           javafx.scene.shape.Rectangle
           javafx.scene.control.Button
           javafx.scene.text.Text
           javafx.scene.effect.Glow
           javafx.scene.effect.DropShadow
           java.math.BigDecimal
           javafx.scene.effect.BlurType
           javafx.scene.effect.BoxBlur
           javafx.scene.layout.StackPane
           javafx.scene.layout.HBox
           javafx.geometry.Pos
           javafx.scene.layout.Region
           javafx.scene.Group
           javafx.scene.layout.VBox
           javafx.scene.layout.BorderPane
           javafx.stage.Stage))

(defn root [chip-controller {:keys [deck hand opponent-hand poker-state]} state]
  (prn "state " poker-state)
  (let [main-action                                         (clojure.string/capitalize (name poker-state))
        {:keys [chip-group all-in-pot? reset-chips]}        chip-controller
        {:keys [new-chip-pot]}                              chip-controller
        round-on?                                           (boolean
                                                             (or
                                                              (= :call poker-state)
                                                              (= :draw poker-state)))
        total-pot                                           (calculate-total-pot state)
        {:keys [pot min-raise]}                @state
        min-to-check (calculate-min-to-check state)
        score                                               (calculate-hand hand)
        opponent-selected                                   (pick-good-cards opponent-hand)
        opponent-cards                                      (doto
                                                             (h-box (map #(second (card-node % opponent-selected false)) opponent-hand)))
        cards                                               (h-box (map #(create-card % state) hand))
        [pot-text pot-display]                              (display-banner nil)
        [money-text money-display]                          (display-banner (str (:money @state) "$"))
        [opp-money-text opp-money-display]                  (display-banner (str (:opponent-money @state) "$"))
        pot-stash                                           (doto (StackPane.)
                                                                  (.setPickOnBounds false)
                                                                  (css-class "pot-stash")
                                                                  (children [@chip-group pot-display]))
        set-default-text                                    (fn []
                                                              (case poker-state
                                                                :tie     (.setText pot-text (str "Tie (your pot $" pot))
                                                                :victory (.setText pot-text (str "You won $" total-pot))
                                                                :loss    (.setText pot-text (str "You lost $" total-pot))
                                                                (.setText pot-text (str "Total Pot $" total-pot))))
        call-button                                         (action-bar-button main-action)
        toolbar                                             (vertical-layout
                                                             [money-display
                                                              [(when round-on?
                                                                 (doto
                                                                  (action-bar-button "🗙 Fold")
                                                                  (.setOnAction
                                                                    (fx [_]
                                                                        (-> state
                                                                        (override {:poker-state :fold})
                                                                        (initialize-round))
                                                                        (reset-chips)))))
                                                               (when round-on?
                                                                 (doto
                                                                  call-button
                                                                  (.setOnMouseEntered
                                                                    (fx [_]
                                                                        (when (= :call poker-state)
                                                                          (if (= "0" min-to-check)
                                                                            (.setText call-button (str "Check"))
                                                                            (.setText call-button (str "+$" min-to-check))))))
                                                                  (.setOnMouseExited
                                                                    (fx [_]
                                                                        (.setText call-button main-action)))
                                                                  (.setOnAction
                                                                    (fx [_]
                                                                        (case poker-state
                                                                          :draw (draw state)
                                                                          :call (call state))))))]])

        vbox                                                (doto (VBox.)
                                                                  (.setSpacing 10)
                                                                  (children [toolbar cards]))
        resolve-round                                       (fx [_]
                                                                (prn "resolving round!")
                                                                (initialize-round state)
                                                                (reset-chips))]
    (doto @chip-group
          (.setPickOnBounds true)
          (.setOnMouseClicked
            (fx [_]
                (match [round-on? poker-state]
                       [true _]
                       (raise state)

                       [_ :victory]
                       (take-away-node @chip-group resolve-round)

                       [false _]
                       (give-away-node @chip-group resolve-round))))
          (.setOnMouseEntered
            (fx [_]
                (when round-on?
                  (let [[operator sum] ["Raise +$" (big+ min-raise min-to-check)]]
                    (.setText pot-text (str operator sum))))))
          (.setOnMouseExited
            (fx [_]
                (set-default-text))))
    (.setTranslateY pot-display 100)
    (set-default-text)
    (doto (BorderPane.)
          (.setCenter pot-stash)
          (.setTop (vertical-layout [opponent-cards opp-money-display]))
          (.setBottom vbox))))

(defn chip-pot-controller [state]
  (let [chip-group   (atom nil)
        chips-in-pot (atom nil)
        reset-fn     (fn []
                       (let [total-pot (calculate-total-pot state)
                             hot-spot-size 300]
                         (reset! chips-in-pot total-pot)
                         (reset! chip-group
                                 (doto (StackPane.)
                                       (.setMaxWidth hot-spot-size)
                                       (.setMaxHeight hot-spot-size)
                                       (.setMinWidth hot-spot-size)
                                       (.setMinHeight hot-spot-size)
                                       (.setMaxSize (Region/USE_PREF_SIZE) (Region/USE_PREF_SIZE))
                                       (children [(pile-of-chips (split->chips total-pot))])
                                       ))))]
    (reset-fn)
    {:chip-group   chip-group
     :all-in-pot?  (fn []
                     (let [total-pot (calculate-total-pot state)]
                       (= total-pot @chips-in-pot)))
     :new-chip-pot (fn []
                     (let [total-pot (calculate-total-pot state)
                           new-chips (str (- (big total-pot) (big @chips-in-pot)))]
                       (reset! chips-in-pot total-pot)
                       new-chips))
     :reset-chips  reset-fn}))

(defn
  scene []
  (let [state           (initialize (atom {}))
        chip-controller (chip-pot-controller state)
        new-root        (fn [] (root chip-controller @state state))
        scene           (Scene. (new-root) 900 720)]
    (.add (.getStylesheets scene) "app/poker.css")
    (add-watch state nil
               (fn [& _]
                 (Platform/runLater
                  (fn []
                    (let [{:keys [new-chip-pot all-in-pot? chip-group]} chip-controller]
                    (if-not (all-in-pot?)
                      (doto @chip-group
                            (children
                             (pile-of-chips
                              (split->chips
                               (new-chip-pot)))))))
                    (.setRoot scene (new-root))))))
    scene))

