(ns app.game
  (:use [app.util])

  (:require
    [app.deck :refer [create-deck sort-deck calculate-hand ]])

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
           javafx.scene.effect.BlurType
           javafx.scene.effect.BoxBlur
           javafx.scene.layout.StackPane
           javafx.scene.layout.HBox
           javafx.geometry.Pos
           javafx.scene.layout.VBox
           javafx.scene.layout.BorderPane
           javafx.stage.Stage))


(def shine (Glow. 1))

(def selected-effect {:red (DropShadow. (BlurType/GAUSSIAN) (Color/RED) 12.0 0.8 0 0)
                      :black (DropShadow. (BlurType/GAUSSIAN) (Color/BLACK) 12.0 0.8 0 0)})
;(def suits [[:spade "♠" :black] [:heart "♥" :red] [:diamond "♦" :red] [:club "♣" :black]])
(def suit->color {:spade :black
                  :heart :red
                  :diamond :red
                  :club :black})
(def suit->symbol {:spade "♠"
                  :heart "♥"
                  :diamond "♦"
                  :club "♣"})
(defn children [node childrens]
  (.addAll (.getChildren node) childrens))

(defn h-box [nodes]
  (doto (HBox.)
        (.setSpacing 5)
        (.setAlignment (Pos/CENTER))
        (children nodes)))

(def selected-cards (atom nil))

(defn in?
  [coll elm]
  (some
   (fn [v]
     (= 0 (compare elm v)))
   coll))

(defn set_v! [state path value]
  (swap! state #(assoc-in % path value)))

(defn join_v! [state path value]
  (swap! state #(update-in % path (fn [v] (conj v value)))))

(defn toggle_v! [state path value]
  (swap! state #(update-in % path (fn [v]
                                    (if (in? v value)
                                      (remove (fn [ef] (= 0 (compare ef value))) v)
                                      (conj v value))))))

(defn create-card [[suit rank :as card-from-deck] state]
  (let [selected? (in? (:selected-cards @state) card-from-deck)
        cardboard (doto (Rectangle. 100 100 100 100)
                        (.setArcWidth 15)
                        (.setArcHeight 15)

                        (.setFill
                          (if (= :red (suit->color suit))
                            (Color/RED)
                            (Color/BLACK))))

        text      (doto (Text.)
                        (.setFill (Color/WHITE))
                        (.setStyle "-fx-font: 22 arial;")
                        (.setText (str (suit->symbol suit) " " (name rank))))
        text2      (doto (Text.)
                        (.setFill (Color/WHITE))
                        (.setStyle "-fx-font: 22 arial;
                                    -fx-line-spacing: 1em;")
                        (.setText (when selected? "✔")))

        hbox (doto (BorderPane.)
                   (.setCenter text)
              (.setBottom (h-box [text2])))

        card      (doto (StackPane.)
                        (.setEffect (when selected?
                                      (get selected-effect (suit->color suit)))))]
    (doto card
          (.setOnMouseClicked
            (fx [_]
                (prn "click")
                (toggle_v! state [:selected-cards] card-from-deck)))
          (.setOnMouseEntered
            (fx [_]
                (when-not selected?
                  (.setEffect card shine))))
          (.setOnMouseExited
            (fx [_]
                (when-not selected?
                  (.setEffect card nil))))
          (children [cardboard hbox]))))

(defn root [state]
  (let [deck (:deck @state)
        hand (sort-deck (take 5 deck))

        hbox (h-box (map #(create-card % state) hand))
        hbox2 (h-box [(doto (Button.)
                            (.setOnAction (fx [_]
                                              (prn (calculate-hand [[:spade :2] [:diamond :2]
                                                                    [:spade 6] [:club 6] [:diamond 6]]))
                                              (-> state
                                                  (override :deck (shuffle (create-deck))
                                                            :selected-cards nil))))
                            (.setText (str "dfsdf")))])
        vbox (doto (VBox.)
                   (.setSpacing 10)
                   (children [hbox2 hbox]))]
    (doto (BorderPane.)
          (.setBottom vbox))))

(defn scene []
  (let [state (atom
                {:deck           (shuffle (create-deck))
                 :selected-cards nil})
        scene (Scene. (root state) 700 420)]
    (add-watch state nil
               (fn [& _]
                 (Platform/runLater
                   (fn []
                     (.setRoot scene (root state))))))
    scene))
