(ns app.gfx.poker
  (:use [app.util]
        [app.gfx.anims])

  (:import javafx.application.Application
           javafx.application.Platform
           [javafx.event ActionEvent]
           javafx.scene.Scene
           javafx.scene.paint.Color
           javafx.scene.shape.Rectangle
           javafx.scene.shape.Circle
           javafx.scene.control.Button
           javafx.scene.text.Text
           javafx.scene.effect.Glow
           javafx.scene.effect.DropShadow
           javafx.scene.effect.BlurType
           javafx.scene.effect.BoxBlur
           javafx.scene.effect.GaussianBlur
           javafx.scene.layout.StackPane
           javafx.scene.layout.HBox
           javafx.scene.Group
           javafx.geometry.Pos
           javafx.scene.text.TextAlignment
           javafx.geometry.VPos
           javafx.scene.layout.VBox
           javafx.scene.layout.BorderPane
           javafx.stage.Stage

           javafx.animation.SequentialTransition
           javafx.animation.TranslateTransition
           javafx.animation.FadeTransition
           javafx.animation.KeyFrame
           javafx.util.Duration))

(def suit->color
  {:spade   :black
   :heart   :red
   :diamond :red
   :club    :black})
(def suit->symbol
  {:spade   "♠"
   :heart   "♥"
   :diamond "♦"
   :club    "♣"})

(defn h-box [nodes]
  (doto (HBox.)
        (.setSpacing 5)
        (.setAlignment (Pos/CENTER))
        (children nodes)))

(defn in?
  [coll elm]
  (some
   (fn [v]
     (= 0 (compare elm v)))
   coll))

(defn toggle_selected-card! [state path value]
  (upsert state :selected-cards
          (fn [v]
            (if (in? v value)
              (remove (fn [ef] (= 0 (compare ef value))) v)
              (conj v value)))))

(defn card-node [[suit rank :as card-from-deck] selected-cards selectable?]
  (let [selected?          (in? selected-cards card-from-deck)
        red?               (= :red (suit->color suit))
        cardboard          (doto (Rectangle. 100 100 100 100)
                                 (css-class
                                   ["cardboard"
                                    (if red?
                                      "cardboard-red"
                                      "cardboard-black")]))
        text               (h-box
                            [(doto (VBox.)
                                   (children
                                    [(doto (StackPane.)
                                           (children
                                             [(doto (Text.)
                                                    (.setTextAlignment (TextAlignment/CENTER))
                                                    (.setTextOrigin (VPos/CENTER))
                                                    (css-class "cardboard-text")
                                                    (.setText (clojure.string/capitalize (name rank))))]))
                                     (doto (StackPane.)
                                           (children
                                             [(doto (Text.)
                                                    (.setTextAlignment (TextAlignment/CENTER))
                                                    (.setTextOrigin (VPos/CENTER))
                                                    (css-class "cardboard-symbol")
                                                    (.setText (suit->symbol suit)))]))]))])
        selected-icon      (doto (Text.)
                                 (css-class "cardboard-selected")
                                 (.setText (when selected? "✔")))

        hbox               (doto (BorderPane.)
                                 (.setTop text)
                                 (.setBottom (h-box [selected-icon])))

        card               (doto (StackPane.)
                                 (children [cardboard hbox]))]
    [cardboard card]))

(defn create-card [[suit rank :as card-from-deck] state]
  (let [selected?                      (in? (:selected-cards @state) card-from-deck)
        [cardboard card]               (card-node card-from-deck (:selected-cards @state) true)]
    (doto card
          (css-class "card-selectable")
          (.setOnMouseClicked
            (fx [_]
                (toggle_selected-card! state [:selected-cards] card-from-deck))))))

(defn poker-chip [value]
  (let [gr           (Group.)
        radius       30.0
        r2x          (* 2 radius)
        r-neg        (- radius)
        r2x-neg      (* 0.5 r-neg)
        inner-circle (doto (Circle. (* 0.75 radius))
                           (css-class (str "poker-chip-" value)))
        inner-ring   (doto (Circle. (* 1.15 radius))
                           (css-class (str "poker-chip-" value)))
        dash         9.0
        t            (doto (Text. (str "$" value))
                           (.setTextAlignment (TextAlignment/CENTER))
                           (.setTextOrigin (VPos/CENTER))
                           (.setFill (Color/WHITE))
                           (css-class "poker-chip-text"))
        c            (doto (Rectangle. r-neg r-neg r2x r2x)
                           (css-class (str "poker-chip-" value))
                           (.setStroke (Color/WHITE))
                           (.setStrokeWidth 4)
                           (.setArcWidth r2x)
                           (.setArcHeight r2x))]
    (.addAll (.getStrokeDashArray c) [dash (* 2 dash)])
    (doto gr (children [inner-ring c inner-circle]))
    (doto (StackPane.)
          (css-class "poker-chip")
          (children [gr t]))))

(defn action-bar-button [text]
  (let [b (doto (Button.)
                (.setMinHeight 40)
                (.setMinWidth 100)
                (css-class "action-button")
                (.setText text))]
    b))


(defn pile-of-chips [chips-in-pot]
  (let [area     50
        r-offset (fn []
                   (- area (rand-int (* 2 area))))
        chips    (map
                  (fn [value]
                    (doto (poker-chip value)
                          (.setTranslateY (r-offset))
                          (.setTranslateX (r-offset))
                          (fly-in-node)))
                  chips-in-pot)]
    (doto (Group.)
          (.setPickOnBounds false)
          (children chips))))

(defn display-banner [initial-text]
  (let [height   55
        width    175
        blur-len 5
        text     (Text.)
        r        (doto (StackPane.)
                       (.setPickOnBounds false)
                       (children
                        [(doto (Rectangle. width height width height)
                               (.setPickOnBounds false)
                               (css-class "display-banner-rectangle")
                               (.setEffect (GaussianBlur.)))
                         (doto text
                               (.setText initial-text)
                               (.setPickOnBounds false)
                               (css-class "display-banner-text"))]))]
    [text r]))

(defn vertical-layout [components]
  (doto (VBox.)
        (.setSpacing 5)
        (children (map h-box components))))