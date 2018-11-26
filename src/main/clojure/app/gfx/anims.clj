(ns app.gfx.anims
  (:use [app.util])

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


(defn make-node-appear [node]
  (let [ft (doto (FadeTransition. (Duration/millis 500) node)
                 (.setFromValue 0.0)
                 (.setToValue 1.0)
                 (.setCycleCount 1)
                 (.setAutoReverse false))]
    (.play ft)
    node))

(defn fly-in-node [node]
  (let [ft (doto (TranslateTransition. (Duration/millis 500) node)
                 (.setFromX 0.0)
                 (.setToX (.getTranslateX node))
                 (.setFromY 0.0)
                 (.setToY (.getTranslateY node))
                 (.setCycleCount 1)
                 (.setAutoReverse false))
        st (doto (SequentialTransition.)
                 (children [ft])
                 (.setCycleCount 1)
                 (.setAutoReverse false))]
    (.play st)
    node))

(defn take-away-node [node on-finished]
  (let [ft (doto (TranslateTransition. (Duration/millis 100) node)
                 (.setFromX (.getTranslateX node))
                 (.setToX 0.0)
                 (.setFromY (.getTranslateY node))
                 (.setToY 500.0)
                 (.setCycleCount 1)
                 (.setAutoReverse false))
        st (doto (SequentialTransition.)
                 (children [ft])
                 (.setOnFinished on-finished)
                 (.setCycleCount 1)
                 (.setAutoReverse false))]
    (.play st)
    node))

(defn give-away-node [node on-finished]
  (let [ft (doto (TranslateTransition. (Duration/millis 100) node)
                 (.setFromX (.getTranslateX node))
                 (.setToX 0.0)
                 (.setFromY (.getTranslateY node))
                 (.setToY -500.0)
                 (.setCycleCount 1)
                 (.setAutoReverse false))
        st (doto (SequentialTransition.)
                 (children [ft])
                 (.setOnFinished on-finished)
                 (.setCycleCount 1)
                 (.setAutoReverse false))]
    (.play st)
    node))