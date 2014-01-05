(ns incanter.charts
  (:require [goog.graphics :as gfx])
  (:import [goog.graphics Stroke SolidFill LinearGradient Font]
           [goog.graphics Path AffineTransform CanvasGraphics]))

(deftype Canvas [])

(defn canvas
  [w h]
  (CanvasGraphics. w h))
