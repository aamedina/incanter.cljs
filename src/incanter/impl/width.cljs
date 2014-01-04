(ns incanter.impl.width
  (:require [clojure.string :as str]))

(declare get-initial-widths max-width-per-field actual-width
         auto-resize-widths)

(def ^:dynamic *width* (delay 80))
                                        ; " | " and "-+-" are inner borders
(def inner-border-length 3)
                                        ; "+-" and "-+" are outer borders
(def outer-border-length 2)

(defn get-widths [all-rows]
  (-> all-rows get-initial-widths vec auto-resize-widths))

(defn auto-resize-widths [widths]
  (loop [new-widths []
         widths widths
         field-count (count widths)
         max-width @*width*]
    (if (empty? widths)
      new-widths
      (let [width (first widths)
            width-per-field (max-width-per-field max-width field-count)
            new-width (if (< width width-per-field) width width-per-field)]
        (recur
         (conj new-widths new-width)
         (rest widths)
         (- field-count 1)
         (- max-width (+ new-width inner-border-length)))))))

(defn get-initial-widths [all-rows]
  (map
   (fn [idx]
     (apply max (map #(count (str (nth % idx))) all-rows)))
   (range (count (first all-rows)))))

(defn max-width-per-field [current-width field-count]
  (quot (actual-width current-width field-count) field-count))

(defn actual-width [current-width field-count]
  (- current-width (+ (* 2 outer-border-length)
                      (* (dec field-count) inner-border-length))))

(defn ensure-valid-width [arg]
  (if (integer? arg)
    (if (> arg 0) arg 80)
    arg))
