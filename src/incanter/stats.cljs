(ns incanter.stats
  (:require [clojure.set :as set]
            [catvec.core :refer [catvec]]
            [incanter.impl.matrix :as mat
             :refer [matrix? matrix to-matrix-2d matrix-like?]]
            [goog.math :as math]
            [goog.math.tdma :as tdma]
            [incanter.impl.pprint :refer [print-table]]
            [incanter.core :refer
             [$ abs plus minus div mult mmult to-list bind-columns
              gamma pow sqrt diag trans regularized-beta ncol
              nrow identity-matrix decomp-cholesky decomp-svd
              matrix length log10 sum sum-of-squares sel
              cumulative-sum solve vectorize bind-rows]])
  (:require-macros [incanter.macros :refer [with-data transform-with $fn]])
  (:import [goog.math Matrix]))

(defn scalar-abs
  [x]
  (if (< x 0)
    (* -1 x)
    x))

(defn- deep-merge-with
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))


(defn indicator
  [pred coll]
  (let [pred-int (fn [pred el]
                   (if (pred el) 1 0))]
    (if (coll? coll)
      (map (partial pred-int pred) coll)
      (pred-int pred coll))))

(defn pdf-f
  ([x & {:keys [df1 df2] :or {df1 1 df2 1}}]
     (let [pdf-fx (fn [x]
                    (* (/ (gamma (/ (+ df1 df2) 2))
                          (* (gamma (/ df1 2)) (gamma (/ df2 2))))
                       (pow (/ df1 df2) (/ df1 2))
                       (pow x (dec (/ df1 2)))
                       (pow (inc (* (/ df1 df2) x))
                            (- 0 (/ (+ df1 df2) 2)))))]
       (if (coll? x)
         (map pdf-fx x)
         (pdf-fx x)))))

(def A (matrix [[1 2 3]
                [4 5 6]
                [7 8 9]]))

(def B (matrix [10 11 12]))

(def C (matrix [[2 0 0] [0 2 0] [0 0 2]]))
