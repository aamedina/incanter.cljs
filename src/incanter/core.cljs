(ns incanter.core
    (:require [clojure.browser.repl]
              [catvec.core :refer [catvec]]
              [incanter.impl.matrix :as mat
               :refer [matrix? matrix to-matrix-2d]])
    (:require-macros [incanter.core :refer [with-data]])
    (:import [goog.math Matrix]))

(enable-console-print!)

(def ^:dynamic $data nil)

(defrecord Dataset [column-names rows])

(defn ^boolean dataset? [obj] (instance? Dataset obj))

(defn nrow
  [mat]
  (mat/-nrows mat))

(defn ncol
  [mat]
  (mat/-ncols mat))

(defn ^clj dim
  [mat]
  [(nrow mat) (ncol mat)])

(defn identity-matrix
  [n]
  (matrix (Matrix/createIdentityMatrix n)))

(defn diag
  [mat]
  (let [n (apply min (dim mat))]
    (map #(get-in mat [% %]) (range n))))

(defn trans
  [mat]
  (matrix (.getTranspose (to-matrix-2d mat))))

(defn except-for
  [n exceptions]
  (let [except (if (coll? exceptions) exceptions [exceptions])]
    (for [i (range n) :when (reduce #(and %1 %2) (map #(not= i %) except))]
      i)))

(declare sel)

(defprotocol ISel
  (-sel [_ args] [_ rows cols]))

(extend-protocol ISel
  nil
  (-sel [_ _] [])
  (-sel [_ _ _] [])
  
  default
  (-sel [lst {:keys [rows cols except-rows except-cols filter-fn all]}]
    (when (satisfies? ISequential lst)
      (let [rows (cond
                   rows rows
                   except-rows (except-for (nrow lst) except-rows)
                   :else true)
            cols (cond
                   cols cols
                   except-rows (except-for (nrow (first lst)) except-cols))
            lst (if (nil? filter-fn) lst (filter filter-fn lst))
            all-rows? (or (true? rows) (= rows :all) all)
            all-cols? (or (true? cols) (= cols :all) (= all :all))]
        (cond
          (and (number? rows) (number? cols)) (nth (nth lst rows) cols)
          (and all-rows? (coll? cols))
          (map (fn [r] (map #(nth r %) cols)) lst)
          (and all-rows? (number? cols)) (map #(nth % cols) lst)
          (and (coll? rows) (number? cols)) (map #(nth % cols)
                                                 (map #(nth lst %) rows))
          (and (coll? rows) all-cols?) (map #(nth lst %) rows)
          (and (number? rows) all-cols?) (nth lst rows)
          (and (number? rows) (coll? cols)) (map #(nth (nth lst rows) %) cols)
          (and (coll? rows) (coll? cols)) (map (fn [r] (map #(nth r %) cols))
                                               (map #(nth lst %) rows))
          (and all-rows? all-cols?) lst))))
  (-sel [lst rows cols]
    (when (satisfies? ISequential lst)
      (sel lst :rows rows :cols cols)))

  Matrix
  (-sel [mat {:keys [rows cols except-rows except-cols filter-fn all]}]
    (let [rows (cond
                 rows rows
                 except-rows (except-for (nrow mat) except-rows)
                 :else true)
          cols (cond
                 cols cols
                 except-rows (except-for (nrow (first mat)) except-cols)
                 all all
                 :else true)
          mat (if (nil? filter-fn) mat (filter filter-fn mat))
          all-rows? (or (true? rows) (= rows :all) all)
          all-cols? (or (true? cols) (= cols :all) (= all :all))]
      (cond
        (and (number? rows) (number? cols)) (get-in mat [rows cols])
        (and all-rows? (coll? cols)) (get-in mat [rows cols])
        (and all-rows? (number? cols)) (get-in mat [rows cols])
        (and (coll? rows) (number? cols)) (get-in mat [rows cols])
        (and (coll? rows) all-cols?) (get-in mat [rows cols])
        (and (number? rows) all-cols?) (get-in mat [rows cols])
        (and (number? rows) (coll? cols)) (get-in mat [rows cols])
        (and (coll? rows) (coll? cols)) (get-in mat [rows cols])
        (and all-rows? all-cols?) mat)))
  (-sel [mat rows cols]
    (let [all-rows? (or (true? rows) (= rows :all))
          all-cols? (or (true? cols) (= cols :all))]
      (cond
        (and (number? rows) (number? cols)) (get-in mat [rows cols])
        (and all-rows? (coll? cols)) (get-in mat (range (nrow mat)) cols)
        (and (coll? rows) all-cols?) (get-in mat rows (range (ncol mat)))
        (and (coll? rows) (coll? cols)) (get-in mat [rows cols])
        (and all-rows? all-cols?) mat))))

(defn sel
  [mat & options]
  (if (keyword? (first options))
    (-sel mat options)
    (let [[rows cols] options]
      (-sel mat rows cols))))

(defn bind-rows
  [& args]
  (reduce
   (fn [A B]
     (cond
       (nil? (seq A))
       B
       (nil? (seq B))
       A
       (or (coll? A) (coll? B))
       (conj (if (or (matrix? A) (matrix? (first A)))
               A
               (matrix A (count A)))
             (if (or (matrix? B) (matrix? (first B)))
               B
               (matrix B (count B))))
       :else
       (throw (js/Error. "Incompatible types"))))
   args))

(defn bind-columns
  [& args]
  (reduce
   (fn [A B]
     (cond
       (nil? (seq A))
       B
       (nil? (seq B))
       A
       (or (coll? A) (coll? B))
       (.appendColumns
        (clone (if (or (matrix? A) (matrix? (first A)))
                 A
                 (matrix A (count A))))
        (if (or (matrix? B) (matrix? (first B)))
          B
          (matrix B (count B))))
       :else
       (throw (js/Error. "Incompatible types"))))
   args))

(defn ^:export -main [& args]
  (println "Hello, world!"))

(def A (matrix [[1 2 3]
                [4 5 6]
                [7 8 9]]))

(def B (matrix [10 11 12]))

(def test-mat (matrix (for [row (range 10)] (range 10))))
