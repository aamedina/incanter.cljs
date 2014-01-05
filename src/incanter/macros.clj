(ns incanter.macros)

(defmacro with-data
  [data-binding & body]
  `(binding [$data ~data-binding]
     (do ~@body)))

(defmacro transform-with
  [A op fun]
  `(cond
     (incanter.impl.matrix/matrix? ~A)
     (-> (incanter.impl.matrix/to-matrix-2d (incanter.impl.matrix/matrix ~A))
         (goog.math.Matrix/map (fn [n#] (~fun n#)))
         (incanter.impl.matrix/matrix))
     (and (coll? ~A) (coll? (first ~A)))
     (let [mA# (incanter.impl.matrix/matrix ~A)]
       (-> (incanter.impl.matrix/to-matrix-2d mA#)
           (goog.math.Matrix/map (fn [n#] (~fun n#)))
           (incanter.impl.matrix/matrix)))
     (coll? ~A) (map ~op ~A)
     (number? ~A) (~op ~A)))

(defmacro $fn
  [col-bindings body]
  `(fn [{:keys ~col-bindings}] ~body))
