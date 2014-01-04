(ns incanter.macros)

(defmacro with-data
  [data-binding & body]
  `(binding [$data ~data-binding]
     (do ~@body)))

(defmacro transform-with
  [A op fun]
  `(cond
     (incanter.impl.matrix/matrix-like? ~A)
     (incanter.impl.matrix/matrix
      (goog.math.Matrix/map (incanter.impl.matrix/to-matrix-2d ~A)
                            (fn [n#] (~fun n#))))
     (coll? ~A) (map ~op ~A)
     (number? ~A) (~op ~A)))

(defmacro $fn
  [col-bindings body]
  `(fn [{:keys ~col-bindings}] ~body))
