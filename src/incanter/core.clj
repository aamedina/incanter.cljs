(ns incanter.core)

(defmacro with-data
  [data-binding & body]
  `(binding [$data ~data-binding]
     (do ~@body)))
