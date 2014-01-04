(ns incanter.impl.matrix
  (:require [clojure.set :as set]
            [goog.math :as math]
            [goog.vec :as gvec]
            [goog.math.tdma :as tdma]
            [goog.math.Matrix :as mat])
  (:require-macros [incanter.core :refer [with-data]])
  (:import [goog.vec Float32Array Float64Array Ray Mat3 Mat4 Matrix3 Matrix4]
           [goog.vec Vec2 Vec3 Vec4]
           [goog.math Bezier Box Coordinate Coordinate3 ExponentialBackoff]
           [goog.math Integer Line Long Range RangeSet Size]
           [goog.math.interpolator Interpolator1 Linear1 Pchip1 Spline1]))

(enable-console-print!)

(defprotocol IMatrixLike
  (^clj -matrix [_] [_ ncol] [_ rows cols])
  (^number -nrows [_])
  (^number -ncols [_])
  (^clj -dims [_]))

(declare matrix? matrix valid-matrix?)

(deftype Matrix [meta mat nrows ncols ^:mutable __hash]
  ISequential

  Object
  (toString [_] (pr-str* mat))

  ICloneable
  (-clone [_] (Matrix. meta mat nrows ncols __hash))
  
  IWithMeta
  (-with-meta [_ meta] (Matrix. meta mat nrows ncols __hash))
  
  IMeta
  (-meta [_] meta)

  IEquiv
  (-equiv [mat1 mat2]
    (cond
      (matrix? mat2) (= mat1 mat2)
      (vector? mat2) (= (.-mat mat1) mat2)
      (coll? mat2) (and (= (count (flatten mat1))
                           (count (flatten mat2)))
                        (every? true? (map #(== %1 %2)
                                           (flatten mat1)
                                           (flatten mat2))))
      :else (= mat1 mat2)))

  ISeq
  (-first [mat] (first (seq mat)))
  (-rest [_] (rest (seq mat)))

  ISeqable
  (-seq [_] (seq mat))

  INext
  (-next [_] (next (seq mat)))

  IEmptyableCollection
  (-empty [_] (matrix []))
  
  IReversible
  (-rseq [_] (rseq mat))

  ICollection
  (-conj [_ x] (conj mat x))

  ICounted
  (-count [_] nrows)

  IIndexed
  (-nth [_ n] (-nth mat n))
  (-nth [_ n not-found] (-nth mat n not-found))

  ILookup
  (-lookup [mat k] (-nth mat k))
  (-lookup [mat k not-found] (or (-nth mat k) not-found))

  IAssociative
  (-assoc [_ k v]
    (Matrix. meta (-assoc mat k v) nrows ncols __hash))

  IMatrixLike
  (-matrix [mat] mat)
  (-ncols [_] ncols)
  (-nrows [_] nrows)
  (-dims [_] [nrows ncols])

  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (pr-writer mat writer opts))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash)))

(defn to-matrix-2d
  [coll]
  (cond
    (instance? Matrix coll) (recur (.-mat coll))
    (and (array? coll) (mat/isValidArray coll)) (math/Matrix. coll)    
    (and (vector? coll) (valid-matrix? coll)) (recur (to-array-2d coll))
    (and (coll? coll) (valid-matrix? coll)) (recur (to-array-2d coll))
    :else (throw (js/Error. "Input must be coercible to matrix"))))

(defn ^clj to-vector-2d
  [coll]
  (reduce-kv (fn [acc idx row]
               (cond
                 (array? row)
                 (conj acc (cljs.core.PersistentVector/fromArray row))
                 (vector? row) (conj acc row)
                 (coll? row) (recur acc idx (to-array-2d row))
                 :else
                 (throw (js/Error. "All elements must be seqable"))))
             [] (if (array? coll)
                  (cljs.core.PersistentVector/fromArray coll)
                  (cljs.core.PersistentVector/fromArray (to-array-2d coll)))))

(extend-protocol IMatrixLike
  nil
  (-matrix [_] nil)
  
  default
  (-matrix [iseq]
    (if (sequential? iseq)
      (-matrix (to-vector-2d iseq))
      (throw (js/Error. "Invalid sequence for matrix."))))
  
  array
  (-matrix [arr]
    (if (mat/isValidArray arr)
      (let [mat (to-vector-2d arr)]
        (Matrix. nil mat (-nrows arr) (-ncols arr) nil))
      (throw (js/Error. "Invalid array for matrix."))))
  (-nrows [arr] (alength arr))
  (-ncols [arr] (alength (aget arr 0)))
  
  math/Matrix
  (-matrix [mat]
    (let [arr (.toArray mat)]
      (Matrix. nil (to-vector-2d arr) (-nrows mat) (-ncols mat) nil)))
  (-nrows [mat] (.-height (.getSize mat)))
  (-ncols [mat] (.-width (.getSize mat)))

  PersistentVector
  (-matrix [mat]
    (if (and (some vector? mat) (valid-matrix? mat))
      (let [arr (to-array-2d mat)]
        (Matrix. nil (to-vector-2d arr) (-nrows arr) (-ncols arr) nil))
      (if (every? number? mat)
        (-matrix [mat])
        (throw (js/Error. "Invalid vector for matrix.")))))
  (-nrows [mat] (count mat))
  (-ncols [mat] (count (first mat))))

(defn ^boolean matrix? [obj] (instance? Matrix obj))

(defn ^boolean matrix-like? [obj] (satisfies? mat/IMatrixLike obj))

(defn dims
  [mat]
  (-dims mat))

(defn matrix
  ([data] (-matrix data))
  ([data ncol] (-matrix (partition ncol data)))
  ([init-val nrows ncols]
     (-> (math/Matrix. (mat/createZeroPaddedArray_ nrows ncols))
         (mat/map (fn [_] init-val))
         (-matrix))))

(defn ^boolean valid-matrix?
  [^not-native vec]
  (mat/isValidArray (to-array-2d vec)))
