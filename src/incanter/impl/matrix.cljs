(ns incanter.impl.matrix
  (:require [clojure.set :as set]
            [goog.math :as math]
            [goog.vec :as gvec]
            [goog.math.tdma :as tdma]
            [goog.math.Matrix :as mat]
            [catvec.core :refer [catvec Catvec]])
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

(declare matrix? matrix valid-matrix? to-matrix-2d)

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
      (matrix? mat2) (.equals (to-matrix-2d mat1) (to-matrix-2d mat2))
      (vector? mat2) (= mat mat2)
      (coll? mat2) (and (= (count (flatten mat1))
                           (count (flatten mat2)))
                        (every? true? (map #(== %1 %2)
                                           (flatten mat1)
                                           (flatten mat2))))
      :else (= mat mat2)))

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
  (-lookup [mat k] (-nth mat k)
    ;; (cond
    ;;   (number? k) (-nth mat k)
    ;;   (coll? k) (mapv -nth (repeat mat) k)
    ;;   :else nil)
    )
  (-lookup [mat k not-found] (-nth mat k not-found)
    ;; (cond
    ;;   (number? k) (or (-nth mat k not-found) not-found)
    ;;   (coll? k) (mapv -nth (repeat mat) k (repeat not-found))
    ;;   :else not-found)
    )

  IAssociative
  (-assoc [_ k v]
    (Matrix. meta (-assoc mat k v) nrows ncols __hash))

  IMatrixLike
  (-matrix [mat] mat)
  (-ncols [_] ncols)
  (-nrows [_] nrows)
  (-dims [_] [nrows ncols])

  IKVReduce
  (-kv-reduce [coll f init]
    (-kv-reduce mat f init))

  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (pr-writer mat writer opts))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash)))

(defn rows
  [mat rows]
  (reduce conj [] (map #(nth mat %) rows)))

(defn cols
  [mat cols]
  (mapv (fn [row cols] (mapv #(nth row %) cols)) mat (repeat cols)))

(defn get-in-matrix
  [mat rs cs]
  (-> (rows mat rs)
      (cols cs)))

(defn to-matrix-2d
  [coll]
  (cond
    (instance? math/Matrix coll) coll
    (empty? coll) []
    (instance? Matrix coll) (recur (.-mat coll))
    (and (array? coll) (mat/isValidArray coll)) (math/Matrix. coll)    
    (satisfies? IMatrixLike coll) (recur (to-array-2d (-matrix coll)))
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
    (if (and (sequential? iseq) (valid-matrix? iseq))
      (if (every? number? iseq)
        (-matrix (mapv vector iseq))
        (-matrix (to-vector-2d iseq)))
      (throw (js/Error. "Invalid sequence for matrix."))))
  
  array
  (-matrix [arr]
    (if (mat/isValidArray arr)
      (let [mat (to-vector-2d arr)]
        (Matrix. nil mat (-nrows arr) (-ncols arr) nil))
      (if (every? number? arr)
        (-matrix (into-array (map array arr)))
        (throw (js/Error. "Invalid array for matrix.")))))
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
        (-matrix (mapv vector mat))
        (throw (js/Error. "Invalid vector for matrix.")))))
  (-nrows [mat] (count mat))
  (-ncols [mat] (count (first mat)))

  Subvec
  (-matrix [mat]
    (if (and (some vector? mat) (valid-matrix? mat))
      (let [arr (to-array-2d mat)]
        (Matrix. nil (to-vector-2d arr) (-nrows arr) (-ncols arr) nil))
      (if (every? number? mat)
        (-matrix (mapv vector mat))
        (throw (js/Error. "Invalid vector for matrix.")))))
  (-nrows [mat] (count mat))
  (-ncols [mat] (count (first mat)))

  Catvec
  (-matrix [mat]
    (if (and (some vector? mat) (valid-matrix? mat))
      (let [arr (to-array-2d mat)]
        (Matrix. nil (to-vector-2d arr) (-nrows arr) (-ncols arr) nil))
      (if (every? number? mat)
        (-matrix (mapv vector mat))
        (throw (js/Error. "Invalid vector for matrix.")))))
  (-nrows [mat] (count mat))
  (-ncols [mat] (count (first mat))))

(defn ^boolean matrix? [obj] (instance? Matrix obj))

(defn ^boolean matrix-like? [obj]
  (or (instance? math/Matrix obj)
      (and (satisfies? IMatrixLike obj) (valid-matrix? obj))))

(defn dims
  [mat]
  (-dims mat))

(defn matrix
  ([data]
     (if (and (coll? data) (empty? data))
       []
       (-matrix data)))
  ([data ncol] {:pre [(number? (first data))]}
     (-matrix (partition ncol data)))
  ([init-val nrows ncols]
     (-> (math/Matrix. (mat/createZeroPaddedArray_ nrows ncols))
         (mat/map (fn [_] init-val))
         (-matrix))))

(defn ^boolean valid-matrix?
  [^not-native vec]
  (and (coll? vec)
       (or (every? number? vec)
           (mat/isValidArray (to-array-2d vec)))))
