(ns incanter.core
  (:require [clojure.browser.repl]
            [catvec.core :refer [catvec]]
            [incanter.impl.matrix :as mat
             :refer [matrix? matrix to-matrix-2d matrix-like? get-in-matrix
                     rows cols acol arow swap-rows!]]
            [goog.math :as math]
            [goog.math.tdma :as tdma]
            [clojure.set :as set]
            [incanter.impl.pprint :refer [print-table]])
  (:require-macros [incanter.macros :refer [with-data transform-with $fn]])
  (:import [goog.math Matrix]))

(enable-console-print!)

(def ^:dynamic $data nil)

(defrecord Dataset [column-names rows]
  IPrintWithWriter
  (-pr-writer [ds writer opts]
    (print-table column-names rows)))

(defn ^boolean dataset? [obj] (instance? Dataset obj))

(extend-protocol mat/IMatrixLike
  Dataset
  (-nrows [ds] (count (:rows ds))))

(defn nrow
  [mat]
  (mat/-nrows mat))

(defn ncol
  [mat]
  (mat/-ncols mat))

(defn ^clj dim
  [mat]
  [(nrow mat) (ncol mat)])

(defn ^boolean row? [mat] (and (matrix? mat) (== 1 (first (dim mat)))))

(defn ^boolean column? [mat] (and (matrix? mat) (== 1 (second (dim mat)))))

(defn ^boolean square? [mat] (reduce == (dim mat)))

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

(defn mmap
  ([f mat] (Matrix/map (to-matrix-2d mat) (fn [n] (f n)))))

(declare sel except-for-cols get-column-id dataset col-names)

(defprotocol ISel
  (-sel [_ args] [_ rows cols]))

(extend-protocol ISel
  nil
  (-sel [_ _] [])
  (-sel [_ _ _] [])
  
  object
  (-sel [lst args]
    (when (satisfies? IList lst)
      (let [{:keys [rows cols except-rows except-cols filter-fn all]} args
            rows (cond
                   rows rows
                   except-rows (except-for (nrow lst) except-rows)
                   :else true)
            cols (cond
                   cols cols
                   except-cols (except-for (nrow (first lst)) except-cols)
                   all all
                   :else true)
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
    (when (satisfies? IList lst)
      (sel lst {:rows rows :cols cols})))

  mat/Matrix
  (-sel [mat args]
    (let [{:keys [rows cols except-rows except-cols filter-fn all]} args
          rows (cond
                 rows rows
                 except-rows (except-for (nrow mat) except-rows)
                 :else true)
          cols (cond
                 cols cols
                 except-cols (except-for (nrow (first mat)) except-cols)
                 all all
                 :else true)
          mat (if (nil? filter-fn) mat (filter filter-fn mat))
          all-rows? (or (true? rows) (= rows :all) all)
          all-cols? (or (true? cols) (= cols :all) (= all :all))]
      (cond
        (and (number? rows) (number? cols))
        (get-in-matrix mat rows cols)
        (and all-rows? (coll? cols))
        (get-in-matrix mat (range (nrow mat)) cols)
        (and all-rows? (number? cols))
        (get-in-matrix mat (range (nrow mat)) [cols])
        (and (coll? rows) (number? cols)) (get-in-matrix mat rows [cols])
        (and (coll? rows) all-cols?)
        (get-in-matrix mat rows (range (ncol mat)))
        (and (number? rows) all-cols?)
        (get-in-matrix mat rows (range (ncol mat)))
        (and (number? rows) all-cols?)
        (get-in-matrix mat [rows] (range (ncol mat)))
        (and (number? rows) (coll? cols))
        (get-in-matrix mat [rows] cols)
        (and (coll? rows) (coll? cols)) (get-in-matrix mat rows cols)
        (and all-rows? all-cols?) mat)))
  (-sel [mat rows columns]
    (let [rws (if (number? rows) [rows] rows)
          cols (if (number? columns) [columns] columns)
          all-rows? (or (true? rws) (= rws :all))
          all-cols? (or (true? cols) (= cols :all))]
      (cond
        (and (number? rows) (number? columns))
        (get-in-matrix mat rws cols)
        (and all-rows? (coll? cols))
        (get-in-matrix mat (range (nrow mat)) cols)
        (and (coll? rws) all-cols?)
        (get-in-matrix mat rws (range (ncol mat)))
        (and (coll? rws) (coll? cols))
        (get-in-matrix mat rws cols)
        (and all-rows? all-cols?) mat)))

  PersistentVector
  (-sel [mat args]
    (-sel (matrix mat) args))
  (-sel [mat rows columns]
    (-sel (matrix mat) rows columns))

  Matrix
  (-sel [mat args]
    (-sel (matrix mat) args))
  (-sel [mat rows columns]
    (-sel (matrix mat) rows columns))

  Dataset
  (-sel [data args]
    (let [{:keys [rows cols except-rows except-cols filter all]} args
          rows (cond
                 rows rows
                 except-rows (except-for (nrow data) except-rows)
                 :else true)
          cols (cond

                 cols cols
                 except-cols (except-for-cols data except-cols)
                 all all
                 :else true)
          colnames (:column-names data)
          selected-cols (cond
                          (or (= cols :all) (true? cols)) colnames
                          (coll? cols) (map #(get-column-id data %) cols)
                          :else [cols])
          selected-rows (cond
                          (or (= rows :all) (true? rows) all)
                          (:rows data)
                          (number? rows)
                          (list (nth (:rows data) rows))
                          (coll? rows)
                          (map #(nth (:rows data) %) rows))
          _data (map (fn [row] (map #(row (get-column-id data %))
                                    selected-cols)) selected-rows)
          result (if (nil? filter) _data (clojure.core/filter filter _data))]
      (cond
        (and (= (count selected-cols) 1) (not (or (coll? cols) (true? cols))))
        (if (= (count result) 1)
          (ffirst result)
          (mapcat identity result))
        (and (= (count result) 1) (not (or (coll? rows) (true? rows))))
        (first result)
        :else
        (dataset selected-cols
                 (map #(apply assoc {} (interleave selected-cols %))
                      result)))))
  (-sel [mat rows columns]
    (-sel (matrix mat) rows columns)))

(defn sel
  [mat & options]
  (if (keyword? (first options))
    (-sel mat (apply hash-map options))
    (let [[rows cols] options]
      (-sel mat rows cols))))

(defprotocol IListLike
  (-to-list [_]))

(extend-protocol IListLike
  mat/Matrix
  (-to-list [mat] (.-mat mat))
  
  Dataset
  (-to-list [ds]
    (map (fn [row]
           (map (fn [col] (row col)) (:column-names ds)))
         (:rows ds)))

  default
  (-to-list [s] s)

  nil
  (-to-list [s] nil))

(defn to-list
  [arg]
  (-to-list arg))


(defn map-matrix
  [f args]
  (reduce (fn [acc x]
            (cond
              (number? acc) (f acc x)
              (matrix? acc)
              (let [m (matrix (map-matrix f (list (to-list acc) x)))]
                (if (row? acc)
                  (trans m)
                  m))
              (dataset? acc) (dataset (col-names acc)
                                      (map-matrix f (list (to-list acc) x)))
              (and (coll? acc) (coll? (first acc)))
              (map (fn [a] (map #(f %1 x) a)) acc)
              (coll? acc) (map #(f %1 x) acc)))
          args))

(defn reduce-matrix
  ([f coll]
     (reduce-matrix f (first coll) (rest coll)))
  ([f init coll]
     (matrix (reduce f (to-matrix-2d init) (map to-matrix-2d coll)))))

(defn bind-rows
  [& args]
  (reduce-matrix #(.appendRows %1 %2) args))

(defn bind-columns
  [& args]
  (println args)
  (reduce-matrix #(.appendColumns %1 %2) args))

(defn plus
  [& args]
  (reduce (fn [acc x]
            (cond
              (and (matrix-like? acc) (matrix-like? x))
              (matrix (.add (to-matrix-2d acc) (to-matrix-2d x)))
              (and (matrix-like? acc) (number? x))
              (matrix (Matrix/map (to-matrix-2d acc) (fn [n] (+ n x))))
              (and (matrix-like? x) (number? acc))
              (recur x acc)
              (and (number? acc) (number? x))
              (* acc x)))
          args))

(defn split-by
  ([pred coll] ((juxt #(filter pred %) #(remove pred %)) coll))
  ([p1 p2 coll]
     ((juxt #(filter p1 %)
            #(filter p2 %)
            #(remove (some-fn p1 p2) %)) coll)))

(defn mult
  [& args]
  (reduce (fn [acc x]
            (cond
              (and (matrix-like? acc) (matrix-like? x)
                   (== (count acc) (count x)))
              (mapv mult acc x)
              (and (matrix-like? acc) (number? x))
              (matrix (Matrix/map (to-matrix-2d acc) (fn [n] (* n x))))
              (and (matrix-like? x) (number? acc))
              (recur x acc)
              (and (number? acc) (number? x))
              (* acc x)))
          args))

(defn mmult
  [& args]  
  (reduce-matrix #(.multiply %1 %2) args))

(defn kronecker
  [& args]
  (reduce (fn [A B]
            (let [a (cond
                      (matrix-like? A) A
                      (number? A) (matrix [A])
                      :else (matrix A))
                  b (cond
                      (matrix-like? B) B
                      (number? B) (matrix [B])
                      :else (matrix B))
                  rows (* (nrow a) (nrow b))
                  cols (* (ncol a) (ncol b))]
              (println a b rows cols)
              (apply bind-rows
                     (for [i (range (nrow a))]
                       (apply bind-columns
                              (for [j (range (ncol a))]
                                (mult (sel a i j) b)))))))
          args))


(defn div
  [& args]
  (if (= (count args) 1)
    (div 1 (first args))
    (reduce (fn [acc x]
              (cond
                (and (matrix-like? acc) (matrix-like? x))
                (reduce into [] (map div acc x))
                (and (matrix-like? acc) (number? x))
                (matrix (Matrix/map (to-matrix-2d acc) (fn [n] (/ n x))))
                (and (matrix-like? x) (number? acc))
                (recur x acc)
                (and (number? acc) (number? x))
                (/ acc x)))
            args)))

(defn pow
  [& args]
  (map-matrix #(Math/pow %1 %2) args))

(defn atan2
  [& args]
  (map-matrix #(Math/atan2 %1 %2) args))

(defn sqrt
  [A]
  (transform-with A #(Math/sqrt %) Math/sqrt))

(defn sq
  [A]
  (mult A A))

(defn log
  [A]
  (transform-with A #(Math/log %) Math/log))

(defn log2
  [A]
  (transform-with A #(/ (Math/log %) (Math/log 2))
                  #(/ (Math/log %) (Math/log 2))))

(defn log10
  [A]
  (transform-with A #(/ (Math/log %) (Math/log 10))
                  #(/ (Math/log %) (Math/log 10))))

(defn exp
  [A]
  (transform-with A #(Math/exp %) Math/exp))

(defn abs
  [A]
  (transform-with A #(Math/abs %) Math/abs))

(defn sin
  [A]
  (transform-with A #(Math/sin %) Math/sin))

(defn asin
  [A]
  (transform-with A #(Math/asin %) Math/asin))

(defn cos
  [A]
  (transform-with A #(Math/cos %) Math/cos))

(defn acos
  [A]
  (transform-with A #(Math/acos %) Math/acos))

(defn tan
  [A]
  (transform-with A #(Math/tan %) Math/tan))

(defn atan
  [A]
  (transform-with A #(Math/atan %) Math/atan))

(defn fac
  [n]
  (loop [n n acc 1]
    (if (== n 0)
      acc
      (recur (dec n) (* n acc)))))

(defn gamma
  [n]
  (fac (dec n)))

(defn beta
  [a b]
  (/ (* (gamma a) (gamma b))
     (+ (gamma (+ a b)))))

(defn incomplete-beta
  [a b]
  0)

(defn regularized-beta
  [a b]
  (/ (incomplete-beta a b)
     (beta a b)))

(defn factorial
  [A]
  (transform-with A fac fac))

(defn choose
  [n k]
  (-> (fac n)
      (/ (* (fac k)
            (fac (- n k))))
      Math/round))

(defn det
  [mat]
  (.getDeterminant (to-matrix-2d mat)))

(declare lu-solve decomp-lu)

(defn solve
  ([A] (when-let [inverted (.getInverse (to-matrix-2d A))] (matrix inverted)))
  ([A B]
     (lu-solve (decomp-lu A) B)))

(defn submatrix
  ([mat] (submatrix mat 0 0 (dec (ncol mat)) (dec (nrow mat))))
  ([mat x1 y1 x2 y2]
     (-> (to-matrix-2d mat)
         (.getSubmatrixByCoordinates_ x1 y1 x2 y2))))

(defn vectorize
  [mat]
  (reduce catvec (trans mat)))

(defn half-vectorize
  [mat]
  (for [j (range (nrow mat))
        i (range (ncol mat))]
    (sel mat i j)))

(defn sum
  [xs]
  (areduce xs idx ret 0
    (+ ret (aget xs idx))))

(defn cumulative-sum
  [xs]
  (reductions + xs))

(declare positive? maybe-positive)

(defn positive? [mat] true)

(defn cholesky
  [a]
  (assert (and (positive? a) (.isSquare a))
          "Cholesky decompositions require positivity and must be square")
  (let [n (alength (.toArray a))
        u (Matrix. n n)]
    (loop [i 0]
      (when (< i n)
        (let [square-sum (double-array 1 0.0)]
          (loop [j 0]
            (when (< j i)
              (let [cross-sum (double-array 1 0.0)]
                (loop [k 0]
                  (when (< k j)
                    (aset cross-sum 0 (+ (aget cross-sum 0)
                                         (* (.getValueAt u i k)
                                            (.getValueAt u j k))))
                    (recur (inc k))))
                (let [aij (.getValueAt a i j)]
                  (.setValueAt u i j (/ (- aij (aget cross-sum 0))
                                        (.getValueAt u j j)))
                  (aset square-sum 0 (+ (aget square-sum 0)
                                        (* (.getValueAt u i j)
                                           (.getValueAt u i j))))))
              (recur (inc j))))
          (.setValueAt u i i (Math/sqrt (- (.getValueAt a i i)
                                           (aget square-sum 0)))))
        (recur (inc i))))
    u))

(defn decomp-cholesky
  [mat]
  (cholesky (to-matrix-2d mat)))

(defn decomp-svd
  [mat]
  )

(defn decomp-eigenvalue
  [mat])

(defn lu
  [A]
  (let [n (count A)
        P (Matrix/createIdentityMatrix n)]
    (loop [j 0]
      (when (< j n)
        (let [col (acol A j)]
          (loop [i 0]
            (when (< i n)
              (let [kmax (Math/min i j)
                    s (double-array 1 0.0)]
                (loop [k 0]
                  (when (< k kmax)
                    (aset s 0 (+ (aget s 0)
                                 (* (.getValueAt A i k) (aget col k))))
                    (recur (inc k))))
                (.setValueAt A i j (aset col i (- (aget col i) (aget s 0)))))
              (recur (inc i))))

          (loop [biggest j
                 i (inc j)]
            (if (< i n)
              (if (> (Math/abs (aget col i))
                     (Math/abs (aget col biggest)))
                (recur i (inc i))
                (recur biggest (inc i)))
              (when-not (== biggest j)
                (swap-rows! A biggest j)
                (swap-rows! P biggest j))))
          
          (when (and (< j n) (not (== (.getValueAt A j j) 0)))
            (loop [i (inc j)]
              (when (< i n)
                (.setValueAt A i j (/ (.getValueAt A i j)
                                      (.getValueAt A j j)))
                (recur (inc i))))))
        (recur (inc j))))
    
    (let [L (Matrix. n n)]
      (loop [i 0]
        (when (< i n)
          (loop [j 0]
            (when (< j i)
              (.setValueAt L i j (.getValueAt A i j))
              (recur (inc j))))
          (.setValueAt L i i 1.0)
          (recur (inc i))))
      
      (loop [i 0]
        (when (< i n)
          (loop [j 0]
            (when (< j i)
              (.setValueAt A i j 0)
              (recur (inc j))))
          (recur (inc i))))
      
      {:L L :U A :P P})))

(defn decomp-lu
  [mat]
  (let [gmat (to-matrix-2d mat)]
    (when (.isSquare gmat)
      (lu gmat))))

(defn vector-length
  [u]
  (sqrt (reduce + (map (fn [c] (pow c 2)) u))))

(defn inner-product
  [u v]
  (apply + (mult u (trans v))))

(defn proj
  [u v]
  (mult (div (inner-product v u) (inner-product u u)) u))

(defn decomp-qr
  [m & {:keys [type]}])

(defn condition
  [mat]
  (let [s (:S (decomp-svd mat))]
    (/ (apply max s) (apply min s))))

(defn rank
  [mat]
  )

(defn length
  [coll]
  (cond
    (number? coll) 1
    (matrix? coll) (* (nrow coll) (ncol coll))
    (coll? coll) (count coll)
    :else 1))

(defn group-on
  [mat on-cols & {:keys [cols except-cols]}]
  (let [groups (if (coll? on-cols)
                 (into #{} (to-list (sel mat :cols on-cols)))
                 (sort (into #{} (to-list (sel mat :cols on-cols)))))
        filter-fn (fn [group]
                    (cond
                      (and (coll? on-cols) (> (count on-cols) 1))
                      (fn [row]
                        (reduce #(and %1 %2)
                                (map (fn [i g] (= (nth row i) g))
                                     on-cols group)))
                      (and (coll? on-cols) (= (count on-cols) 1))
                      (fn [row] (= (nth row (first on-cols)) group))
                      :else (fn [row] (= (nth row on-cols) group))))]
    (cond
      cols (map #(sel mat :cols cols :filter (filter-fn %)) groups)
      except-cols
      (map #(sel mat :except-cols except-cols :filter (filter-fn %)) groups)
      :else (map #(sel mat :filter (filter-fn %)) groups))))

(defn dataset
  [column-names & data]
  (let [dat (cond
              (or (map? (ffirst data)) (coll? (ffirst data))) (first data)
              (map? (first data)) data
              :else (map vector (first data)))
        rows (cond
               (map? dat) [dat]
               (map? (first dat)) dat
               :else (map #(apply assoc {} (interleave column-names %)) dat))]
    (Dataset. (into [] column-names) rows)))

(defn get-column-id [dataset column-key]
  (let [headers (:column-names dataset)
        col-key (if (and (keyword? column-key)
                         (not (some #{column-key} headers))) 
                  (name column-key)
                  column-key)
        id (if (number? col-key)
             (if (some #(= col-key %) headers)
               col-key
               (nth headers col-key))
             col-key)]
    id))

(defn map-get
  ([m k]
     (if (keyword? k)
       (or (get m k) (get m (name k)))
       (get m k)))
  ([m k colnames]
     (cond
       (keyword? k) (or (get m k) (get m (name k)))
       (number? k) (get m (nth colnames k))
       :else (get m k))))

(defn submap [m ks]
  (zipmap (if (coll? ks) ks [ks])
          (map #(map-get m %) (if (coll? ks) ks [ks]))))

(defn query-to-pred
  ([query-map]
     (let [in-fn (fn [value val-set] (some val-set [value]))
           nin-fn (complement in-fn)
           ops {:gt #(> (compare %1 %2) 0)
                :lt #(< (compare %1 %2) 0)
                :eq =
                :ne not=
                :gte #(>= (compare %1 %2) 0)
                :lte #(<= (compare %1 %2) 0)
                :in in-fn :nin nin-fn :fn (fn [v f] (f v))
                :$gt #(> (compare %1 %2) 0)
                :$lt #(< (compare %1 %2) 0)
                :$eq = :$ne not=
                :$gte #(>= (compare %1 %2) 0)
                :$lte #(<= (compare %1 %2) 0)
                :$in in-fn :$nin nin-fn
                :$fn (fn [v f] (f v))}
           _and (fn [a b] (and a b))]
       (fn [row]
         (reduce _and
                 (for [k (keys query-map)]
                   (if (map? (query-map k))
                     (reduce _and
                             (for [sk (keys (query-map k))]
                               (cond
                                 (fn? sk)
                                 (sk (row k) ((query-map k) sk))
                                 (nil? (ops sk))
                                 (throw
                                  (js/Error.
                                   (str "Invalid key in query-map: " sk)))
                                 :else
                                 ((ops sk) (row k) ((query-map k) sk)))))
                     (= (row k) (query-map k)))))))))

(defn query-dataset
  ([data query-map]
     (if (fn? query-map)
       (assoc data :rows
              (for [row (:rows data) :when (query-map row)] row))
       (let [qmap (into {}
                        (for [k (keys query-map)]
                          (if (keyword? k)
                            (if (some #{k} (:column-names data))
                              [k (query-map k)]
                              [(name k) (query-map k)])
                            [k (query-map k)])))
             pred (query-to-pred qmap)
             rows (:rows data)]
         (assoc data :rows
                (for [row rows :when (pred row)] row))))))


(defn- except-for-cols
  ([data except-cols]
     (let [colnames (:column-names data)
           _except-cols (if (coll? except-cols)
                          (map #(get-column-id data %) except-cols)
                          [(get-column-id data except-cols)])
           except-names  (if (some number? _except-cols)
                           (map #(nth colnames %) _except-cols)
                           _except-cols)]
       (for [name colnames :when (not (some #{name} except-names))]
         name))))

(defn to-dataset
  [obj & {:keys [transpose]}]
  (let [transpose? (true? transpose)
        colnames (cond
                   (dataset? obj) (:column-names obj)
                   (map? obj) (keys obj)
                   (coll? obj)
                   (cond
                     (map? (first obj)) (keys (first obj))
                     (coll? (first obj)) (map #(keyword (str "col-" %))
                                              (range (length (first obj))))
                     transpose?
                     (map #(keyword (str "col-" %)) (range (length obj)))
                     :else [:col-0])
                   :else [:col-0])
        rows (cond
               (dataset? obj) (:rows obj)
               (map? obj) (if (reduce #(or %1 %2) (map coll? (vals obj)))
                            (trans (vals obj))
                            [(vals obj)])
               (coll? obj) (cond
                             (coll? (first obj)) obj
                             transpose? [obj]
                             :else obj)
               :else [obj])]
    (dataset colnames rows)))

(defn make-unique
  ([coll] (make-unique coll #{}))
  ([coll seen]
     (let [new-name
           (fn new-name [x]
             (if (not (contains? seen x))
               x
               (let [match (re-matches #"(.*\-)([0-9]+)"
                                       (.getName x))]
                 (if match
                   (new-name (keyword (str (second match)
                                           (inc (Integer/parseInt
                                                 (nth match 2))))))
                   (new-name (keyword (str (.getName x) "-1")))))))]

       (if (empty? coll)
         ()
         (let [name (new-name (first coll))]
           (cons name
                 (make-unique (rest coll) (conj seen name))))))))

(defn col-names
  ([data] (:column-names data))
  ([data colnames] (dataset colnames (to-list data))))

(defn conj-cols
  ([& args]
     (reduce (fn [A B]
               (let [a (to-dataset A)
                     b (to-dataset B)
                     ncol-a (ncol a)
                     ncol-b (ncol b)
                     colnames (make-unique (concat (col-names a)
                                                   (col-names b)))]
                 (dataset colnames
                          (map concat (to-list a) (to-list b)))))
             args)))

(defn conj-rows
  ([& args]
     (reduce (fn [A B]
               (let [a (to-dataset A :transpose true)
                     b (to-dataset B :transpose true)]
                 (dataset (:column-names a)
                          (concat (to-list a) (to-list b)))))
             args)))

(defn $
  ([cols]
     ($ :all cols $data))
  ([arg1 arg2]
     (let [rows-cols-data
           (cond (nil? arg2) [:all arg1 $data]
             (or (matrix? arg2) (dataset? arg2)) [:all arg1 arg2]
             :else [arg1 arg2 $data])]
       (apply $ rows-cols-data)))
  ([rows cols data]
     (let [except-rows? (and (vector? rows) (= :not (first rows)))
           except-cols? (and (vector? cols) (= :not (first cols)))
           _rows (if except-rows?
                   (conj [:except-rows]
                         (if (coll? (second rows))
                           (second rows)
                           (rest rows)))
                   [:rows rows])
           _cols (if except-cols?
                   (if (coll? (second cols))
                     (conj [:except-cols] (second cols))
                     (conj [:except-cols] (rest cols)))
                   [:cols cols])
           args (concat _rows _cols)]
       (apply sel data args))))

(defn head
  ([len mat]
     (cond
       (= len 0) ($ :none :all mat)
       (<= len (- (nrow mat))) (head 0 mat)
       (< len 0) (head (+ (nrow mat) len) mat)
       :else ($ (range (min len (nrow mat))) :all mat)))
  ([mat]
     (head 10 mat)))

(defn tail
  ([len mat]
     (cond
       (= len 0) ($ :none :all mat)
       (<= len (- (nrow mat))) (head 0 mat)
       (< len 0) (head (+ (nrow mat) len) mat)
       :else ($ (range (max 0 (- (nrow mat) len)) (nrow mat)) :all mat)))
  ([mat]
     (tail 10 mat)))


(defn $where
  ([query-map] (query-dataset $data  query-map))
  ([query-map data] (query-dataset data query-map)))

(defn $rollup
  ([summary-fun col-name group-by]
     ($rollup summary-fun col-name group-by $data))
  ([summary-fun col-name group-by data]
     (let [key-fn (if (coll? col-name)
                    (fn [row]
                      (into [] (map #(map-get row %) col-name)))
                    (fn [row]
                      (map-get row col-name)))
           rows (:rows data)
           rollup-fns {:max (fn [col-data] (apply max col-data))
                       :min (fn [col-data] (apply min col-data))
                       :sum (fn [col-data] (apply + col-data))
                       :count count
                       :mean (fn [col-data] (/ (apply + col-data)
                                               (count col-data)))}
           rollup-fn (if (keyword? summary-fun)
                       (rollup-fns summary-fun)
                       summary-fun)]
       (loop [cur rows reduced-rows {}]
         (if (empty? cur)
           (let [group-cols (to-dataset (keys reduced-rows))
                 res (conj-cols group-cols
                                (map rollup-fn (vals reduced-rows)))]
             (col-names res (concat (col-names group-cols)
                                    (if (coll? col-name) col-name
                                        [col-name]))))
           (recur (next cur)
                  (let [row (first cur)
                        k (submap row group-by)
                        a (reduced-rows k)
                        b (key-fn row)]
                    (assoc reduced-rows k (if a (conj a b) [b])))))))))

(defn $order
  ([cols order]
     ($order cols order $data))
  ([cols order data]
     (let [key-cols (if (coll? cols) cols [cols])
           key-fn (fn [row] (into [] (map #(map-get row %) key-cols)))
           comp-fn (if (= order :desc)
                     (comparator (fn [a b] (pos? (compare a b))))
                     compare)]
       (dataset (col-names data) (sort-by key-fn comp-fn (:rows data))))))

(defn $group-by
  ([cols]
     ($group-by cols $data))
  ([cols data]
     (let [orig-col-names (:column-names data)
           groups (group-by #(submap % cols) (:rows data))]
       (into {}
             (for [[group-value group-rows] groups]
               {group-value (dataset orig-col-names group-rows)})))))

(defn matrix-map
  ([f m]
     (let [m (if (and (matrix? m) (row? m)) (to-list m) m)]
       (if (sequential? m)
         (if (sequential? (first m))
           (map (fn [& a] (apply map f a)) m)
           (map f m))
         (f m))))
  ([f m & ms]
     (let [m (if (and (matrix? m) (row? m)) (to-list m) m)]
       (if (sequential? m)
         (if (sequential? (first m))
           (apply map (fn [& a] (apply map f a)) m ms)
           (apply map f m ms))
         (apply f m ms)))))

(defn $map
  ([fun col-keys data]
     (let [rows (:rows data)]
       (if (coll? col-keys)
         (map (fn [row] (apply fun (map (fn [k] (map-get row k)) col-keys)))
              (:rows data))
         (map (fn [row] (fun (map-get row col-keys))) (:rows data)))))
  ([fun col-keys]
     ($map fun col-keys $data)))

(defn $join
  ([[left-keys right-keys] left-data]
     ($join [left-keys right-keys] left-data $data))
  ([[left-keys right-keys] left-data right-data]
     (let [left-keys (if (coll? left-keys) left-keys [left-keys])
           right-keys (if (coll? right-keys) right-keys [right-keys])
           index
           (apply hash-map
                  (interleave
                   (map (fn [row]
                          (apply hash-map
                                 (interleave
                                  right-keys
                                  (map #(map-get (submap row left-keys) %)
                                       left-keys))))
                        (:rows left-data))
                   (map #(reduce dissoc % left-keys) (:rows left-data))))
           rows (map #(merge (index (submap % right-keys)) %)
                     (:rows right-data))]
       (to-dataset rows))))

(defn replace-by-number-or-value [col-vec [old-col new-col-name]]
  (if (number? old-col)
    (assoc col-vec old-col new-col-name)
    (replace {old-col new-col-name} col-vec)))

(defn rename-cols
  ([col-map]
     (rename-cols col-map $data))
  ([col-map data]
     (let [old-col-names (col-names data)
           new-col-names (reduce
                          replace-by-number-or-value old-col-names col-map)]
       (col-names data new-col-names))))

(defn update
  ([m key f] (update-in m [key] f))
  ([m key f & kfs] (apply update (update-in m [key] f) kfs)))

(defn replace-column
  ([column-name values]
     (replace-column column-name values $data))
  ([column-name values data]
     (update data :rows
             (fn [rows]
               (map #(assoc %1 column-name %2)
                    rows values)))))

(defn add-column
  ([column-name values]
     (add-column column-name values $data))
  ([column-name values data]
     (if (some #{column-name} (:column-names data))
       (replace-column column-name values data)
       (update data :column-names #(conj % column-name)
               :rows #(mapv (fn [r v]
                              (assoc r column-name v))
                            % (concat values (repeat nil)))))))

(defn add-derived-column
  ([column-name from-columns f]
     (add-derived-column column-name from-columns f $data))
  ([column-name from-columns f data]
     (update data :column-names #(conj % column-name)
             :rows (fn [rows]
                     (mapv (fn [row]
                             (assoc row column-name
                                    (apply f (map #(map-get row %)
                                                  from-columns))))
                           rows)))))

(defn transform-col
  [dataset column f & args]
  (->> (map #(apply update-in % [column] f args) (:rows dataset))
       vec
       (assoc dataset :rows)))


(defn deshape
  ([& {:keys [data remove-na group-by merge] :or {remove-na true}}]
     (let [data (or data $data)
           colnames (col-names data)
           _group-by (into #{} (when group-by
                                 (if (coll? group-by)
                                   group-by
                                   [group-by])))
           _merge (into #{} (when merge
                              (if (coll? merge)
                                merge
                                [merge])))
           __group-by (if (empty? _group-by)
                        (set/difference (into #{} (col-names data)) _merge)
                        _group-by)
           __merge (if (empty? _merge)
                     (set/difference (into #{} (col-names data)) _group-by)
                     _merge)
           deshaped-data
           (mapcat (fn [row]
                     (let [base-map
                           (zipmap __group-by
                                   (map #(map-get row % colnames)
                                        __group-by))]
                       (filter identity
                               (map (fn [k]
                                      (if (and remove-na
                                               (nil? (map-get row k
                                                              colnames)))
                                        nil
                                        (assoc base-map :variable k
                                               :value (map-get row k
                                                               colnames))))
                                    __merge))))
                   (:rows data))]
       (to-dataset deshaped-data))))


(defn get-categories
  [cols data]
  (if (coll? cols)
    (for [col cols] (into #{} ($ col data)))
    (into #{} ($ cols data))))

(defprotocol IMapLike
  (-to-map [_]))

(extend-protocol IMapLike
  Dataset
  (-to-map [data]
    (let [cols (map (partial sel data :cols) (col-names data))
          col-keys (map keyword (col-names data))]
      (zipmap col-keys cols)))

  Matrix
  (-to-map [mat]
    (-to-map (matrix mat)))

  mat/Matrix
  (-to-map [mat]
    (let [cols (to-list (trans mat))
          col-keys (range (ncol mat))]
      (zipmap col-keys cols))))

(defn to-map
  [map-like]
  (-to-map map-like))

(defn melt
  [dataset pivot-key]
  (let [in-m (to-map dataset)
        nrows (nrow dataset)
        ks (keys in-m)]
    (to-dataset
     (for [k ks i (range nrows) :when (not (= pivot-key k))]
       (zipmap [pivot-key :variable :value]
               [(nth (pivot-key in-m) i) k (nth (k in-m) i)])))))

(defn categorical-var
  ([& {:keys [data ordered? labels levels] :or {ordered? false}}]
     (let [labels (or labels
                      (if (nil? data)
                        levels
                        (sort (into #{} data))))
           levels (or levels (range (count labels)))]
       {:ordered? ordered?
        :labels labels
        :levels levels
        :to-labels (apply assoc {} (interleave levels labels))
        :to-levels (apply assoc {} (interleave labels levels))})))

(defn to-levels
  ([coll & options]
     (let [opts (when options (apply assoc {} options))
           cat-var (or (:categorical-var opts) (categorical-var :data coll))
           to-levels (:to-levels cat-var)]
       (for [label coll] (to-levels label)))))

(defn to-labels
  ([coll cat-var]
     (let [to-labels (:to-labels cat-var)]
       (for [level coll] (to-labels level)))))

(defn get-dummies [n]
  (let [nbits (int (dec (Math/ceil (log2 n))))]
    (map #(for [i (range nbits -1 -1)] (if (bit-test % i) 1 0))
         (range n))))

(defn to-dummies [coll]
  (let [cat-var (categorical-var :data coll)
        levels (:levels cat-var)
        encoded-data (to-levels coll :categorical-var cat-var)
        bit-map (get-dummies (count levels))]
    (for [item encoded-data]
      (nth bit-map item))))

(defn get-columns [dataset column-keys]
  (map (fn [col-key] (map #(% (get-column-id dataset col-key))
                          (:rows dataset))) column-keys))

(defn string-to-categorical [dataset column-key dummies?]
  (let [col (first (get-columns dataset [column-key]))]
    (if (some string? col)
      (if dummies? (matrix (to-dummies col)) (matrix (to-levels col)))
      (matrix col))))

(defn to-matrix
  [dataset & {:keys [dummies] :or {dummies false}}]
  (reduce bind-columns
          (map #(string-to-categorical dataset % dummies)
               (range (count (keys (:column-names dataset)))))))


(defn solve-quadratic
  [a b c]
  (let [t1 (- 0 b)
        t2 (Math/sqrt (- (* b b) (* 4 a c)))
        t3 (* 2 a)]
    [(/ (- t1 t2) t3)
     (/ (+ t1 t2) t3)]))

(defn symmetric-matrix
  [data & {:keys [lower] :or {lower true}}]
  (let [n (count data)
        p (int (second (solve-quadratic 0.5 0.5 (- 0 n))))
        mat (matrix 0 p p)
        indices (if lower
                  (vec (for [i (range p) j (range p) :when (<= j i)] [i j]))
                  (vec (for [i (range p) j (range p) :when (<= i j)] [j i])))]
    (reduce-kv (fn [mat idx [i j]]
                 (let [res (nth data idx)
                       d (if (and (matrix? res)
                                  (= 1 (nrow res))
                                  (= 1 (ncol res)))
                           (sel res 0 0)
                           res)]
                   (assoc-in mat [i j] d)
                   (assoc-in mat [j i] d)))
               mat indices)))

(defn toeplitz
  [x]
  (symmetric-matrix
   (loop [v (vec (rseq x))
          d []]
     (if (nil? v)
       d
       (recur (next v) (catvec v d))))))

(defprotocol IViewable
  (-view [obj options]))

(extend-protocol IViewable
  Matrix
  (-view [obj options])

  Dataset
  (-view [obj options])

  mat/Matrix
  (-view [obj options]))

(defn view
  [obj & options]
  (if (satisfies? IViewable obj)
    (-view obj options)
    (if (coll? obj)
      (let [rows (if (coll? (first obj))
                   obj
                   (map vector obj))
            colnames (range (count (first rows)))]
        (view (dataset colnames rows))))))

(defn data-table
  [data]
  (let [col-names (:column-names data)
        col-vals (map (fn [row] (map #(row %) col-names)) (:rows data))
        table-model []]
    ))

(defn set-data
  [table data])

(defn quit
  [])

(defn save
  [])

(defn grid-apply
  [f x-min x-max y-min y-max]
  (let [x-vals (range x-min x-max (/ (- x-max x-min) 100))
        y-vals (range y-min y-max (/ (- y-max y-min) 100))
        xyz (for [_x x-vals _y y-vals] [_x _y (f _x _y)])
        transpose #(list (conj (first %1) (first %2))
                         (conj (second %1) (second %2))
                         (conj (nth %1 2) (nth %2 2)))]
    (reduce transpose [[] [] []] xyz)))

(defn reorder-columns
  [ds cols]
  (let [cols (filter (partial contains? (set (:column-names ds))) cols)]
    (cond
      (empty? cols) nil
      (= (count cols) 1) (dataset cols (sel ds :cols (first cols)))
      :else (sel ds :cols cols))))

(defn ^:export -main [& args]
  (println "Hello, world!"))

(def A (matrix [[1 2 3]
                [4 5 6]
                [7 8 9]]))

(def B (matrix [10 11 12]))

(def C (matrix [[2 0 0] [0 2 0] [0 0 2]]))

(comment
  (lu-decomp A)
  [[1.0 0.0 0.0
    0.14285714285714285 1.0 0.0
    0.5714285714285714 0.5000000000000002 1.0]
   [7.0 8.0 9.0
    0.0 0.8571428571428572 1.7142857142857144
    0.0 0.0 1.1102230246251565E-16]
   [0.0 1.0 0.0
    0.0 0.0 1.0
    1.0 0.0 0.0]])
