(ns lsh-mmd.core
  (:require [clojure.set :as set]))

(defn shingles [n s]
  (->> s
    (partition n 1)
    (map (partial apply str))
    set))

(defn jacc-sim [s1 s2]
  (/ (count (set/intersection s1 s2))
     (count (set/union s1 s2))))

(defn- lcs 
  "Longest common subsequence"
  [a b]
  (let [ca  (count a)
        cb  (count b)
        m   (vec (repeat (inc ca) 
                         (vec (repeat (inc cb) 0))))
        pos (for [i (range 1 (inc ca)) j (range 1 (inc cb))] [i j])
        f   (fn [m [i j]]
              (let [x (nth a (dec i))
                    y (nth b (dec j))]
                #_(prn [i j] x y m)
                (assoc-in m [i j]
                  (if (= x y )
                    (inc (get-in m [(dec i) (dec j)]))
                    (max (get-in m [i (dec j)])
                         (get-in m [(dec i) j]))))))
        m (reduce f m pos)]
    (get-in m [ca cb])))

(defn edit-dist [a b]
  (Math/abs
    (- (+ (count a) (count b))
       (* 2 (lcs a b)))))

(defn pow [n x]
  (Math/pow x n))

(defn ln-dist 
  [n a b]
  (->> (map - a b)
    (map (partial pow n))
    (reduce +)
    (pow (/ 1 n))
    Math/abs))

(def l1-dist (partial ln-dist 1))
(def l2-dist (partial ln-dist 2))

(l1-dist [0 0] [1 1])

(comment)
  ;; Question 1
  (let [words #{"he" "she" "his" "hers"}
        pairs (into #{} (for [x words y words :when (> (hash x) (hash y))] 
                          [x y]))
        _     (prn pairs)
        f     (fn [m [x y]]
                (update-in m [(edit-dist x y)] conj [x y]))]
      (clojure.pprint/pprint (reduce f {} pairs)))
  ;; Question 4
  (->> ["ABRACADABRA"
        "BRICABRAC"]
    (map (partial shingles 2))
    (apply jacc-sim))
  ;; Question 6
  (let [p1     [0 0]
        p2     [100 40]
        points [[50 18] [53 15] [53 18] [66 5]]
        f      #(vector %
                 :l1 (if (< (l1-dist p1 %) (l1-dist p2 %)) :p1 :p2)
                 :l2 (if (< (l2-dist p1 %) (l2-dist p2 %)) :p1 :p2))]
    (map f points))



