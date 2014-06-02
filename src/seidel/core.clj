(ns seidel.core
  (:gen-class))

(java.util.Locale/setDefault java.util.Locale/US)

(defn read-row [next-line]
  (let [s (next-line)
        spl (clojure.string/split s #" ")
        dbl (map #(Double/parseDouble %) spl)]
    (vec dbl)
    )
  )

(defn read-matrix [rows next-line]
  (loop [matr [] i rows]
    (if (zero? i)
      matr
      (recur (conj matr (read-row next-line)) (dec i))
      )
    )
  )

(defn alpha-divide [matr]
  (loop [i 0 res matr]
    (if (= i (count matr))
      res
      (let [aii ((res i) i)]
        (recur (inc i)
               (assoc res i
                 (vec (map #(/ % aii) (res i)))
                 )
               )
        )
      )
    )
  )

(defn alpha-zero-diag [matr]
  (loop [i 0 res matr]
    (if (= i (count res))
      res
      (recur (inc i)
             (assoc res i
               (assoc (res i) i 0)
               )
             )
      )
    )
  )

(defn cons-alpha [matr]
  (alpha-zero-diag (alpha-divide matr))
  )

(defn cons-beta [A B]
  (loop [i 0 res []]
    (if (= i (count A))
      res
      (recur (inc i) (conj res
                           (/ (B i) ((A i) i))
                           )
             )
      )
    )
  )

(defn substract-vector [v1 v2]
  (apply map - [v1 v2])
  )

(defn scalar-product [a b]
  (reduce + (apply map * [a b]))
  )

(defn multiply-mat-vec [a b]
  (loop [i 0 res []]
    (if (= i (count a))
      res
      (recur (inc i)
             (conj res (scalar-product (a i) b)))
      )
    )
  )

(defn good-enough? [A x B eps]
  (let [b (multiply-mat-vec A x)
        diff (map #(java.lang.Math/abs %) (substract-vector b B))
        delta (reduce + diff)]
    (< delta eps)
    )
  )

(defn solve [A B a b eps]
  (loop [i 10000 x b]
    (let [new-x (substract-vector b (multiply-mat-vec a x))]
      (cond
       (zero? i) (do (println "overcycled!") new-x)
       (good-enough? A new-x B eps) new-x
       :else (recur (dec i) new-x)
       )
      )
    )
  )

(defn check-solution [A x B]
  (let [b (multiply-mat-vec A x)
        diff (map #(java.lang.Math/abs %) (substract-vector b B))
        delta (reduce + diff)]
    (println "Difference vector is " diff)
    (println "Delta is " delta)
    )
  )

(defn read-and-solve [eps]
  (with-open [fr (java.io.FileReader. "input.txt")
              scr (java.util.Scanner. fr)]
    (let [next-line #(. scr nextLine)
          A (read-matrix (Integer/parseInt (next-line)) next-line)
          B (read-row next-line)
          a (cons-alpha A)
          b (cons-beta A B)
          x (solve A B a b eps)]
      (println "Solution is " x)
      (check-solution A x B)
      )
    )
  )

(defn -main [& args]
  (if (zero? (count args))
    (read-and-solve 1)
    (read-and-solve (Double/parseDouble (first args)))
    )
  )
