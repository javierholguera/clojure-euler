(ns clojure-euler.core)

(defn three-and-five-multiples
  []
  (letfn [(multiple-fn ([n] (or
                             (zero? (mod n 3))
                             (zero? (mod n 5)))))]
    (->> (range 0 1000)
         (filter multiple-fn)
         (reduce +))))

(defn even-fibo-numbers
  []
  (letfn [(fibo-gen ([] (map first
                             (iterate (fn [[a b]] [b (+ a b)])
                                      [1 2]))))]
    (->> (fibo-gen)
         (take-while (fn [n] (< n 4000000)))
         (filter even?)
         (reduce +))))

(defn prime-factors
  ([number]
   (prime-factors 600851475143 1 []))

  ([number last-cand result]

   (letfn [(is-prime? [n] (every? (partial (fn [x y] (-> x (mod y) zero? not)) n)
                                  (range 2 (/ (inc n) 2))))

           (next-prime [n] (let [cand (inc n)]
                             (if (is-prime? cand)
                               cand
                               (recur cand))))]

     (let [next-cand (next-prime last-cand)]
       (if (> next-cand number)
         result
         (if (zero? (mod number next-cand))
           (recur (/ number next-cand)
                  next-cand
                  (conj result next-cand))
           (recur number
                  next-cand
                  result)))))))

