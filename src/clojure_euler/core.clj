(ns clojure-euler.core)

(defn problem1 []
  (letfn [(multiple-fn ([n] (or
                             (zero? (mod n 3))
                             (zero? (mod n 5)))))]
    (->> (range 0 1000)
         (filter multiple-fn)
         (reduce +))))

(defn problem2 []
  (letfn [(fibo-gen ([] (map first
                             (iterate (fn [[a b]] [b (+ a b)])
                                      [1 2]))))]
    (->> (fibo-gen)
         (take-while (fn [n] (< n 4000000)))
         (filter even?)
         (reduce +))))
