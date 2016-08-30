(ns clojure-euler.core)

(defn three-and-five-multiples
  "Find the sum of all the multiples of 3 or 5 below 1000."
  []
  (letfn [(multiple-fn ([n] (or
                             (zero? (mod n 3))
                             (zero? (mod n 5)))))]
    (->> (range 0 1000)
         (filter multiple-fn)
         (reduce +))))

(defn even-fibo-numbers
  "By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms."
  []
  (letfn [(fibo-gen ([] (map first
                             (iterate (fn [[a b]] [b (+ a b)])
                                      [1 2]))))]
    (->> (fibo-gen)
         (take-while (fn [n] (< n 4000000)))
         (filter even?)
         (reduce +))))

(defn largest-prime-factors
  "What is the largest prime factor of the number 600851475143?"
  ([]
   (largest-prime-factors 600851475143 1 []))

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

(defn larget-palindrome-factor
  "Find the largest palindrome made from the product of two 3-digit numbers."
  []
  (letfn [(palindrome? [number]
            (let [text (str number)]
              (= text
                 (apply str (reverse text)))))]

    (->>
     (mapcat (fn [x] (map (partial * x)
                          (range 0 1000)))
             (range 0 1000))
     (filter palindrome?)
     (sort)
     (last))))

(defn smallest-multiple
  "What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"
  [n]
  (let [factors (range 1 (inc n))]
    (reduce (fn [acc v]
              (when (every? (fn [factor] (zero? (mod v factor))) factors)
                (reduced v)))
            (range))))

(defn sum-square-difference
  "Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."
  [n]
  (letfn [(sum-squares [n]
            (reduce (fn [acc v] (+ acc (* v v)))
                    (range 1 (inc n))))

          (square-sum [n]
            (let [r (apply + (range 1 (inc n)))]
              (* r r)))]

    (- (square-sum n) (sum-squares n))))

(def thousand-digit-number
  ["73167176531330624919225119674426574742355349194934"
   "96983520312774506326239578318016984801869478851843"
   "85861560789112949495459501737958331952853208805511"
   "12540698747158523863050715693290963295227443043557"
   "66896648950445244523161731856403098711121722383113"
   "62229893423380308135336276614282806444486645238749"
   "30358907296290491560440772390713810515859307960866"
   "70172427121883998797908792274921901699720888093776"
   "65727333001053367881220235421809751254540594752243"
   "52584907711670556013604839586446706324415722155397"
   "53697817977846174064955149290862569321978468622482"
   "83972241375657056057490261407972968652414535100474"
   "82166370484403199890008895243450658541227588666881"
   "16427171479924442928230863465674813919123162824586"
   "17866458359124566529476545682848912883142607690042"
   "24219022671055626321111109370544217506941658960408"
   "07198403850962455444362981230987879927244284909188"
   "84580156166097919133875499200524063689912560717606"
   "05886116467109405077541002256983155200055935729725"
   "71636269561882670428252483600823257530420752963450"])

(def large-number-seq
  (map (comp read-string str)
       (clojure.string/join "" thousand-digit-number)))

(defn largest-product-in-series
  "Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?"
  [size initial-seq]
  (loop [res (hash-map)
         digit-seq initial-seq]
    (if (< (count digit-seq) size)
      res
      (let [chunk (take size digit-seq)
            product (apply * chunk)]
        (recur (assoc res chunk product)
               (drop size digit-seq))))))

