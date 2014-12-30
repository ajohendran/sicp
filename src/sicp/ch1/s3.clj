(ns sicp.ch1.s3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3 Procedures as Arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exp [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))

(defn sum-integers [a b]
  (if (> a b)
    0
    (+ a (sum-integers (inc a) b))))

(defn sum-integers [a b sum]
  (if (> a b)
    sum
    (sum-integers (inc a) b (+ sum a))))

(defn sum-cubes [a b sum]
  (if (> a b)
    sum
    (sum-cubes (inc a) b (+ sum (exp a 3)))))

(defn sum-integers [a b]
  (loop [acc 0
        a a
        b b]
    (if (> a b)
      acc
      (recur (+ acc a) (inc a) b))))

(defn sum-cubes [a b]
  (loop [acc 0
        a a
        b b]
    (if (> a b)
      acc
      (recur (+ acc (exp a 3)) (inc a) b))))

(defn pi-sum [a b]
  (loop [acc 0
        a a
        b b]
    (if (> a b)
      acc
      (recur (+ acc (/ 1 (* a (+ a 2)))) (+ a 4) b))))


(defn sum [term a next b]
  (loop [acc 0
        a a]
    (if (> a b)
      acc
      (recur (+ acc (term a)) (next a)))))

(defn cube [n]
  (* n n n))

;; (sum (fn [x] (/ 1 (* x (+ x 2)))) 1 (fn [x] (+ x 4)) 5)
;; (sum cube 1 inc 3)

