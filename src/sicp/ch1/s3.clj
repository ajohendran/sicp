(ns sicp.ch1.s3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3 Procedures as Arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exp [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))


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
        a a]
    (if (> a b)
      acc
      (recur (+ acc a) (inc a) b))))

(defn sum-cubes [a b]
  (loop [acc 0
        a a]
    (if (> a b)
      acc
      (recur (+ acc (exp a 3)) (inc a) b))))

(defn pi-sum [a b]
  (loop [acc 0
        a a]
    (if (> a b)
      acc
      (recur (+ acc (/ 1 (* a (+ a 2)))) (+ a 4) b))))

(defn sum [term a nxt b]
  (loop [acc 0
        a a]
    (if (> a b)
      acc
      (recur (+ acc (term a)) (nxt a)))))

(defn cube [n]
  (* n n n))


(defn sum-integers [a b]
  (sum identity a inc b))

(defn sum-cubes [a b]
 (sum cube a inc b))

;; (sum (fn [x] (/ 1 (* x (+ x 2)))) 1 (fn [x] (+ x 4)) 5)
;; (sum cube 1 inc 3)

(defn integral [f a b dx]
  (letfn [(add-dx [x] (+ x dx))]
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.29
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; f(a+0h) + 4f(a+1h) + f(a+2h)
;; f(a+2h) + 4f(a+3h) + f(a+4h)
;; f(a+4h) + 4f(a+5h) + f(a+6h)
;; ------
;; f(a+(n-2)h) + 4f(a+(n-1)h) + f(a+nh)

(defn integral [f a b n]
  (let [h (double (/ (- b a) n))
        term (fn [x]
               (+ (f (+ a (* x h)))
                  (* 4 (f (+ a (* (+ x 1) h))))
                  (f (+ a (* (+ x 2) h)))))
        nxt (fn [x] (+ x 2))]
    (* (/ h 3)
       (sum term 0 nxt (- n 2)))))

