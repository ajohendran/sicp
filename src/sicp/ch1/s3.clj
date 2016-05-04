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
  (let [h (/ (- b a) n)
        term (fn [x]
               (+ (f (+ a (* x h)))
                  (* 4 (f (+ a (* (+ x 1) h))))
                  (f (+ a (* (+ x 2) h)))))
        nxt (fn [x] (+ x 2))]
    (* (/ h 3)
       (sum term 0 nxt (- n 2)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.31
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn product [term a nxt b]
  (loop [acc 1
        x a]
    (if (> x b)
      acc
      (recur (* acc (term x)) (nxt x)))))

(defn factorial [n]
  (product identity 1 inc n))


(defn pi-apprx [n]
  (let [term (fn [x]
               (/ (* x (+ x 2))
                  (* (inc x) (inc x))))
        nxt (fn [y] (+ y 2))]
    (double (* 4 (product term 2 nxt (* 2 n))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.32
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn accumulator [combiner null-val term a nxt b]
  (loop [acc null-val
        x a]
    (if (> x b)
      acc
      (recur (combiner acc (term x)) (nxt x)))))

(defn sum [term a nxt b]
  (accumulator + 0 term a nxt b))

(defn product [term a nxt b]
  (accumulator * 1 term a nxt b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.33
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn filtered-accumulate [combiner null-val term a nxt b fltr]
  (loop [acc null-val
        x a]
    (if (> x b)
      acc
      (recur (if (fltr x)
               (combiner acc (term x))
               acc)
             (nxt x)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.34
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn f [g]
  (g 2))

;; (f f) seems silly. it will call f with 2 and then try to use 2 as a fuction




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3 Procedures as General Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *tolerance*  0.0000001)
(defn fixed-point [f guess]
  (let [n (f guess)]
    (if (< (Math/abs (- n guess)) *tolerance*)
      guess
      (recur f n))))

(defn sqrt [x]
  (fixed-point (fn [y] (/ x y)) 1.0))

(defn avg [a b]
  (/ (+ a b) 2))

(defn sqrt [x]
  (fixed-point (fn [y] (avg y (/ x y))) 1.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.35
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; definition of golden ratio is (x * x) = (x + 1)
;; same as x -> 1 + 1/x

(defn gldnrto []
  (fixed-point (fn [y] (+ 1 (/ 1 y))) 1.0 ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.36
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fixed-point [f guess]
  (let [n (f guess)]
    (println "approx (" guess ") -> " n)
    (if (< (Math/abs (- n guess)) *tolerance*)
      guess
      (recur f n))))


(defn x-raised-x [x]
  (fixed-point (fn [y] (/ (Math/log10 x) (Math/log10 y))) 1.1))


(defn x-raised-x [x]
  (fixed-point (fn [y] (avg y (/ (Math/log x) (Math/log y)))) 1.1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.37
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;recursive
(defn cont-frac 
  ([n d k] (cont-frac n d k 1))
  ([n d k cntr]
   (if (< cntr k)
     (/ (n cntr) (+ (d cntr) (cont-frac n d k (inc cntr))))
     (/ (n cntr) (d cntr)))))



;; iterative
(defn cont-frac 
  ([n d k] (cont-frac n d k 0))
  ([n d k res]
   (if (= k 1) (/ (n 1) (+ (d 1) res))
       (recur n d (dec k) (double (/ (n k) (d k)))))))

;; (cont-frac (constantly 1) (constantly 1) 500)
;; k of 12 provides answer to accuracy of 4 decimal places

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.38
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1  -> 1 * 2  
;; 4  -> 2 *2
;; 7  -> 3 * 2
;; 10 -> 4 * 2
;; 13 -> 5 * 2


(defn denom-euler [n]
  (if (== 0 (mod (dec n) 3))
    (* 2 (inc (quot n 3)))
    1))

;; (cont-frac (constantly 1) denom-euler 100)

(defn euler-c ())
