(ns sicp.ch1.s3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3 Procedures as Arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cube [x] (* x x x))

(defn sum [term a nextt b]
  (if (> a b)
    0
    (+ (term a) (sum term (nextt a) nextt b))))

(defn sum-cubes [a b]
  (sum cube a inc b))

(defn sum-integers [a b]
  (sum identity a inc b))

(defn pi-sum [a b]
  (defn pi-term [x]
    (/ 1.0 (* x (+ 2 x))))
  (defn pi-next [x]
    (+ x 4))
  (* 8 (sum pi-term a pi-next b)))

(defn integral [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* dx (sum f (+ a (/ dx 2)) add-dx b)))

;; If sum is altered to print out 'a'
;; sicp.ch1.s3> (integral cube 0 1 0.1)

;; a= 0.05
;; a= 0.15000000000000002
;; a= 0.25
;; a= 0.35
;; a= 0.44999999999999996
;; a= 0.5499999999999999
;; a= 0.6499999999999999
;; a= 0.7499999999999999
;; a= 0.8499999999999999
;; a= 0.9499999999999998

;; 0.24874999999999994


;; NOTE - Following definition is different from book's definition
;; According to book sum is called with a-> a + dx/2
;; What if 'a' in the formula is same as parameter 'a' of sum procedure
;; That is, 'a' passed to integral procedure is  'a' passed to sum procedure

(defn integral-2 [f a b dx]
  (defn i-term [x] (f (+ x (/ dx 2.0))))
  (defn add-dx [x] (+ x dx))
  (* dx (sum i-term a add-dx b))) 

;; sicp.ch1.s3> (integral-correct cube 0 1 0.1)

;; a= 0
;; a= 0.1
;; a= 0.2
;; a= 0.30000000000000004
;; a= 0.4
;; a= 0.5
;; a= 0.6
;; a= 0.7
;; a= 0.7999999999999999
;; a= 0.8999999999999999
;; a= 0.9999999999999999

;; 0.36451249999999996


;; The above values of 'a' are clearly wrong
;; NOTE: 'a' in formula is different from parameter 'a' passed to sum procedure
;; i.e.: 'a' passed to integral procedure may not be 'a' passed to sum procedure,
;; their meanings are different!



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.29
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (h/3) * [y0 + 4y1 + 2y2 + 4y3 + 2y4 + ......2yn-2+ 4yn-1+yn]
;; h=(b-a)/n
;; yk = f(a+kh)


;; First, every 3rd element needs to be broken into two so that
;; we can use our SUM procedure easily.
;; So each TERM is composed of three parts

;; f(a+0h) + 4f(a+1h) + f(a+2h) +
;; f(a+2h) + 4f(a+3h) + f(a+4h) +
;; f(a+4h) + 4f(a+5h) + f(a+6h) +
;; ............................ +
;; ............................ +
;; f(a+(n-2)h) + 4f(a+(n-1)h) + f(a+nh)


;; This algorithm is essentially breaking the interval b-a into small parts
;; by dividing into  n parts.
;; We successively add each small part to 'a' and apply the function over it.
;; We then add the resulting values with a periodic set of co-efficients applied

;; If n=10,a=0, b=1,f=cube, then h = 0.1

;; intergal = (1)/(10*3) *
;; [
;;  f(a+0h) + 4f(a+1h) + f(a+2h)
;;  f(a+2h) + 4f(a+3h) + f(a+4h)
;;  f(a+4h) + 4f(a+5h) + f(a+6h)
;;  f(a+6h) + 4f(a+7h) + f(a+8h)
;;  f(a+8h) + 4f(a+9h) + f(a+10h)
;; ]

;; = 1/30 *
;; [
;;  (0.0)^3 + 4*(0.1)^3 + (0.2)^3
;;  (0.2)^3 + 4*(0.3)^3 + (0.4)^3
;;  (0.4)^3 + 4*(0.5)^3 + (0.6)^3
;;  (0.6)^3 + 4*(0.7)^3 + (0.8)^3
;;  (0.8)^3 + 4*(0.9)^3 + (1.00)^3
;; ]

;; = 1/30 *
;; [
;;  0.000 + 0.004 + 0.008    ;; term-> 0.012
;;  0.008 + 0.108 + 0.064    ;; term-> 0.18
;;  0.064 + 0.500 + 0.216    ;; term-> 0.78
;;  0.216 + 1.372 + 0.512    ;; term-> 2.1
;;  0.512 + 2.916 + 1.000    ;; term-> 4.428
;; ]

;; = 1/30 * 7.5

;; = 0.25



;; NOTE: 'a' passed to integral procedure is not the 'a' passed to sum procedure
;; See analysis in above section
;; For this formula, we have a to b ,  divided into h sections
;; For sum procedure, we start with formula value 'a' and add h to it until we reach
;; formula value 'b'. So, in this case from a+0h to a+nh
(defn simpson-rule [f a b n]
  (defn nextt [x] (+ x 2))
  (defn helper [h]
    (defn term [x]
      (+ (f (+ a (* x h)))
         (* 4 (f (+ a (* (inc x) h))))
         (f (+ a (* (+ 2 x) h)))))
    (* (/ h 3.0) (sum term 0 nextt (- n 2))))
  (helper (/ (- b a) n)))

;; If we altered sum procedure to print a and term(a)
;; sicp.ch1.s3> (simpson-rule cube 0 1 10.0)
;; a= 0  ; term = 0.012000000000000004
;; a= 2  ; term = 0.18000000000000005
;; a= 4  ; term = 0.7800000000000001
;; a= 6  ; term = 2.1000000000000005
;; a= 8  ; term = 4.428000000000001
;; 0.25

;;
;; (defn sum [term a nextt b]
;;   (if (> a b)
;;     0
;;     (do (println "a=" a "; term(a)=" (term a))
;;         (+ (term a) (sum term (nextt a) nextt b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.30 - Iterative sum procedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn sum [term a nextt b]
  (defn iterr [a res]
    (if (> a b)
      res
      (iterr (nextt a) (+ res (term a)))))
  (iterr a 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.31 - Product 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn product [term a nextt b]
  (if (> a b)
    1
    (* (term a) (product term (nextt a) nextt b))))





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
