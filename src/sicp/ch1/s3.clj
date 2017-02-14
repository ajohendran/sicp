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


;;;; a ;;;;

(defn product [term a nextt b]
  (if (> a b)
    1
    (* (term a) (product term (nextt a) nextt b))))


(defn factorial [n]
  (product identity 1 inc n))

;; Procedure can be written in many different ways
;; As with Simpson rule, we'll look to use the simplest definition
;; by consolidating what can be a term
(defn pi-approx [n]
  (defn nxt [x] (+ x 2.0))
  (defn term [x] (/
                  (* x (+ 2 x))
                  (* (inc x) (inc x))))
  (* 4.0 (product term 2.0 nxt (* 2 n))))


;;;;; b.

(defn product [term a nxt b]
  (defn itr [nxt-val res]
    (if (> nxt-val b)
      res
      (recur (nxt nxt-val) (* res (term nxt-val)))))
  (itr a 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.32
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;a;;
(defn accumulate [combiner null-value term a nxt b]
  (defn process [next-val]
    (if (> next-val b)
    null-value
    (combiner (term next-val) (process (nxt next-val)))))
  (process a))

(defn sum [term a nxt b]
  (accumulate + 0 term a nxt b))

(defn product [term a nxt b]
  (accumulate * 1 term a nxt b))


;;b;;

(defn accumulate [combiner null-value term a nxt b]
  (defn process [next-val res]
    (if (> next-val b)
      res
      (recur (nxt next-val) (combiner res (term next-val)))))
  (process a null-value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.33
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; recursive
(defn filtered-accumulate [combiner filtr null-value term a nxt b]
  (defn process [next-val]
    (cond
      (> next-val b) null-value
      (filtr next-val) (combiner (term next-val) (process (nxt next-val))) 
      :else (process (nxt next-val))))
  (process a))

;; iterative
(defn filtered-accumulate [combiner filtr null-value term a nxt b]
  (defn process [next-val res]
    (cond
      (> next-val b) res
      (filtr next-val) (process (nxt next-val) (combiner res (term next-val))) 
      :else (process (nxt next-val) res)))
  (process a null-value))


(defn square [n]
  (* n n))

;; could be made a tad bit more efficient by introducing another function.
;; for purposes of illustration and for maintaining clarity, leaving as is
(defn check-non-trivial-sqrt [result m]
  (cond
    (= result (dec m)) (rem (square result) m)
    (= result 1) (rem (square result) m)
    (= (rem (square result) m) 1) 0 
    :else (rem (square result) m)))

(defn expmod-mr [base exp m]
  (cond 
    (= exp 1) base
    (even? exp) (check-non-trivial-sqrt
                 (expmod-mr base (/ exp 2) m)
                 m)
    :else (rem (* base (expmod-mr base (dec exp) m)) m)))


(defn miller-rabin-test [n]
  (defn try-it [a]
    (= (expmod-mr a (dec n) n) 1))
  (try-it (inc (rand-int (dec n)))))


(defn fast-prime-mr? [n times]
  (cond
    (= times 0) true
    (miller-rabin-test n) (fast-prime-mr? n (dec times))
    :else false))

(defn prime? [n]
  (fast-prime-mr? n 20))

(defn gcd [a b]
  (if (= b 0)
    a
    (recur b (rem a b))))


;;;; a ;;;;
(defn sum-squares-primes [a b]
  (filtered-accumulate + prime? 0 identity a inc b))


;;;; b ;;;;


(defn product-relative-prime [n]
  (defn relative-prime? [x]
    (= 1 (gcd n x)))
  (filtered-accumulate * relative-prime? 1 identity 1 inc n))


;; sicp.ch1.s3> (filter #(= 1 (gcd 10 %)) (range 1 21))
;; (1 3 7 9 11 13 17 19)
;; sicp.ch1.s3> (* 1 3 7 9 11 13 17 19)
;; 8729721
;; sicp.ch1.s3> (product-relative-prime 20)
;; 8729721


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.34
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn f [g]
  (g 2))

;; (f f) seems silly. it will call f with 2 and then try to use 2 as a procedure




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.3 Procedures as General Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; SInce lambda and let have been covered,going forward let or letfn
;; will be used instead of writing defn within another defn
;; loop/recur also may not a bad idea

(defn average [a b]
  (/ (+ a b) 2.0))

(defn close-enough? [a b]
  (< (Math/abs (- a b)) 0.001) )

(defn search-hi [f neg-point pos-point]
  (let [mid-point (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      mid-point
      (let [mid-val (f mid-point)]
        (cond (pos? mid-val) (search-hi f neg-point mid-point)
              (neg? mid-val) (search-hi f mid-point pos-point)
              :else mid-point)))))

(defn half-interval-method [f a b]
  (let [first-val (f a)
        second-val (f b)]
    (cond
      (and (pos? first-val) (neg? second-val)) (search-hi f b a)
      (and (neg? first-val) (pos? second-val)) (search-hi f a b)
      :else (throw (RuntimeException.
                    (str "Values are not of opposite sign " a  "," b))))))


;; sicp.ch1.s3> (half-interval-method #(Math/sin %) 2 4)
;; 3.14111328125

;; sicp.ch1.s3> (half-interval-method (fn [x] (- (* x x x) (* 2 x) 3))
;;                                    1.0
;;                                    2.0)
;; 1.89306640625



(def ^:dynamic *tolerance*  0.00001)

(defn fixed-point [f first-guess]
  (defn try-it [current-guess]
    (let [next-guess (f current-guess)]
              (if (< (Math/abs (- next-guess current-guess)) *tolerance*)
                next-guess
                (recur next-guess))))
  (try-it first-guess ))

;; Internal procedure defintions with defn are so ugly!
;; Maybe letfn will make it look simpler?
(defn fixed-point [f first-guess]
  (letfn [(try-it [current-guess]
            (let [next-guess (f current-guess)]
              (if (< (Math/abs (- next-guess current-guess)) *tolerance*)
                next-guess
                (recur next-guess))
              ))]
    (try-it first-guess )))
;; Ugh, not so much!


;; I assume this is a good point to start using loop/recur
(defn fixed-point [f first-guess]
  (loop [current-guess first-guess
         next-guess (f first-guess)]
    (println "next-guess: " next-guess)
    (if (< (Math/abs (- next-guess current-guess)) *tolerance*)
      next-guess
      (recur next-guess (f next-guess)))))



;; sicp.ch1.s3> (fixed-point #(Math/cos %) 1.0)
;; 0.7390822985224024

;; sicp.ch1.s3> (fixed-point (fn [y] (+ (Math/sin y) (Math/cos y)))
;;                           1.0)
;; 1.2587315962971173


(defn sqrt [x]
  (fixed-point (fn [y] (/ x y)) 1.0))

(defn avg [a b]
  (/ (+ a b) 2.0))

(defn sqrt [x]
  (fixed-point (fn [y] (avg y (/ x y))) 1.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Definition of Sqrt from Section 1.17
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square [x]
  (* x x))

(defn good-enough? [guess x]
  (< (Math/abs (- (square guess) x))  0.00001))

(defn average [a b]
  (/ (+ a b) 2.0))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter [guess x]
  (println "guess: " guess)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.35
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; definition of golden ratio is thatit satisfies equation (x * x) = (x + 1)
;; same as x -> 1 + 1/x

(defn gldnrto []
  (fixed-point (fn [y] (+ 1 (/ 1 y))) 1.0 ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.36
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; x^x=1000
;; x = log 1000 / log x
(defn x-raised-x [x]
  (fixed-point (fn [y] (/ (Math/log x) (Math/log y))) 1.1))

;; sicp.ch1.s3> (x-raised-x 1000)
;; next-guess:  72.47657378429035
;; next-guess:  1.6127318474109593
;; next-guess:  14.45350138636525
;; next-guess:  2.5862669415385087
;; next-guess:  7.269672273367045
;; next-guess:  3.4822383620848467
;; next-guess:  5.536500810236703
;; next-guess:  4.036406406288111
;; next-guess:  4.95053682041456
;; next-guess:  4.318707390180805
;; next-guess:  4.721778787145103
;; next-guess:  4.450341068884912
;; next-guess:  4.626821434106115
;; next-guess:  4.509360945293209
;; next-guess:  4.586349500915509
;; next-guess:  4.535372639594589
;; next-guess:  4.568901484845316
;; next-guess:  4.546751100777536
;; next-guess:  4.561341971741742
;; next-guess:  4.551712230641226
;; next-guess:  4.558059671677587
;; next-guess:  4.55387226495538
;; next-guess:  4.556633177654167
;; next-guess:  4.554812144696459
;; next-guess:  4.556012967736543
;; next-guess:  4.555220997683307
;; next-guess:  4.555743265552239
;; next-guess:  4.555398830243649
;; next-guess:  4.555625974816275
;; next-guess:  4.555476175432173
;; next-guess:  4.555574964557791
;; next-guess:  4.555509814636753
;; next-guess:  4.555552779647764
;; next-guess:  4.555524444961165
;; next-guess:  4.555543131130589
;; next-guess:  4.555530807938518
;; next-guess:  4.555538934848503
;; 4.555538934848503



;; x+x = x + (log 1000 / log x)
;; x = 1/2 * [x +  (log 1000 / log x)]
(defn x-raised-x [x]
  (fixed-point (fn [y] (avg y (/ (Math/log x) (Math/log y)))) 1.1))

;; sicp.ch1.s3> (x-raised-x 1000)
;; next-guess:  36.78828689214517
;; next-guess:  19.352175531882512
;; next-guess:  10.84183367957568
;; next-guess:  6.870048352141772
;; next-guess:  5.227224961967156
;; next-guess:  4.701960195159289
;; next-guess:  4.582196773201124
;; next-guess:  4.560134229703681
;; next-guess:  4.5563204194309606
;; next-guess:  4.555669361784037
;; next-guess:  4.555558462975639
;; next-guess:  4.55553957996306
;; next-guess:  4.555536364911781
;; 4.555536364911781





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
   (if (= k 0)
     res
     (recur n d (dec k) (/ (n k) (+ (d k) res))))))

;; sicp.ch1.s3> (cont-frac (fn [i] 1.0) (fn [i] 1.0) 13)
;; 0.6180371352785146
;; sicp.ch1.s3> (cont-frac (fn [i] 1.0) (fn [i] 1.0) 11)
;; 0.6180555555555556
;; (cont-frac (constantly 1) (constantly 1) 500)
;; k of 12 provides answer to accuracy of 4 decimal places



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.38
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e = 2.71828182846


;; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ....

;; 2nd term  -> 1 * 2  
;; 5th term  -> 2 * 2
;; 8         -> 3 * 2
;; 11        -> 4 * 2
;; 14        -> 5 * 2

(defn denom-euler [n]
  (if (== 0 (mod (inc n) 3))
    (* 2.0 (quot (inc n) 3))
    1.0))


(defn euler-cf []
  (+ 2.0 (cont-frac (constantly 1.0) denom-euler 100)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.39
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Using overloaded method definition
(defn cont-frac 
  ([n d k] (cont-frac n d k + 0.0))
  ([n d k f] (cont-frac n d k f 0.0))
  ([n d k f res]
   (if (= k 0)
     res
     (recur n d (dec k) f (/ (n k) (f (d k) res))))))

;; using loop/recur
(defn cont-frac 
  ([n d k] (cont-frac n d k + 0.0))
  ([n d k f null-value]
   (loop [k k
          res null-value]
     (if (= k 0)
       res
       (recur (dec k) (/ (n k) (f (d k) res)))))))

(defn tan-cf [x k]
  (cont-frac
   (fn [i] (Math/pow x (if (= i 1) 1 2)))
   (fn [i] (dec (* 2 i)))
   k
   -
   0.0))

;; sicp.ch1.s3> (Math/tan 1.0)
;; 1.5574077246549023
;; sicp.ch1.s3> (tan-cf 1.0 10)
;; 1.557407724654902




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Procedures as Returned Values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn avg [a b]
  (/ (+ a b) 2.0))

(defn average-damp [f]
  (fn [x] (avg x (f x))))

(def ^:dynamic *tolerance*  0.000001)

(defn fixed-point [f first-guess]
  (loop [current-guess first-guess
         next-guess (f first-guess)]
    (println "next-guess: " next-guess)
    (if (< (Math/abs (- next-guess current-guess)) *tolerance*)
      next-guess
      (recur next-guess (f next-guess)))))

(defn sqrt [x]
  (fixed-point (fn [y] (avg y (/ x y))) 1.0))

(defn sqrt [x]
  (fixed-point (average-damp (fn [y] (/ x y)))
               1.0))

(defn cube-root [x]
  (fixed-point (average-damp (fn [y] (/ x (* y y))))
               1.0))


;;;;;;; Neton's Method ;;;;;;

(def ^:dynamic *dx*  0.000001)

(defn deriv [g]
  (fn [x]
    (/ (- (g (+ x *dx*)) (g x))
       *dx*)))

(defn square [x] (* x x))

(defn cube [x] (* x x x))

;;((deriv cube) 5)
;; 75.00014999664018

;; The thing to remember about Newton's method is that
;; we have to express the equation as g(x) = 0
;; That is we are trying to find the root!
;; y = x/y
;; y^2 = x
;; Y^2 - x = 0
(defn newton-transform [g]
  (fn [x]
    (- x
       (/ (g x)
          ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn sqrt [x]
  (newtons-method (fn [y] (- (square y) x))
                  1.0))

;; Hmm, following definition results in 'divide by zero' exception
(defn sqrt [x]
  (newtons-method (fn [y] (/ x y))
                  1.0))



(defn fixed-point-of-transform [g transform guess]
  (fixed-point (transform g) guess))


(defn sqrt [x]
  (fixed-point-of-transform (fn [y] (/ x y))
                            average-damp
                            1.0))

;; sicp.ch1.s3> (sqrt 144.0)
;; next-guess:  72.5
;; next-guess:  37.24310344827586
;; next-guess:  20.554795555442038
;; next-guess:  13.7802299905638
;; next-guess:  12.11499150672641
;; next-guess:  12.000545730742438
;; next-guess:  12.000000012408687
;; next-guess:  12.0
;; 12.0

(defn sqrt [x]
  (fixed-point-of-transform (fn [y] (- (square y) x))
                            newton-transform
                            1.0))

;; sicp.ch1.s3> (sqrt 144.0)
;; next-guess:  72.49996461787373
;; next-guess:  37.243086414059384
;; next-guess:  20.554788099216534
;; next-guess:  13.780227704616813
;; next-guess:  12.114991288777757
;; next-guess:  12.00054573328618
;; next-guess:  12.00000001243097
;; next-guess:  12.0
;; 12.0

(defn cube-root [x]
  (fixed-point-of-transform (fn [y] (- (cube y) x))
                            newton-transform
                            1.0))

;; sicp.ch1.s3> (cube-root 8)
;; next-guess:  3.3333310001595553
;; next-guess:  2.4622212642840395
;; next-guess:  2.0813411060566223
;; next-guess:  2.0031375260791937
;; next-guess:  2.0000049133240116
;; next-guess:  2.0000000000145284
;; next-guess:  2.0
;; 2.0



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.40
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function is x3 + ax2 + bx + c
;;(newtons-method (cubic a b c) 1)

(defn cubic [a b c]
  (fn [x]
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;; (newtons-method (cubic 1 1 1) 1.0)
;; -0.999999999999977


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.41
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn double [f]
  (fn [x] (f (f x))))

;; (((double (double double)) inc) 5)
;; 21



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compose [f g]
  (fn [x] (f (g x))))

;; ((compose square inc) 6)
;; 49



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.43
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn repeated [f n]
  (fn [x]
    (defn try-it [cntr res]
      (if (<= cntr 1) 
        res
        (recur (dec cntr) (f res))))
    (try-it n (f x))))


;; Same as  above procedure but with loop/recur
(defn repeated [f n]
  (fn [x]
    (loop [cntr n
           res (f x)]
      (if (<= cntr 1) 
        res
        (recur (dec cntr) (f res))))))


;; using compose
;; Yikes! This definition fails for ((repeated (fn [x] (inc x)) 1000000) 2)
;; with stack overflow error
(defn repeated [f n]
  (loop [cntr n
         res f]
    (if (<= cntr 1) 
      res
      (recur (dec cntr) (compose f res)))))



;; recurive definition. This will fail for large values of n as well
(defn repeated [f n]
  (if (<= n 1)
    f
    (compose f (repeated f (dec n)))))



;; ((repeated square 2) 5)
;; 625

;; ((repeated (fn [x] (inc x)) 5) 2)
;; 7


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.44
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *dx*  0.0001)

(defn smoothed [f]
  (fn [x]
    (/ (+ (f (- x *dx*))
          (f x)
          (f (+ x *dx*)))
       3)))


(defn n-fold-smoothed [f n]
  ((repeated smoothed n) f))

;; sicp.ch1.s3> ((n-fold-smoothed square 5) 4)
;; 16.000000000003336


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Analysis on how n-fold-smoothing works
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(def ^:dynamic *dx*  0.01)

(defn my-square [x]
  (println "My-Square: Squaring " x " to produce " (* x x))
  (* x x))

(defn smoothed [f]
  (println "Smoothed: Called with parameter function: " f)
  (letfn [(mr-smooth [x]
            (println "Mr-Smooth: Smoothing out " f " on " x )
            (let [ans (/ (+ (f (- x *dx*))
                            (f x)
                            (f (+ x *dx*)))
                         3)]
              (println "Mr-Smooth: Smoothed out " f " on " x " with result " ans)
              ans))]
    (println "Smoothed: Returning function: " mr-smooth)
    mr-smooth))

;; sicp.ch1.s3> ((n-fold-smoothed my-square 2) 4)
;; Smoothed: Called with parameter function:  #function[sicp.ch1.s3/my-square]
;; Smoothed: Returning function:  #function[sicp.ch1.s3/smoothed/mr-smooth--11219]
;; Smoothed: Called with parameter function:  #function[sicp.ch1.s3/smoothed/mr-smooth--11219]
;; Smoothed: Returning function:  #function[sicp.ch1.s3/smoothed/mr-smooth--11219]
;; Mr-Smooth: Smoothing out  #function[sicp.ch1.s3/smoothed/mr-smooth--11219]  on  4
;; Mr-Smooth: Smoothing out  #function[sicp.ch1.s3/my-square]  on  3.99
;; My-Square: Squaring  3.9800000000000004  to produce  15.840400000000004
;; My-Square: Squaring  3.99  to produce  15.920100000000001
;; My-Square: Squaring  4.0  to produce  16.0
;; Mr-Smooth: Smoothed out  #function[sicp.ch1.s3/my-square]  on  3.99  with result  15.920166666666669
;; Mr-Smooth: Smoothing out  #function[sicp.ch1.s3/my-square]  on  4
;; My-Square: Squaring  3.99  to produce  15.920100000000001
;; My-Square: Squaring  4  to produce  16
;; My-Square: Squaring  4.01  to produce  16.080099999999998
;; Mr-Smooth: Smoothed out  #function[sicp.ch1.s3/my-square]  on  4  with result  16.000066666666665
;; Mr-Smooth: Smoothing out  #function[sicp.ch1.s3/my-square]  on  4.01
;; My-Square: Squaring  4.0  to produce  16.0
;; My-Square: Squaring  4.01  to produce  16.080099999999998
;; My-Square: Squaring  4.02  to produce  16.160399999999996
;; Mr-Smooth: Smoothed out  #function[sicp.ch1.s3/my-square]  on  4.01  with result  16.080166666666667
;; Mr-Smooth: Smoothed out  #function[sicp.ch1.s3/smoothed/mr-smooth--11219]  on  4  with result  16.000133333333334
;; 16.000133333333334


(defn smoothed [f]
  (println "Smoothed: Called with parameter function: " f)
  (letfn [(mr-smooth [x]
            (println "Mr-Smooth: Smoothing out " f " on " x )
            (let [ans (/ (+ (f (- x *dx*))
                            (f x)
                            (f (+ x *dx*)))
                         3)]
              (println "Mr-Smooth: Smoothed out " f " on " x " with result " ans)
              ans))]
    (println "Smoothed: Returning function: " mr-smooth)
    mr-smooth))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.45
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
