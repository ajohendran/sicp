(ns sicp.ch1.s2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.2.1  Linear Recursion and Iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn plus-rec [a b]
  (if (= a 0)
    b
    (inc (plus-rec (dec a) b))))

;; Using substitution model fpr (plus-rec 4 5)
;;
;; (plus-rec 4 5)
;; (inc (plus-rec 3 5))
;; (inc (inc (plus-rec 2 5)))
;; (inc (inc (inc (plus-rec 1 5))))
;; (inc (inc (inc (inc (plus-rec 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; CLearly this process has a recursion shape


(defn plus-itr [a b]
  (if (= a 0)
    b
    (plus-itr (dec a) (inc b))))

;; Using substitution model fpr (plus-itr 4 5)
;;
;; (plus-itr 4 5)
;; (plus-itr 3 6)
;; (plus-itr 2 7)
;; (plus-itr 1 8)
;; (plus-itr 0 9)
;; 9

;; Clearly the above process is iterative since state variables summarize the
;; state of the computation



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn Ack [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (Ack (- x 1) (Ack x (- y 1)))))

;; (Ack 1 10) => 1024

;; Trace for (Ack 1 10)

;; (sicp.ch1.s2/Ack 1 10)
;; | (sicp.ch1.s2/Ack 1 9)
;; | | (sicp.ch1.s2/Ack 1 8)
;; | | | (sicp.ch1.s2/Ack 1 7)
;; | | | | (sicp.ch1.s2/Ack 1 6)
;; | | | | | (sicp.ch1.s2/Ack 1 5)
;; | | | | | | (sicp.ch1.s2/Ack 1 4)
;; | | | | | | | (sicp.ch1.s2/Ack 1 3)
;; | | | | | | | | (sicp.ch1.s2/Ack 1 2)
;; | | | | | | | | | (sicp.ch1.s2/Ack 1 1)
;; | | | | | | | | | => 2
;; | | | | | | | | | (sicp.ch1.s2/Ack 0 2)
;; | | | | | | | | | => 4
;; | | | | | | | | => 4
;; | | | | | | | | (sicp.ch1.s2/Ack 0 4)
;; | | | | | | | | => 8
;; | | | | | | | => 8
;; | | | | | | | (sicp.ch1.s2/Ack 0 8)
;; | | | | | | | => 16
;; | | | | | | => 16
;; | | | | | | (sicp.ch1.s2/Ack 0 16)
;; | | | | | | => 32
;; | | | | | => 32
;; | | | | | (sicp.ch1.s2/Ack 0 32)
;; | | | | | => 64
;; | | | | => 64
;; | | | | (sicp.ch1.s2/Ack 0 64)
;; | | | | => 128
;; | | | => 128
;; | | | (sicp.ch1.s2/Ack 0 128)
;; | | | => 256
;; | | => 256
;; | | (sicp.ch1.s2/Ack 0 256)
;; | | => 512
;; | => 512
;; | (sicp.ch1.s2/Ack 0 512)
;; | => 1024
;; => 1024


;; (Ack 2 4) => 65536

;; (Ack 3 3) => 65536

(defn f [n] (Ack 0 n)) ;; => 2n

(defn g [n] (Ack 1 n)) ;; => (exp 2 n)

(defn h [n] (Ack 2 n)) ;; =>

;; (Ack 2 0) => 0
;; (Ack 2 1) => 2
;; (Ack 2 2) => (Ack 1 2) => (Ack 0 2) => 4
;; (Ack 2 3) => (Ack 1 4) ==> (Ack 0 8) => 16
;; (Ack 2 4) ==> (Ack 1 16) ==> 65536
;;           ==> (Ack 1 (Ack 2 (- n 1)))
;;           ==> (g (Ack 2 (- n 1)))
;;           ==> (g (h (- n 1)))
;;           ==> (exp 2 (h (- n 1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example: Counting change
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn first-denomination [kinds-of-coins]
  (cond (= kinds-of-coins 1) 1
        (= kinds-of-coins 2) 5
        (= kinds-of-coins 3) 10
        (= kinds-of-coins 4) 25
        :else 50))

(defn cc [amount kinds-of-coins]
  (cond (= amount 0) 1
        (or (< amount 0) (= kinds-of-coins 0)) 0
        :else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins))))

(defn cc-2 [amount kinds-of-coins]
  (cond (or (= amount 0) (= kinds-of-coins 1)) 1
        (or (< amount 0) (= kinds-of-coins 0)) 0
        :else (+ (cc-2 amount
                       (- kinds-of-coins 1))
                 (cc-2 (- amount (first-denomination kinds-of-coins))
                       kinds-of-coins))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.12 Pascal's Triangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn pascal [r c]
  (cond (or (< r 1) (< c 1) (> c r)) -1
        (or (= c 1) (= c r)) 1
        :else (+ (pascal (dec r) (dec c))
                 (pascal (dec r) c))))

(defn print-pascal [n]
  (for [r (range 1 (inc n))
        c (map inc (range r))]
    (do
      (if (= c 1) 
        (print (apply str (take (- (inc n) r) (repeat "   ")))))
      (print (pascal r c)) 
      (if (= r c) (println) (print "    ")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.14 Coin Counting Order of Growth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Amount —> N
;; Denominations —> d1(penny), d2(nickel), d3(dime), d4(quarter), d5(half-dollar)
;; cc(N,d) will be represented as [N,d]


;; Analysis:

;; With only denomination d1:
;; [N,d1]
;; Order of growth —> N steps


;; With denominations d1 & d2:
;; [N,d2]
;; [N-d2,d2] + [N,d1]
;; [N-2*d2,d2] + [N-d2,d1] + [N,d1]
;; [N-3*d2,d2] + [N-2*d2,d1] + [N-d2,d1] + [N,d1]
;; [N-4*d2,d2] + [N-3*d2,d1] + [N-2*d2,d1] + [N-d2,d1] + [N,d1]

;; For sufficiently large values of N, values such as [N-d2] , [N-2*d2] , [N-3*d2] , etc. are pretty close to N.
;; They can be considered to be same as N. So we have 

;; [N-4*d2,d2] + [N,d1] + [N,d1] + [N,d1] + [N,d1]
;; [N-4*d2,d2] + 4N

;; This means, for every time we subtract d2 from N, we have N steps.
;; Order of growth —> N/d2*N —> N^2 steps.


;; With denominations d1, d2 & d3:
;; [N-d3,d3] + [N,d2]
;; [N-2*d3,d3] + [N-d3,d2] + [N,d2]
;; [N-3*d3,d3] + [N-2*d3,d2] + [N-d3,d2] + [N,d2]
;; [N-4*d3,d3] + [N-3*d3,d2] + [N-2*d3,d2] + [N-d3,d2] + [N,d2]

;; Similar to previous analysis, for sufficiently large N, N-4*d3 can be considered just N.
;; [N-4*d3,d3] + [N,d2] + [N,d2] + [N,d2] + [N,d2]
;; [N-4*d3,d3] + 4[N,d2]

;; So, for every time we subtract d3 from N, we have to calculate steps for [N,d2]
;; Order of growth —> N/d3*N^2 —> N^3 steps.

;; The sequence of steps would be similar with more denominations. For 5 coins we have N^5 steps.
;; In general, for d different coins, the number of steps is roughly N^d.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.15 Sine , Order of growth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cube [x] (* x x x))

(defn p [x] (- (* 3 x) (* 4 (cube x))))

(defn sine [angle]
  (if (not (> (Math/abs angle) 0.01))
    angle
    (p (sicp.ch1.s2/sine (/ angle 3.0)))))
;; namespace qualifying so function can be traced in clojurescript


;; a. For (sine 12.15), procedure p is applied 5 times. See trace below.
;; (sine 12.15)
;; TRACE t10663: (sicp.ch1.s2/sine 12.15)
;; TRACE t10664: | (sicp.ch1.s2/sine 4.05)
;; TRACE t10665: | | (sicp.ch1.s2/sine 1.3499999999999999)
;; TRACE t10666: | | | (sicp.ch1.s2/sine 0.44999999999999996)
;; TRACE t10667: | | | | (sicp.ch1.s2/sine 0.15)
;; TRACE t10668: | | | | | (sicp.ch1.s2/sine 0.049999999999999996)
;; TRACE t10669: | | | | | | (sicp.ch1.s2/sine 0.016666666666666666)
;; TRACE t10670: | | | | | | | (sicp.ch1.s2/sine 0.005555555555555556)
;; TRACE t10670: | | | | | | | => 0.005555555555555556
;; TRACE t10671: | | | | | | | (sicp.ch1.s2/p 0.005555555555555556)
;; TRACE t10671: | | | | | | | => 0.016665980795610425
;; TRACE t10669: | | | | | | => 0.016665980795610425
;; TRACE t10672: | | | | | | (sicp.ch1.s2/p 0.016665980795610425)
;; TRACE t10672: | | | | | | => 0.04997942615445553
;; TRACE t10668: | | | | | => 0.04997942615445553
;; TRACE t10673: | | | | | (sicp.ch1.s2/p 0.04997942615445553)
;; TRACE t10673: | | | | | => 0.1494388954247979
;; TRACE t10667: | | | | => 0.1494388954247979
;; TRACE t10674: | | | | (sicp.ch1.s2/p 0.1494388954247979)
;; TRACE t10674: | | | | => 0.4349676185073074
;; TRACE t10666: | | | => 0.4349676185073074
;; TRACE t10675: | | | (sicp.ch1.s2/p 0.4349676185073074)
;; TRACE t10675: | | | => 0.9757248787040262
;; TRACE t10665: | | => 0.9757248787040262
;; TRACE t10676: | | (sicp.ch1.s2/p 0.9757248787040262)
;; TRACE t10676: | | => -0.7885380669825341
;; TRACE t10664: | => -0.7885380669825341
;; TRACE t10677: | (sicp.ch1.s2/p -0.7885380669825341)
;; TRACE t10677: | => -0.40438666108762367
;; TRACE t10663: => -0.40438666108762367
;; -0.40438666108762367

;; b.Order of growth. 
;;   We repeatedly divide until the quotient is less than 01.
;;   That is, 
;;    a / (3^n) < 0.1
;;   a * 10 < 3^n
;;   Taking log base 3
;;   log (a * 10) base 3 < n * log (3) base 3
;;   n > log (a * 10) base 3
;;   So order of growth is ceiling[log(a*10) base 3]
;;   Since order of growth is defined as k1*f(n) < R(n) < k2f(n) 
;;   where k1 and k2 are arbitrary constants, the base of log really doesn't matter
;;   So order of growth is essentially log(a), that is logarithmic in a.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.16 Exponential
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expt-r [b n]
  (if (= n 1) 
    b
    (* b (sicp.ch1.s2/expt-r b (dec n)))))

(defn expt-i
  ([b n] (sicp.ch1.s2/expt-i b n 1))
  ([b n v] 
   (if (= n 0) 
     v
     (recur b (dec n) (* b v)))))

;; clojure already has even? defined in clojure.core namespace
;; (defn even?
;;   "Returns true if n is even, throws an exception if n is not an integer"
;;   {:added "1.0"
;;    :static true}
;;    [n] (if (integer? n)
;;         (zero? (bit-and (clojure.lang.RT/uncheckedLongCast n) 1))
;;         (throw (IllegalArgumentException. (str "Argument must be an integer: " n)))))


(defn square [n] (* n n))

(defn fast-expt-r [b n]
  (cond
    (= n 0) 1
    (even? n) (square (sicp.ch1.s2/fast-expt-r b (/ n 2)))
    :else (* b (sicp.ch1.s2/fast-expt-r b (dec n)))))

;; sicp.ch1.s2> (fast-expt-r 4 5)
;; TRACE t11383: (sicp.ch1.s2/fast-expt-r 4 5)
;; TRACE t11384: | (sicp.ch1.s2/fast-expt-r 4 4)
;; TRACE t11385: | | (sicp.ch1.s2/fast-expt-r 4 2)
;; TRACE t11386: | | | (sicp.ch1.s2/fast-expt-r 4 1)
;; TRACE t11387: | | | | (sicp.ch1.s2/fast-expt-r 4 0)
;; TRACE t11387: | | | | => 1
;; TRACE t11386: | | | => 4
;; TRACE t11388: | | | (sicp.ch1.s2/square 4)
;; TRACE t11388: | | | => 16
;; TRACE t11385: | | => 16
;; TRACE t11389: | | (sicp.ch1.s2/square 16)
;; TRACE t11389: | | => 256
;; TRACE t11384: | => 256
;; TRACE t11383: => 1024
;; 1024



