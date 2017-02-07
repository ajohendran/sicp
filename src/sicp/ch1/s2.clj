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



(defn fast-expt-i [b n a]
  (cond (< n 1) a
        (even? n) (fast-expt-i (* b b) (/ n 2) a)
        :else (fast-expt-i b (dec n) (* a b)))) ;; use 'recur' for real tail recursion on the JVM


;; sicp.ch1.s2> (fast-expt-i 2 37 1)
;; TRACE t11000: (sicp.ch1.s2/fast-expt-i 2 37 1)
;; TRACE t11001: | (sicp.ch1.s2/fast-expt-i 2 36 2)
;; TRACE t11002: | | (sicp.ch1.s2/fast-expt-i 4 18 2)
;; TRACE t11003: | | | (sicp.ch1.s2/fast-expt-i 16 9 2)
;; TRACE t11004: | | | | (sicp.ch1.s2/fast-expt-i 16 8 32)
;; TRACE t11005: | | | | | (sicp.ch1.s2/fast-expt-i 256 4 32)
;; TRACE t11006: | | | | | | (sicp.ch1.s2/fast-expt-i 65536 2 32)
;; TRACE t11007: | | | | | | | (sicp.ch1.s2/fast-expt-i 4294967296 1 32)
;; TRACE t11008: | | | | | | | | (sicp.ch1.s2/fast-expt-i 4294967296 0 137438953472)
;; TRACE t11008: | | | | | | | | => 137438953472
;; TRACE t11007: | | | | | | | => 137438953472
;; TRACE t11006: | | | | | | => 137438953472
;; TRACE t11005: | | | | | => 137438953472
;; TRACE t11004: | | | | => 137438953472
;; TRACE t11003: | | | => 137438953472
;; TRACE t11002: | | => 137438953472
;; TRACE t11001: | => 137438953472
;; TRACE t11000: => 137438953472
;; 137438953472






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.17 Multiplication in terms of addition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn dbl [n] (+ n n)) ;; work double already used in clojure core
(defn halve [n] (/ n 2))

;; just thinking about nature of arithmetics &
;; recursion with following fuctions
(defn add-i [a b]
  (if (= b 0)
    a
    (add-i (inc a) (dec b)))) ;; use 'recur' for real tail recursion on the JVM

(defn add-r [a b]
  (if (= b 0)
    a
    (inc (add-r a (dec b))))) 


(defn mult-r [a b]
  (if (= b 0)
    0
    (+ a (mult-r a (dec b)))))


(defn mult-i [a b v]
  (if (= b 0)
    v
    (recur a (dec b) (+ a v))))


(defn fast-mult-r [a b]
  (cond
    (= b 1) a
    (even? b) (dbl (fast-mult-r a (halve b))) 
    :else (+ a (fast-mult-r a (dec b)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.18 Fast multiplication by iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fast-mult-i [a b v]
  (cond
    (= b 0) v
    (even? b) (fast-mult-i (dbl a) (halve b) v) 
    :else (fast-mult-i a (dec b) (+ a v) )))


;; sicp.ch1.s2> (fast-mult-i 25 5 0)
;; TRACE t11062: (sicp.ch1.s2/fast-mult-i 25 5 0)
;; TRACE t11063: | (sicp.ch1.s2/fast-mult-i 25 4 25)
;; TRACE t11064: | | (sicp.ch1.s2/fast-mult-i 50 2 25)
;; TRACE t11065: | | | (sicp.ch1.s2/fast-mult-i 100 1 25)
;; TRACE t11066: | | | | (sicp.ch1.s2/fast-mult-i 100 0 125)
;; TRACE t11066: | | | | => 125
;; TRACE t11065: | | | => 125
;; TRACE t11064: | | => 125
;; TRACE t11063: | => 125
;; TRACE t11062: => 125
;; 125




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.19 Fibonacci in logarithmic steps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; 0,1,1,2,3,5,8,13,21,34,55 .....


;; (defn fib
;;   ([n] (fib 1 0 0 1 n))
;;   ([a b p q c]
;;    (cond (= c 0) b
;;          (even? c) (fib a
;;                         b
;;                         ?
;;                         ?
;;                         (/ c 2))
;;          :else (fib (+ (* b q) (* a q) (* a p))
;;                     (+ (* b p) (* a q))
;;                     p
;;                     q
;;                     (dec c)))))
;; (fib 2)
;; (fib 1 0 p q 2)
;; (fib 1 0 p1 q1 1)
;; (fib [q1+p1] [q1] 0)
;;q'=1

;; (fib 3)
;; (fib 1 0 p q 3) 
;; (fib 1 1 p q 2)
;; (fib 1 1 p1 q1 1)
;; (fib [2q1+p1] [p1+q1] p1 q1 0)
;;p1+q1 = 2
;; therefore p1=1, q1=1

;; (fib 4)
;; (fib 1 0 0 1 4)
;; (fib 1 0 p1 q1 2)
;; (fib 1 0 p2 q2 1)
;; (fib [q2+p2] [q2] p2 q2 0)
;;q2 = 3

;; (fib 5)
;; (fib 1 0 0 1 5)
;; (fib 1 1 0 1 4)
;; (fib 1 1 p' q' 2)
;; (fib 1 1 p'' q'' 1)
;; (fib [2q2+p2] [p2+q2] p2 q2 0)
;;p2+q2 = 5
;;therefore p2 = 2, q2=3

;; From above analysis, it can be seen that p and q go up a level
;; when n = 2^x. Therefore it doesn't make sense to analyse for
;; n values of 6,7,9,10,11,12,13,14,15,17 etc.

;; (fib 8)
;; (fib 1 0 p q 8)
;; (fib 1 0 p1 q1 4)
;; (fib 1 0 p2 q2 2)
;; (fib 1 0 p3 q3 1)
;; (fib [q3+p3]
;;      [q3]
;;      p3
;;      q3
;;      0)
;;q3 = 21

;; (fib 9)
;; (fib 1 0 0 1 9)
;; (fib 1 1 p q 8)
;; (fib 1 1 p1 q1 4)
;; (fib 1 1 p2 q2 2)
;; (fib 1 1 p3 q3 1)
;; (fib [21+21+p3]
;;      [p3+21]
;;      p3
;;      21
;;      0)
;; p3+21 = 34
;; therefore p3 = 13

;; Based on above analysis, we can see q4 will be (fib 16) -> 987
;; p4 will be (- (fib 17) q4)  -> 1597 - 987 -> 610

;; So we have
;; p0, q0 -> 0,1      -> fib0 , fib1
;; p1, q1 -> 1,1      -> fib1 , fib2
;; p2, q2 -> 2,3      -> fib3 , fib4
;; p3, q3 -> 13,21    -> fib7 , fib8
;; p4, q4 -> 610, 987 -> fib15, fib16


;;If we start with a=2 , b=1  and apply the transformation
;; with p=1, q=1
;; we end up with a=5 and b=3. So we jumped 2 spots


;; if we start with a=2, b=1 and apply transformation
;; with  p=2 , q=3
;; we end up with a=8 , b=13. We jumped 4 spots!!

;; if we start with a=2, b=1 and apply transformation
;; with  p=13 , q=21
;; we end up with a=55 , b=89. We jumped 8 spots!!!

;; So it can be seen now how this algorithm works. Based on the value
;; of p and q, we can jump very fast from one fibonacci number to the
;; another that is much further away in just one transformation
;; All we need to figure out now is how p and q update themselves.

;; Carefully looking at table below we see that the values of p and q
;; themselves are fibonnaci numbers and they are jumping up
;; exactly as if the transformations were applied to themselves!!
;; So that's the key! Just note with our transformation algorithm,
;; a,b -> 1,0 and p0,q0 -> 0,1.  So we will need to reverse things in the code

;; p0, q0 -> 0,1      -> fib0 , fib1
;; p1, q1 -> 1,1      -> fib1 , fib2
;; p2, q2 -> 2,3      -> fib3 , fib4
;; p3, q3 -> 13,21    -> fib7 , fib8
;; p4, q4 -> 610, 987 -> fib15, fib16

;; that is the transformation will be


(defn fib
  ([n] (fib 1 0 0 1 n))
  ([a b p q c]
   (cond (= c 0) b
         (even? c) (fib a
                        b
                        (+ (* p p) (* q q))
                        (+ (* p q) (* q q) (* q p))
                        (/ c 2))
         :else (fib (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p
                    q
                    (dec c)))))


;; sicp.ch1.s2> (fib 3)
;; TRACE t10866: (sicp.ch1.s2/fib 3)
;; TRACE t10867: | (sicp.ch1.s2/fib 1 0 0 1 3)
;; TRACE t10868: | | (sicp.ch1.s2/fib 1 1 0 1 2)
;; TRACE t10869: | | | (sicp.ch1.s2/fib 1 1 1 1 1)
;; TRACE t10870: | | | | (sicp.ch1.s2/fib 3 2 1 1 0)
;; TRACE t10870: | | | | => 2
;; TRACE t10869: | | | => 2
;; TRACE t10868: | | => 2
;; TRACE t10867: | => 2
;; TRACE t10866: => 2
;; 2
;; sicp.ch1.s2> (fib 5)
;; TRACE t10873: (sicp.ch1.s2/fib 5)
;; TRACE t10874: | (sicp.ch1.s2/fib 1 0 0 1 5)
;; TRACE t10875: | | (sicp.ch1.s2/fib 1 1 0 1 4)
;; TRACE t10876: | | | (sicp.ch1.s2/fib 1 1 1 1 2)
;; TRACE t10877: | | | | (sicp.ch1.s2/fib 1 1 2 3 1)
;; TRACE t10878: | | | | | (sicp.ch1.s2/fib 8 5 2 3 0)
;; TRACE t10878: | | | | | => 5
;; TRACE t10877: | | | | => 5
;; TRACE t10876: | | | => 5
;; TRACE t10875: | | => 5
;; TRACE t10874: | => 5
;; TRACE t10873: => 5
;; 5
;; sicp.ch1.s2> (fib 8)
;; TRACE t10839: (sicp.ch1.s2/fib 8)
;; TRACE t10840: | (sicp.ch1.s2/fib 1 0 0 1 8)
;; TRACE t10841: | | (sicp.ch1.s2/fib 1 0 1 1 4)
;; TRACE t10842: | | | (sicp.ch1.s2/fib 1 0 2 3 2)
;; TRACE t10843: | | | | (sicp.ch1.s2/fib 1 0 13 21 1)
;; TRACE t10844: | | | | | (sicp.ch1.s2/fib 34 21 13 21 0)
;; TRACE t10844: | | | | | => 21
;; TRACE t10843: | | | | => 21
;; TRACE t10842: | | | => 21
;; TRACE t10841: | | => 21
;; TRACE t10840: | => 21
;; TRACE t10839: => 21
;; 21
;; sicp.ch1.s2> (fib 13)
;; TRACE t10847: (sicp.ch1.s2/fib 13)
;; TRACE t10848: | (sicp.ch1.s2/fib 1 0 0 1 13)
;; TRACE t10849: | | (sicp.ch1.s2/fib 1 1 0 1 12)
;; TRACE t10850: | | | (sicp.ch1.s2/fib 1 1 1 1 6)
;; TRACE t10851: | | | | (sicp.ch1.s2/fib 1 1 2 3 3)
;; TRACE t10852: | | | | | (sicp.ch1.s2/fib 8 5 2 3 2)
;; TRACE t10853: | | | | | | (sicp.ch1.s2/fib 8 5 13 21 1)
;; TRACE t10854: | | | | | | | (sicp.ch1.s2/fib 377 233 13 21 0)
;; TRACE t10854: | | | | | | | => 233
;; TRACE t10853: | | | | | | => 233
;; TRACE t10852: | | | | | => 233
;; TRACE t10851: | | | | => 233
;; TRACE t10850: | | | => 233
;; TRACE t10849: | | => 233
;; TRACE t10848: | => 233
;; TRACE t10847: => 233
;; 233
;; sicp.ch1.s2> (fib 16)
;; TRACE t10857: (sicp.ch1.s2/fib 16)
;; TRACE t10858: | (sicp.ch1.s2/fib 1 0 0 1 16)
;; TRACE t10859: | | (sicp.ch1.s2/fib 1 0 1 1 8)
;; TRACE t10860: | | | (sicp.ch1.s2/fib 1 0 2 3 4)
;; TRACE t10861: | | | | (sicp.ch1.s2/fib 1 0 13 21 2)
;; TRACE t10862: | | | | | (sicp.ch1.s2/fib 1 0 610 987 1)
;; TRACE t10863: | | | | | | (sicp.ch1.s2/fib 1597 987 610 987 0)
;; TRACE t10863: | | | | | | => 987
;; TRACE t10862: | | | | | => 987
;; TRACE t10861: | | | | => 987
;; TRACE t10860: | | | => 987
;; TRACE t10859: | | => 987
;; TRACE t10858: | => 987
;; TRACE t10857: => 987
;; 987




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.20 GCD - Normal order evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gcd [a b]
  (if (= b 0)
    a
    (sicp.ch1.s2/gcd b (rem a b))))

;; normal order evaluation
;;(gcd 206 40)

;;(gcd 40 (rem 206 40)) 
;; 1 rem evaluation
;; (rem 206 40) is evaluated
;; conditionaL evaluates to 6

;;(gcd (rem 206 40) (rem 40 (rem 206 40)))
;; 2 rem evaluations
 ;; (rem 206 40) and (rem 40 6) are evaluated
 ;; conditional evaluates to 4

;;(gcd (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40))))
;; 4 rem evaluations
;; (rem 206 40), (rem 40 6) , (rem 206 40) and (rem 6 4) are performed
;; conditional evalues to 2


;;(gcd
;;   (rem (rem 206 40) (rem 40 (rem 206 40))) 
;;   (rem (rem 40 (rem 206 40))  (rem (rem 206 40) (rem 40 (rem 206 40)))))
;; b -> 7 evaluations of rem
;; (rem 206 40), (rem 40 6) (rem 206 40) , (rem 6 4) , (rem 206 40) , (rem 40 6) , (rem 4 2)
;; conditional evalues to 0. So parameter 'a' is evaluated
;; a -> 4 rem operations are performed
;; (rem 206 40), (rem 40 6) , (rem 206 40) and (rem 6 4) are performed
;; final answer is 2

;; In total rem is performed 18 times for normal order evaluation of GCD
;; With applicative order evaluation, rem is called 4 times



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing for Primality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn square [n]
  (* n n))

(defn find-divisor [n test-divisor]
  (cond 
    (> (square test-divisor) n) n
    (= (rem n test-divisor) 0) test-divisor
    :else (recur n (inc test-divisor))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= (smallest-divisor n) n))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fermat Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square [n]
  (* n n))


;; As described in footnote
;; a*b mod n = [a mod n * b mod n] mod n 
;; Also use the property b^x mod n = [b * ((b^x-1) mod n)] mod n
;; Advantage is that we are always dealing with numbers not much larger than m
(defn expmod [base exp m]
  (cond 
    (= exp 1) base
    (even? exp) (rem (square (expmod base (/ exp 2) m)) m)
    :else (rem (* base (expmod base (dec exp) m)) m)))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (inc (rand-int (dec n)))))


(defn fast-prime? [n times]
  (cond
    (= times 0) true
    (fermat-test n) (fast-prime? n (dec times))
    :else false))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Experimenting with Fermat Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Trying to understand why Fermat test is a probabilistic algorithm
;; OK, carmichael numbers fool the test
;; However, probabilistic means [a^n mod n] will equal a for a non prime number
;; n at some point. Trying to find an example

;; There are a dozen different ways to write the following in clojure
;; Sticking to simplest procedure definitions consistent with SICP so far
;; So avoiding do block, when statement, let statement, HOFs and so on

(defn expmod-test-helper [n a]
  (if (= (expmod a n n) a)
    (print a " "))
  (if (>= a n)
    (println)
    (recur n (inc a))))
  
(defn expmod-test [start end]
  (if (<= start end)
    (println "Testing for : " start))
  (if (<= start end)
    (expmod-test-helper start 2))
  (if (<= start end)
    (recur (inc start) end)))


;; sicp.ch1.s2> (expmod-test 2 20)
;; Testing for :  2

;; Testing for :  3
;; 2  
;; Testing for :  4

;; Testing for :  5
;; 2  3  4  
;; Testing for :  6
;; 3  4  
;; Testing for :  7
;; 2  3  4  5  6  
;; Testing for :  8

;; Testing for :  9
;; 8  
;; Testing for :  10
;; 5  6  
;; Testing for :  11
;; 2  3  4  5  6  7  8  9  10  
;; Testing for :  12
;; 4  9  
;; Testing for :  13
;; 2  3  4  5  6  7  8  9  10  11  12  
;; Testing for :  14
;; 7  8  
;; Testing for :  15
;; 4  5  6  9  10  11  14  
;; Testing for :  16

;; Testing for :  17
;; 2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  
;; Testing for :  18
;; 9  10  
;; Testing for :  19
;; 2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  
;; Testing for :  20
;; 5  16  


;; Fermat test is quite leaky. Take 15, for instance. There are 7 integers smaller than 15
;; for which the fermat test will pass!

;; sicp.ch1.s2> (repeatedly 20 #(fast-prime? 15 1))
;; (true false false true true false true true true true false false true true false true false true true false)
;; sicp.ch1.s2> (repeatedly 20 #(fast-prime? 15 2))
;; (false true true true false false false false false false true true false false false false false false false true)
;; sicp.ch1.s2> (repeatedly 20 #(fast-prime? 15 3))
;; (false true false false false false false false true false false false true false true false false false false false)
;; sicp.ch1.s2> (repeatedly 20 #(fast-prime? 15 4))
;; (false false false true false false false false false false false false true false false false false false true false)
;; sicp.ch1.s2> (repeatedly 20 #(fast-prime? 15 5))
;; (false false false false false false false false false false false false false false false false false false false false)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.21 Smallest Divisor for 199,1999,19999
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(smallest-divisor 199)
;; 199

;;(smallest-divisor 1999)
;; 1999

;;(smallest-divisor 19999)
;; 7



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.22 Timed Prime Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; modified the procedures from book to print out only prime numbers
(defn report-prime [n elapsed-time]
  (newline)
  (print n)
  (print " *** ")
  (print elapsed-time))

(defn start-prime-test [n start-time]
  (if (prime? n)
    (report-prime n (- (System/currentTimeMillis) start-time))))

(defn timed-prime-test [n]
  (start-prime-test n (System/currentTimeMillis)))

(defn search-for-primes-helper [start end]
  (if (<= start end)
    (timed-prime-test start))
  (if (<= start end)
    (search-for-primes-helper (+ 2 start) end)))

(defn search-for-primes [start end]
  (if (even? start)
    (search-for-primes-helper (inc start) end)
    (search-for-primes-helper start end)))


;; sicp.ch1.s2> (search-for-primes 1000 1020)

;; 1009 *** 0
;; 1013 *** 0
;; 1019 *** 0
;; nil
;; sicp.ch1.s2> (search-for-primes 10000 10040)

;; 10007 *** 0
;; 10009 *** 0
;; 10037 *** 0
;; 10039 *** 0
;; nil
;; sicp.ch1.s2> (search-for-primes 100000 100050)

;; 100003 *** 0
;; 100019 *** 0
;; 100043 *** 0
;; 100049 *** 0
;; nil
;; sicp.ch1.s2> (search-for-primes 1000000 1000050)

;; 1000003 *** 0
;; 1000033 *** 0
;; 1000037 *** 0
;; 1000039 *** 0
;; nil
;; sicp.ch1.s2> (search-for-primes 10000000 10000200)

;; 10000019 *** 1 
;; 10000079 *** 1
;; 10000103 *** 1
;; 10000121 *** 0
;; 10000139 *** 1
;; 10000141 *** 1
;; 10000169 *** 1
;; 10000189 *** 0
;; nil
;; sicp.ch1.s2> (search-for-primes 100000000 100000200)

;; 100000007 *** 2
;; 100000037 *** 2
;; 100000039 *** 2
;; 100000049 *** 3
;; 100000073 *** 1
;; 100000081 *** 2
;; 100000123 *** 2
;; 100000127 *** 2
;; 100000193 *** 2
;; nil
;; sicp.ch1.s2> (search-for-primes 1000000000 1000000200)

;; 1000000007 *** 8
;; 1000000009 *** 5
;; 1000000021 *** 7
;; 1000000033 *** 5
;; 1000000087 *** 6
;; 1000000093 *** 5
;; 1000000097 *** 5
;; 1000000103 *** 6
;; 1000000123 *** 5
;; 1000000181 *** 5
;; nil
;; sicp.ch1.s2> (search-for-primes 10000000000 10000000200)

;; 10000000019 *** 17
;; 10000000033 *** 17
;; 10000000061 *** 15
;; 10000000069 *** 15
;; 10000000097 *** 16
;; 10000000103 *** 17
;; 10000000121 *** 15
;; 10000000141 *** 15
;; 10000000147 *** 15
;; nil
;; sicp.ch1.s2> (search-for-primes 100000000000 100000000200)

;; 100000000003 *** 56
;; 100000000019 *** 52
;; 100000000057 *** 52
;; 100000000063 *** 51
;; 100000000069 *** 54
;; 100000000073 *** 54
;; 100000000091 *** 53
;; 100000000103 *** 52
;; 100000000129 *** 55
;; 100000000171 *** 47
;; 100000000183 *** 50
;; 100000000193 *** 48
;; nil
;; sicp.ch1.s2> (search-for-primes 1000000000000 1000000000200)

;; 1000000000039 *** 172
;; 1000000000061 *** 165
;; 1000000000063 *** 173
;; 1000000000091 *** 161
;; 1000000000121 *** 175
;; 1000000000163 *** 173
;; 1000000000169 *** 159
;; 1000000000177 *** 168
;; 1000000000189 *** 162
;; 1000000000193 *** 166
;; ni
;; sicp.ch1.s2> (search-for-primes 10000000000000 10000000000200)
;; 10000000000037 *** 535
;; 10000000000051 *** 541
;; 10000000000099 *** 530
;; 10000000000129 *** 547
;; 10000000000183 *** 523
;; nil
;; sicp.ch1.s2> (search-for-primes 100000000000000 100000000000200)

;; 100000000000031 *** 1708
;; 100000000000067 *** 1702
;; 100000000000097 *** 1671
;; 100000000000099 *** 1656
;; 100000000000133 *** 1666
;; 100000000000139 *** 1654
;; 100000000000169 *** 1645
;; 100000000000183 *** 1655
;; nil
;; sicp.ch1.s2> (search-for-primes 1000000000000000 1000000000000100)

;; 1000000000000037 *** 5406
;; 1000000000000091 *** 5291
;; nil

;; OK, doing this exercise in clojure raises several issues.
;; 1) JIT compilation on JVM means the numbers are't exactly consistent between runs
;; 2) Java upto version 8 only has accuracy upto milliseconds.
;; Though nano precision is available, accurancy isn't guaranteed
;; 3) On modern computers, computing upto 1000000 takes less than a millisecond.
;; So using much larger numbers for analysis
;; 4) Because of inconsistency in time for even neighboring numbers, have to pick
;; the modal time value or largest time value in each range for analysis

;; Analysis by picking some numbers from results

;; 10000019 *** 1 ;; sqrt -> 3162
;; 1000000087 *** 6 ;; sqrt -> 31623
;; 100000000003 *** 56 ;; sqrt -> 316228
;; 10000000000129 *** 547 ;; sqrt -> 3162278
;; 1000000000000037 *** 5406 ;; sqrt -> 31622776

;; 100000007 *** 2 ;; sqrt -> 10000
;; 10000000019 *** 17 ;; sqrt -> 100000
;; 1000000000121 *** 175 ;; sqrt -> 1000000
;; 100000000000031 *** 1708 ;; sqrt -> 10000000

;; Q1) you should expect that testing for primes around 10,000 should take about (sqrt 10)
;; times as long as testing for primes around 1000. Do your timing data bear this out?
;; A1) Instead of 10,000 and 1000, we will pick 10000000000129 and 1000000000121.
;; Former takes 547ms and the latter 175ms. (/ 3162278 175.0) is 3.13.  (sqrt 10) is 3.16.
;; The numbers almost perfectly agree.
;; Comparing 1000000000121 and 100000000000031, the ratio is 100.
;; The procedure should tale 10 times as much for the bigger prime and the actual results agree.
;; So the answer to this question is yes for relatively larger primes.
;; For smaller primes, the ratio isn't that accurate but still quite close to expected

;; Q2) How well do the data for 100,000 and 1,000,000 support the (sqrt n) prediction?
;; A2) For larger primes, the result is quite consistent with the (sqrt n) prediction

;; Q3) Is your result compatible with the notion that programs on your machine run in
;; time proportional to the number of steps required for the computation?
;; A3) Yes, to quite a remarkable degree



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.23 More Efficient Smallest Divisor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn square [n]
  (* n n))

(defn next-divisor [test-divisor]
  (if (= test-divisor 2)
    3
    (+ 2 test-divisor)))

(defn find-divisor [n test-divisor]
  (cond 
    (> (square test-divisor) n) n
    (= (rem n test-divisor) 0) test-divisor
    :else (recur n (next-divisor test-divisor))))

(defn smallest-divisor [n]
  (find-divisor n 2))
  
(defn prime? [n]
  (= (smallest-divisor n) n))
  
  ;; modified the procedures from book to print out only prime numbers
(defn report-prime [n elapsed-time]
  (newline)
  (print n)
  (print " *** ")
  (print elapsed-time))

(defn start-prime-test [n start-time]
  (if (prime? n)
    (report-prime n (- (System/currentTimeMillis) start-time))))

(defn timed-prime-test [n]


;; incrementing by one
;;(timed-prime-test 10000019) ;; 1 
;;(timed-prime-test 1000000087) ;; 6 
;;(timed-prime-test 100000000003) ;; 53
;;(timed-prime-test 10000000000129) ;; 507
;;(timed-prime-test 1000000000000037) ;; 5001

;;(timed-prime-test 100000007) ;; 2
;;(timed-prime-test 10000000019) ;; 18
;;(timed-prime-test 1000000000121) ;; 163
;;(timed-prime-test 100000000000031) ;; 1589


;; incrementing by 2
;;(timed-prime-test 10000019) ;; 1 ;; ratio -> 1
;;(timed-prime-test 1000000087) ;; 4 ;; ratio -> 1.5
;;(timed-prime-test 100000000003) ;; 39 ;; ratio -> 1.36
;;(timed-prime-test 10000000000129) ;; 342 ;; ratio -> 1.48
;;(timed-prime-test 1000000000000037) ;; 3359 ;; ratio -> 1.49

;;(timed-prime-test 100000007) ;;2 ;; ratio -> 1
;;(timed-prime-test 10000000019) ;; 12 ;; ratio -> 1.5
;;(timed-prime-test 1000000000121) ;; 120  ;; ratio -> 1.36
;;(timed-prime-test 100000000000031);; 1126 ;; ratio -> 1.41


;; Q1) Since this modification halves the number of test steps,
;; you should expect it to run about twice as fast. Is this expectation confirmed?
;; A1) No

;; Q2) If not, what is the observed ratio of the speeds of the two algorithms
;; A2) Observed ratio is close to 1.4

;; Q3) how do you explain the fact that it is different from 2?
;; A3) Difference in speed by a factor of 1.35 between inc and next-divisor
;; See below


;; sicp.ch1.s2> (time (next-divisor 1000000))
;; "Elapsed time: 0.10766 msecs"
;; 1000002
;; sicp.ch1.s2> (time (next-divisor 1000000))
;; "Elapsed time: 0.111335 msecs"
;; 1000002
;; sicp.ch1.s2> (time (next-divisor 1000000))
;; "Elapsed time: 0.108775 msecs"
;; 1000002
;; sicp.ch1.s2> (time (next-divisor 1000000))
;; "Elapsed time: 0.11509 msecs"
;; 1000002
;; sicp.ch1.s2> (time (inc 1000000))
;; "Elapsed time: 0.08713 msecs"
;; 1000001
;; sicp.ch1.s2> (time (inc 1000000))
;; "Elapsed time: 0.0793 msecs"
;; 1000001
;; sicp.ch1.s2> (time (inc 1000000))
;; "Elapsed time: 0.081625 msecs"
;; 1000001



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.24 Timed Test with Fast Frime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2 major changes - using nanoTime because algorithm is really fast
;; and BIGINT to handle squaring of large long type numbers

(defn square [n]
   (* n n))

;; USING BIGINT
(defn expmod-bigint [base exp m]
  (cond 
    (= exp 1) base
    (even? exp) (rem (square (bigint (expmod-bigint base (/ exp 2) m))) m)
    :else (rem (* base (expmod-bigint base (dec exp) m)) m)))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod-bigint a n n) a))
  (try-it (inc
           (.nextLong (java.util.concurrent.ThreadLocalRandom/current) (dec n)))))

(defn start-prime-test [n start-time]
  (if (fast-prime? n 20)
    (report-prime n (- (System/nanoTime) start-time))))

(defn timed-prime-test [n]
  (start-prime-test n (System/nanoTime)))

;; (timed-prime-test 1019) ;; 127967
;; (timed-prime-test 10037);; 150975
;; (timed-prime-test 100043);; 178028
;; (timed-prime-test 1000037);; 196310
;; (timed-prime-test 10000019) ;; 235928
;; (timed-prime-test 100000007) ;; 264233
;; (timed-prime-test 1000000087) ;; 306367
;; (timed-prime-test 10000000019) ;; 595261
;; (timed-prime-test 100000000003) ;; 820854
;; (timed-prime-test 1000000000121) ;; 902351
;; (timed-prime-test 10000000000129) ;; 941385
;; (timed-prime-test 100000000000031) ;; 1104555
;; (timed-prime-test 1000000000000037) ;; 1152927
;; (timed-prime-test 10000000000000061) ;; 1227878

;; sicp.ch1.s2> (Math/log 1000000000000037)
;; 34.53877639491072
;; sicp.ch1.s2> (Math/log 10000019)
;; 16.118097550956517
;; sicp.ch1.s2> (/ 820854 235928.0)
;; 3.4792563833033805
;; sicp.ch1.s2> (/ 1152927 235928.0)
;; 4.886774778746058
;; sicp.ch1.s2> 


;; Bewteen 1000000000000037 and 10000019 ,
;; the ratio in log of values is roughly 2.15
;; However the ratio in time is roughly 4.8

;; Bewteen 10000019 and 1019 ,
;; the ratio in log of values is roughly 2.32
;; However the ratio in time is roughly 1.84


;; The discrepancy could by caused by
;; a) the fact that nonoTime accuracy is not guaranteed
;; b) squaring , bigint function and rem are not constant time operations
;; They depend on the size of the number


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.25 Alyssa P. Hacker's shortcut
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expmod2 [base exp m]
  (rem (fast-expt-i base exp 1) m))

;; When base and exp are large numbers, performing the actual exponentiation
;; would produce a ridiculously huge number
;; Using modular arithmetic identities will mean
;; the worst mathematical operation performed is squaring of a number
;; that is not much larger than the base
;; So by using modular arithmetic, we have avoided time-consuming unecessary
;; multiplications and , for large numbers, made the impossible possible



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.26 Louis Reasoner's mistake - O(log n) vs O(n)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expmod3 [base exp m]
  (cond 
    (= exp 0) 1
    (even? exp) (rem (* (expmod3 base (/ exp 2) m)
                        (expmod3 base (/ exp 2) m))
                      m)
    :else (rem (* base (expmod3 base (dec exp) m)) m)))


;; sicp.ch1.s2> (expmod3 5 8 8)
;; TRACE t12740: (sicp.ch1.s2/expmod3 5 8 8)
;; TRACE t12741: | (sicp.ch1.s2/expmod3 5 4 8)
;; TRACE t12742: | | (sicp.ch1.s2/expmod3 5 2 8)
;; TRACE t12743: | | | (sicp.ch1.s2/expmod3 5 1 8)
;; TRACE t12744: | | | | (sicp.ch1.s2/expmod3 5 0 8)
;; TRACE t12744: | | | | => 1
;; TRACE t12743: | | | => 5
;; TRACE t12745: | | | (sicp.ch1.s2/expmod3 5 1 8)
;; TRACE t12746: | | | | (sicp.ch1.s2/expmod3 5 0 8)
;; TRACE t12746: | | | | => 1
;; TRACE t12745: | | | => 5
;; TRACE t12742: | | => 1
;; TRACE t12747: | | (sicp.ch1.s2/expmod3 5 2 8)
;; TRACE t12748: | | | (sicp.ch1.s2/expmod3 5 1 8)
;; TRACE t12749: | | | | (sicp.ch1.s2/expmod3 5 0 8)
;; TRACE t12749: | | | | => 1
;; TRACE t12748: | | | => 5
;; TRACE t12750: | | | (sicp.ch1.s2/expmod3 5 1 8)
;; TRACE t12751: | | | | (sicp.ch1.s2/expmod3 5 0 8)
;; TRACE t12751: | | | | => 1
;; TRACE t12750: | | | => 5
;; TRACE t12747: | | => 1
;; TRACE t12741: | => 1
;; TRACE t12752: | (sicp.ch1.s2/expmod3 5 4 8)
;; TRACE t12753: | | (sicp.ch1.s2/expmod3 5 2 8)
;; TRACE t12754: | | | (sicp.ch1.s2/expmod3 5 1 8)
;; TRACE t12755: | | | | (sicp.ch1.s2/expmod3 5 0 8)
;; TRACE t12755: | | | | => 1
;; TRACE t12754: | | | => 5
;; TRACE t12756: | | | (sicp.ch1.s2/expmod3 5 1 8)
;; TRACE t12757: | | | | (sicp.ch1.s2/expmod3 5 0 8)
;; TRACE t12757: | | | | => 1
;; TRACE t12756: | | | => 5
;; TRACE t12753: | | => 1
;; TRACE t12758: | | (sicp.ch1.s2/expmod3 5 2 8)
;; TRACE t12759: | | | (sicp.ch1.s2/expmod3 5 1 8)
;; TRACE t12760: | | | | (sicp.ch1.s2/expmod3 5 0 8)
;; TRACE t12760: | | | | => 1
;; TRACE t12759: | | | => 5
;; TRACE t12761: | | | (sicp.ch1.s2/expmod3 5 1 8)
;; TRACE t12762: | | | | (sicp.ch1.s2/expmod3 5 0 8)
;; TRACE t12762: | | | | => 1
;; TRACE t12761: | | | => 5
;; TRACE t12758: | | => 1
;; TRACE t12752: | => 1
;; TRACE t12740: => 1
;; 1


;; If a is an integer such that 2^a is the closest number to exp
;; that is less than exp,

;; With the fast algorithm, the number of steps is a+(exp-2^a).
;; For sufficiently large values of exp, we can assume exp=2^a.
;; The number of steps is 'a' which is log(exp) or O(log n).

;; With the slow algorithm, the number of multiplications is exactly
;; 2^(a+1) - 1 + (exp-2^a). For sufficiently large values of n,
;; no. of steps = (2*exp)-1. This is O(n)

;; How did we arrive at the formula (2*2^a)-1+ (exp-2^a) ?
;; Let us assume exp = 2^a.
;; One multiplication is performed per function call
;; First call is made with exp initially to be 2^a.
;; Two calls are made with exp parameter value  (2^a)/2
;; Four multiplications with exp parameter (2^a)/4
;; ..... and so on until exp parameter is one
;; So, number of multiplications is 1+2+4+8+....2^a
;; 2^0 + 2^1 + 2^2 + 2^3 + ...... + 2^a
;; This is a geometric sequence with value (1-2^(a+1))/(1-2) or 2^(a+1)-1



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.27 Carmichael numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Carmichael numbers -> 561, 1105, 1729, 2465, 2821, and 6601

;; return 0 for success or value of a when failed
(defn carmichael-test [a n]
  (if (< a n)
    (if (= (expmod a n n) a)
      (recur (inc a) n)
      a)
    0))

;; sicp.ch1.s2> (carmichael-test 2 561)
;; 0
;; sicp.ch1.s2> (carmichael-test 2 1105)
;; 0
;; sicp.ch1.s2> (carmichael-test 2 1729)
;; 0
;; sicp.ch1.s2> (carmichael-test 2 2465)
;; 0
;; sicp.ch1.s2> (carmichael-test 2 2821)
;; 0
;; sicp.ch1.s2> (carmichael-test 2 6601)
;; 0


;; sicp.ch1.s2> (map #(fast-prime? % 100) [561 1105 1729 2465 2821 6601])
;; (true true true true true true)
;; sicp.ch1.s2> (map prime? [561 1105 1729 2465 2821 6601])
;; (false false false false false false)
;; sicp.ch1.s2> (map #(carmichael-test 2 %) [561 1105 1729 2465 2821 6601])
;; (0 0 0 0 0 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex. 1.28 Miller-Rabin Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; poorly worded exercise problem. Took a multiple re-readings and
;; learning from web to understand how miller-rabin is supposed to work

;; The trick is determining if we have come across a number whose square modulo n
;; results in 1. '1 modulo n' and 'n-1 modulo n' would fit this bill however they are 
;; considered trivial square roots and so we look for other numbers.
;; For example (expmod 8 20 21) would recurse to (expmod 8 1 21) which produces 8
;; Now 8 is neither 1 nor 20. Yet (rem (* 8 8) 21) produces 1. 
;; Once we encounter this, we can signal by returning 0 which will get propogated
;; through the recursive calls
;; Once we encounter something like (expmod 8 1 21), '1' gets propogated out as result
;; We break this reasult of 1 being returned by instead signaling 0.



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

;; Feeling lazy, so will just use HOFs to test whether fast-prime-mr? works.
;; Seems to work as expected.
;;(filter #(fast-prime-mr? % 20) (range 10000000 10000200))
;;(10000019
;; 10000079
;; 10000103
;; 10000121
;; 10000139
;; 10000141
;; 10000169
;; 10000189)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Experimenting with Miller-Rabin test
;;;   ---- Looking for false positives ----
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Miller-Rabin test seems much much more robust than Fermat test
;; Let's see for how many non prime numbers below 27, false positives show up


;;NOTE using expmod-mr here,  whixh is modified for miller-rabin test
(defn expmod-mr-test-helper [n a]
  (if (= (expmod-mr a (dec n) n) 1)
    (print a " "))
  (if (>= a n)
    (println)
    (recur n (inc a))))

  
(defn expmod-mr-test [start end]
  (if (<= start end)
    (println "Testing for : " start))
  (if (<= start end)
    (expmod-mr-test-helper start 2))
  (if (<= start end)
    (recur (inc start) end)))


;; sicp.ch1.s2> (expmod-mr-test 2 26)
;; Testing for :  2

;; Testing for :  3
;; 2  
;; Testing for :  4

;; Testing for :  5
;; 2  3  4  
;; Testing for :  6

;; Testing for :  7
;; 2  3  4  5  6  
;; Testing for :  8

;; Testing for :  9
;; 8  
;; Testing for :  10

;; Testing for :  11
;; 2  3  4  5  6  7  8  9  10  
;; Testing for :  12

;; Testing for :  13
;; 2  3  4  5  6  7  8  9  10  11  12  
;; Testing for :  14

;; Testing for :  15
;; 14  
;; Testing for :  16

;; Testing for :  17
;; 2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  
;; Testing for :  18

;; Testing for :  19
;; 2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  
;; Testing for :  20

;; Testing for :  21
;; 20  
;; Testing for :  22

;; Testing for :  23
;; 2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  19  20  21  22  
;; Testing for :  24

;; Testing for :  25
;; 7  18  24  
;; Testing for :  26

;; nil

;;;;
;; Not bad. Much less leaky than Fermat test
;;;;


;; Miller-Rabis test is not fooled by the provided carmichael numbers
;; (map #(fast-prime-mr? % 100) [561 1105 1729 2465 2821 6601])
;; (false false false false false false)


;; failure occurs fairly early for carmichael numbers
 (defn carmichael-test-mr [a n]
  (if (< a n)
    (if (= (expmod-mr a (dec n) n) 1)
      (recur (inc a) n)
      a)
    0))

;; (map #(carmichael-test-mr 2 %) [561 1105 1729 2465 2821 6601])
;; (2 2 2 2 2 2)