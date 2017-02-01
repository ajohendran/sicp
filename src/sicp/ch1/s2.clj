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

;; that is the transformation will bw


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

;;(gcd (rem 40 (rem 206 40)) 
;;	 (rem (rem 206 40) (rem 40 (rem 206 40))))
;; 4 rem evaluations
;; (rem 206 40), (rem 40 6) , (rem 206 40) and (rem 6 4) are performed
;; conditional evalues to 2


;;(gcd (rem (rem 206 40) (rem 40 (rem 206 40))) 
;;	 (rem (rem 40 (rem 206 40))  (rem (rem 206 40) (rem 40 (rem 206 40)))))
;; b -> 7 evaluations of rem
;; (rem 206 40), (rem 40 6) (rem 206 40) , (rem 6 4) , (rem 206 40) , (rem 40 6) , (rem 4 2)
;; conditional evalues to 0. So parameter 'a' is evaluated
;; a -> 4 rem operations are performed
;; (rem 206 40), (rem 40 6) , (rem 206 40) and (rem 6 4) are performed
;; final answer is 2

;; In total rem is performed 18 times for normal order evaluation of GCD
;; With applicative order evaluation, rem is called 4 times











