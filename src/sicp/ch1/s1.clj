(ns sicp.ch1.s1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.1 The Elements of Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn p [] (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; (test 0 (p))
;; Applicative Order -> (p) isexecuted first and program falls into infinite loop
;; Normal Order -> 0 is returned



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code for Ex 1.6 and 1.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn square [x]
  (* x x))

(defn good-enough? [guess x]
  (< (Math/abs (- (square guess) x)) 0.001))

;; 2 should be represented as decimal to avoid ratios
(defn average [a b]
  (/ (+ a b) 2.0))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Question - what happens if new-if is used

(defn new-if [predicate then-clause else-clause]
  (cond
    predicate then-clause
    :else else-clause))

(defn sqrt-iter [guess x]
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))


;; Answer - we fall into infinite loop because new-if, as a function, will have to evaluate its arguments 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (recur (improve guess x) x)))

;; For very large input values of X (say 55555555555555), guess proceeds towards a certain value (7453559.924999261) after which good-enough? returns false but improve returns the same value and so the program gets into an infinite loop
;; When good-enough? squares 7453559.924999261 and compares it to 55555555555555, the difference of 0.0078125 is greater than 0.001 . Improve finds the average of 7453559.924999261 and 7453559.924999262 (which is 55555555555555 divided by 7453559.924999261)  and returns 7453559.924999261 again.
;; As we get closer to the answer, 1/guess would be close to X, and the difference would be in nth decimal place. average of two numbers with a difference in only nth decimal place would yield one of the numbers again due to precision issues.

;; For very small input values of X (say 0.00000005), guess reaches a value (0.031250532810688444) such that square produces a number (0.000976595800952) whose diff from X is less than 0.001. 
;; (good-enough? 0.031250532810688444 0.00000005) will return true because difference between (square guess) and X is 0.000976545800952, which is less than tolerance of 0.001. (square guess) will produce an arbitrarily small number less than tolerance. x is an even smaller number. So the difference between these two numbers will be lower than tolerance. So any guess value whole square is lower than tolerance will essentially pass the good enough test.
;; If we bump down the tolerance in good-enough? to 0.0000000001, we will get much closer to the right answer. 

(defn new-good-enough? [guess old-guess]
  (< (/ (Math/abs (- guess old-guess))
         old-guess)
     0.001))

(defn new-sqrt-iter [guess old-guess x]
  (if (new-good-enough? guess old-guess)
    guess
    (new-sqrt-iter (improve guess x) guess x)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; important to represent 2 and 3 as decimals to avoid ratios
(defn improve-cbrt [y x]
  (/ (+ (/ x (square y))
        (* 2.0 y))
     3.0))

(defn cbrt-iter [guess old-guess x]
  (if (new-good-enough? guess old-guess)
    guess
    (cbrt-iter (improve-cbrt guess x) guess x)))


