(ns sicp.ch1.s1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1.1 The Elements of Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square [x]
  (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

10
;; => 10

(+ 5 3 4)
;; => 12

(- 9 1)
;; => 8

(/ 6 2)
;; => 3

(+ (* 2 4) (- 4 6))
;; => 6

(def a 3)
;; => #'sicp.ch1.s1/a

(def b (+ a 1))
;; => #'sicp.ch1.s1/b

(+ a b (* a b))
;; => 19

(= a b)
;; => false


(if (and (> b a) (< b (* a b)))
    b
    a)
;; => 4

(cond (= a 4) 6
      (= b 4) (+ 6 7 a)
      :else 25)
;; => 16

(+ 2 (if (> b a) b a))
;; => 6

(* (cond (> a b) a
         (< a b) b
         :else -1)
   (+ a 1))
;; => 16





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
;; => -37/150





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; several ways to do this

(defn sum-squares-largest-2 [a b c]
  (if (> a b)
    (if (> b c)
      (sum-of-squares a b)
      (sum-of-squares a c))
    (if (> a c)
      (sum-of-squares a b)
      (sum-of-squares b c))))

(defn sum-squares-largest-2 [a b c]
  (if (> a b)
    (sum-of-squares a (if (> b c) b c))
    (sum-of-squares b (if (> a c) a c))))

(defn sum-squares-largest-2 [a b c]
  (if (> a b)
    (sum-of-squares a (max b c))
    (sum-of-squares b (max a c))))

;; Really clever version from http://community.schemewiki.org/?sicp-ex-1.3
(defn sum-squares-largest-2 [a b c]
  (sum-of-squares (max a b) (max (min a b) c)))

;; (defn permutations [s]
;;   (if (empty? s)
;;     '(())
;;     (mapcat (fn [x]
;;               (map (fn [p] (cons x p))
;;                    (permutations (remove #{x} s))))
;;             s)))
;; sicp.ch1.s1> (map #(apply sum-squares-largest-2 %) (permutations [1 2 3]))
;; (13 13 13 13 13 13)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (a-plus-abs-b a b)
;;  ((if (> b 0) + -) a b))

;; The expression (if (> b 0) + -) is evaluated to determine the operator
;; which is the value of either + or - symbol.





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ex 1.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p [] (p))

(defn test-this [x y]
  (if (= x 0)
      0
      y))

;; (test-this 0 (p))
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
