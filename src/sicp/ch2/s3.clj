(ns sicp.ch2.s3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.53
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn memq [a l]
  (cond (empty? l) false
        (= a (first l)) l
        :else (recur a (next l))))

;; (list 'a 'b 'c)
;; (a b c)

;; (list (list 'george))
;; ((george))

;; (cdr '((x1 x2) (y1 y2)))
;; ((y1 y2))

;; (cadr '((x1 x2) (y1 y2)))
;; (y1 y2)

;; (pair? (car '(a short list)))
;; false

;; (memq 'red '((red shoes) (blue socks)))
;; false

;; (memq 'red '(red shoes blue socks))
;; (red shoes blue socks)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.54
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; equality is a slippery slope with several possible interpretations
;; and philosophical meanings. ALso there are several differencdes between
;; clojure and scheme. So, this best be kept simple.

;; this should be somewhat same as "=" in clojure
(defn equal? [l1 l2]
  (or (and (not (sequential? l1))
           (not (sequential? l2))
           (= l1 l2))
      (and (sequential? l1)
           (sequential? l2)
           (= (first l1) (first l2))
           (equal? (next l1) (next l2)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.55
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (car ''abracadabra)

;; ''abracadabra is equivalent to (quote (quote abracadabra)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Symbolic Differentiation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def variable? symbol?)

(defn same-variable? [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))

(defn make-sum [a b]
  (list '+ a b))

(defn sum? [e]
  (and (seq? e)
       (= '+ (first e))))

(defn addend [e]
  (first (next e)))

(defn augend [e]
  (first (next (next e))))

(defn make-product [a b]
  (list '* a b))

(defn product? [e]
  (and (seq? e)
       (= '* (first e))))

(defn multiplier [e]
  (first (next e)))

(defn multiplicand [e]
  (first (next (next e))))

(defn deriv [exp v]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp v)
                          1
                          0)
        (sum? exp) (make-sum (deriv (addend exp) v) (deriv (augend exp) v))
        (product? exp) (make-sum
                        (make-product (multiplier exp)
                                      (deriv (multiplicand exp) v))
                        (make-product (multiplicand exp)
                                      (deriv (multiplier exp) v)))
        :else (throw (RuntimeException.
                      (str  "unknown expression type -- DERIV " exp)))))


(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn make-sum [a b]
  (cond (=number? a 0) b
        (=number? b 0) a
        (and (number? a) (number? b)) (+ a b)
        :else (list '+ a b)))

(defn make-product [a b]
  (cond (or (=number? a 0) (=number? b 0)) 0
        (=number? a 1) b
        (=number? b 1) a
        (and (number? a) (number? b)) (* a b)
        :else (list '* a b)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.56
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-exponentiation [a b]
  (cond (=number? b 0) 1
        (=number? a 0) 0
        (=number? b 1) a
        (=number? a 1) 1
        (and (number? a) (number? b)) (Math/pow a b)
        :else (list '** a b)))

(defn exponentiation? [e]
  (and (seq? e)
       (= '** (first e))))

(defn base [e]
  (first (next e)))

(defn exponent [e]
  (first (next (next e))))

(defn make-difference [a b]
  (cond (=number? b 0) a
        (and (number? a) (number? b)) (- a b)
        :else (list '- a b)))

(defn deriv [exp v]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp v)
                          1
                          0)
        (sum? exp) (make-sum (deriv (addend exp) v) (deriv (augend exp) v))
        (product? exp) (make-sum
                        (make-product (multiplier exp)
                                      (deriv (multiplicand exp) v))
                        (make-product (multiplicand exp)
                                      (deriv (multiplier exp) v)))
        (exponentiation? exp) (make-product
                               (make-product
                                (exponent exp)
                                (make-exponentiation (base exp)
                                                     (make-difference
                                                      (exponent exp) 1)))
                               (deriv (base exp) v)) 
        :else (throw (RuntimeException.
                      (str  "unknown expression type -- DERIV " exp)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.57
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my solution
(defn augend [e]
  (if (empty? (next (next (next e))))
    (first (next (next e)))
    (cons '+ (next (next e)))))

(defn augend [e]
  (if (empty? (next (next (next e))))
    (first (next (next e)))
    (make-sum (first (next (next e))) (next (next e)))))

(defn foldr [op init s]
  (if (empty? s)
    init
    (op (first s) (foldr op init (next s)))))

;; much better solution from online
(defn augend [e]
  (foldr make-sum 0 (next (next e))))

(defn multiplicand [e]
  (foldr make-product 1 (next (next e))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.58
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; a ;;;;

(defn make-sum [a b]
  (cond (=number? a 0) b
        (=number? b 0) a
        (and (number? a) (number? b)) (+ a b)
        :else (list a '+ b)))

(defn sum? [e]
  (and (seq? e)
       (= '+ (first (next e)))))

(defn addend [e]
  (first e))

(defn augend [e]
  (first (next (next e))))

(defn make-product [a b]
  (cond (or (=number? a 0) (=number? b 0)) 0
        (=number? a 1) b
        (=number? b 1) a
        (and (number? a) (number? b)) (* a b)
        :else (list a '* b)))

(defn product? [e]
  (and (seq? e)
       (= '* (first (next e)))))

(defn multiplier [e]
  (first e))

(defn multiplicand [e]
  (first (next (next e))))



;; Skipping b fror now because it doesn't cover a fundamental learning idea
;; Will return when time permits
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.58
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

