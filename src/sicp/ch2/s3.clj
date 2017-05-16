
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

;; (defn make-sum [a b]
;;   (list '+ a b))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn make-sum [a b]
  (cond (=number? a 0) b
        (=number? b 0) a
        (and (number? a) (number? b)) (+ a b)
        :else (list '+ a b)))

(defn sum? [e]
  (and (seq? e)
       (= '+ (first e))))

(defn addend [e]
  (first (next e)))

(defn augend [e]
  (first (next (next e))))

;; (defn make-product [a b]
;;   (list '* a b))

(defn make-product [a b]
  (cond (or (=number? a 0) (=number? b 0)) 0
        (=number? a 1) b
        (=number? b 1) a
        (and (number? a) (number? b)) (* a b)
        :else (list '* a b)))

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

(defn make-exponentiation [a b]
  (cond (=number? b 0) 1
        (=number? a 0) 0
        (=number? b 1) a
        (=number? a 1) 1
        (and (number? a) (number? b)) (int (Math/pow a b))
        :else (list a '** b)))

(defn exponentiation? [e]
  (and (seq? e)
       (= '** (first (next e)))))

(defn base [e]
  (first e))

(defn exponent [e]
  (first (next (next e))))

(defn make-difference [a b]
  (cond (=number? b 0) a
        (and (number? a) (number? b)) (- a b)
        :else (list a '- b)))


;;;; b ;;;;

(defn precedence [op]
  (cond (= op '+) 0
        (= op '*) 1
        (= op '**) 2
        :else (throw (Exception. (str "Unknown operator " op)))))

(defn higher-precedence [op1 op2]
  (> (precedence op1) (precedence op2)))

(defn lower-precedence [op1 op2]
  (< (precedence op1) (precedence op2)))

(defn right-associative? [op]
  (= op '**))

(defn trim [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (recur (dec n) (next coll))))

(defn head [n coll]
  (if (or (empty? coll) (< n 1))
    '()
    (cons (first coll) (head (dec n) (next coll)))))

(defn tail [n coll]
  (let [remaining (- (count coll) n)]
    (trim remaining coll)))

(defn split-point [n coll]
  (if (or (empty? coll) (<= n 0))
    (list '() coll)
    (let [res (split-point (dec n) (next coll))]
        (list (cons (first coll) (first res)) (second res)))))

(defn convert [exp]
  (cond (empty? exp) exp
        (exmpty? (trim 3 exp)) exp
        :else (convert (convert-1 exp))))

(defn convert-1 [exp]
  (cond (empty? exp) exp
        (empty? (trim 3 exp)) (list exp)
        :else (let [first-op (second exp)
                    second-op (first (trim 3 exp))
                    group-first (fn [] (cons (head 3 exp) (trim 3 exp)))
                    group-next (fn [] (concat (head 2 exp) (convert-1
                                                            (trim 2 exp))))]
                (cond (higher-precedence first-op second-op) (group-first)
                      (lower-precedence first-op second-op) (group-next)
                      (right-associative? first-op) (group-next)
                      :else (group-first)))))

;; (convert '())
;; ;; => ()
;; (convert '(x + y))
;; ;; => (x + y)
;; (convert '(x * y))
;; ;; => (x * y)
;; (convert '(x ** y))
;; ;; => (x ** y)
;; (convert '(x + y + z))
;; ;; => ((x + y) + z)
;; (convert '(x * y * z))
;; ;; => ((x * y) * z)
;; (convert '(x + y * z))
;; ;; => (x + (y * z))
;; (convert '(x * y + z))
;; ;; => ((x * y) + z)
;; (convert '(x * y + z * a))
;; ;; => ((x * y) + (z * a))
;; (convert '(x * y + z + a))
;; ;; => (((x * y) + z) + a)
;; (convert '(x + y + z * a))
;; ;; => ((x + y) + (z * a))
;; (convert '(x + y * z + a))
;; ;; => ((x + (y * z)) + a)
;; (convert '(x ** y + z + a))
;; ;; => (((x ** y) + z) + a)
;; (convert '(x * y ** z + a))
;; ;; => ((x * (y ** z)) + a)
;; (convert '(x + y ** z * a))
;; ;; => (x + ((y ** z) * a))
;; (convert '(x * y + z ** a))
;; ;; => ((x * y) + (z ** a))
;; (convert '(x + y ** z))
;; ;; => (x + (y ** z))
;; (convert '(x ** y + z))
;; ;; => ((x ** y) + z)
;; (convert '(x ** y * z))
;; ;; => ((x ** y) * z)
;; (convert '(x * y ** z))
;; ;; => (x * (y ** z))
;; (convert '(x ** y ** z ** a ** b))
;; ;; => (x ** (y ** (z ** (a ** b))))
;; (convert '(x * y ** z * a ** b))
;; ;; => ((x * (y ** z)) * (a ** b))
;; (convert '(x * y + z ** a * b))
;; ;; => ((x * y) + ((z ** a) * b))
;; (convert '(x + y * z ** a ** b + c))
;; ;; => ((x + (y * (z ** (a ** b)))) + c)
;; (convert '(w * x ** y * z ** a ** b))
;; ;; => ((w * (x ** y)) * (z ** (a ** b)))
;; (convert '(w * x ** y + z ** a ** b))
;; ;; => ((w * (x ** y)) + (z ** (a ** b)))
;; (convert '(w ** x * y ** z + a ** b))
;; ;; => (((w ** x) * (y ** z)) + (a ** b))
;; (convert '(w + x * y + z ** a * b))
;; ;; => ((w + (x * y)) + ((z ** a) * b))
;; (convert '(w + x * y + z ** (a * b)))
;; ;; => ((w + (x * y)) + (z ** (a * b)))
;; (convert '(w * x * y ** z + a + b))
;; ;; => ((((w * x) * (y ** z)) + a) + b)
;; (convert '(w * x * y ** z + a * b))
;; ;; => (((w * x) * (y ** z)) + (a * b))
;; (convert '(w * x * y ** z * a + b))
;; ;; => ((((w * x) * (y ** z)) * a) + 


(defn every-nth [n exp]
  (cond (< n 1) (throw (Exception. "Positive number expected for n"))
        (empty? exp) '()
        :else (cons (first exp) (every-nth n (trim n exp)))))

(defn get-operators [exp]
  (every-nth 2 (next exp)))

(defn lowest-precedence? [op coll]
  (foldl (fn [])))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.58
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 

