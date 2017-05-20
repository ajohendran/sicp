
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
        (= op '-) 0
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
                      (not (= first-op second-op)) (group-first)
                      (right-associative? first-op) (group-next)
                      :else (group-first)))))

;; One approach is to preprocess the expression
(defn convert [exp]
  (let [exp2 (map (fn [e] (if (coll? e) (convert e) e)) exp)]
    (cond (empty? exp2) exp2
        (empty? (trim 3 exp)) exp2
        :else (convert (convert-1 exp2)))))

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
;; ;; => ((((w * x) * (y ** z)) * a) + b)
;; (convert '(w * x * y ** z * (a + b ** (c * d * e) * f ) + d))
;; ;; ((((w * x) * (y ** z)) * (a + ((b ** ((c * d) * e)) * f))) + d)

;; Other approach is to provide appropriate selectors
;; Not the most efficient approach

(defn every-nth [n exp]
  (cond (< n 1) (throw (Exception. "Positive number expected for n"))
        (empty? exp) '()
        :else (cons (first exp) (every-nth n (trim n exp)))))

(defn get-operators [exp]
  (every-nth 2 (next exp)))

(defn as-lowest-precedence? [op exp]
  (let [op-list (get-operators exp)]
    (loop [coll op-list
           res false]
    (cond (empty? coll) (or res false)
          (lower-precedence (first coll) op) false
          (= (first coll) op) (recur (next coll) true)
          :else (recur (next coll) res)))))

(defn split-until-first [pred coll]
  (if (or (empty? coll) (pred (first coll)))
    (list '() coll)
    (let [result (split-until-first pred (next coll))]
      (list (cons (first coll) (first result)) (second result)))))

(defn exists-any [pred coll]
  (and (not (empty? coll))
       (or (pred (first coll))
           (recur pred (next coll)))))

(defn last-index [pred coll]
  (loop [i -1
         c 0
         lst coll]
    (cond (empty? lst) i
          (pred (first lst)) (recur c (inc c) (next lst))
          :else (recur i (inc c) (next lst)))))

(defn split-point [n coll]
  (if (or (empty? coll) (<= n 0))
    (list '() coll)
    (let [res (split-point (dec n) (next coll))]
      (list (cons (first coll) (first res)) (second res)))))

;; severalw ays to write this funtion
(defn split-until-last [pred coll]
  (let [i (last-index pred coll)]
    (if (neg? i)
      (list coll '())
      (split-point i coll))))

;; perhaps not the most efficient way because of repeated list construction
;; OTOH clarity is good. Clojure's shared data structure tree should help
;; with performance
(defn split-until-last [pred coll]
  (foldr (fn [e res]
           (cond (seq (second res)) (list (cons e (first res)) (second res))
                 (pred e) (list '() (cons e (first res)))
                 :else (list (cons e (first res)) '())))
         '(() ())
         coll))

(defn extract-variable [exp]
  (if (= 1 (count exp))
    (first exp)
    exp))

(defn sum? [e]
  (and (seq? e)
       (as-lowest-precedence? '+ e)))

(defn addend [e]
  (extract-variable (first (split-until-last (fn [a] (= a '+)) e))))

(defn augend [e]
  (extract-variable (next (second (split-until-last (fn [a] (= a '+)) e)))))

(defn product? [e]
  (and (seq? e)
       (as-lowest-precedence? '* e)))

(defn multiplier [e]
  (extract-variable (first (split-until-last (fn [a] (= a '*)) e))))

(defn multiplicand [e]
  (extract-variable (next (second (split-until-last (fn [a] (= a '*)) e)))))

(defn exponentiation? [e]
  (and (seq? e)
       (as-lowest-precedence? '** e)))

(defn base [e]
  (extract-variable (first (split-until-first (fn [a] (= a '**)) e))))

(defn exponent [e]
  (extract-variable (next (second (split-until-first (fn [a] (= a '**)) e)))))

;; sicp.ch2.s3> (deriv '(5 * x * y ** x * x + y) 'x)
;; TRACE t12401: (sicp.ch2.s3/deriv (5 * x * y ** x * x + y) x)
;; TRACE t12402: | (sicp.ch2.s3/deriv (5 * x * y ** x * x) x)
;; TRACE t12403: | | (sicp.ch2.s3/deriv x x)
;; TRACE t12403: | | => 1
;; TRACE t12404: | | (sicp.ch2.s3/deriv (5 * x * y ** x) x)
;; TRACE t12405: | | | (sicp.ch2.s3/deriv (y ** x) x)
;; TRACE t12406: | | | | (sicp.ch2.s3/deriv y x)
;; TRACE t12406: | | | | => 0
;; TRACE t12405: | | | => 0
;; TRACE t12407: | | | (sicp.ch2.s3/deriv (5 * x) x)
;; TRACE t12408: | | | | (sicp.ch2.s3/deriv x x)
;; TRACE t12408: | | | | => 1
;; TRACE t12409: | | | | (sicp.ch2.s3/deriv 5 x)
;; TRACE t12409: | | | | => 0
;; TRACE t12407: | | | => 5
;; TRACE t12404: | | => ((y ** x) * 5)
;; TRACE t12402: | => ((5 * x * y ** x) + (x * ((y ** x) * 5)))
;; TRACE t12410: | (sicp.ch2.s3/deriv y x)
;; TRACE t12410: | => 0
;; TRACE t12401: => ((5 * x * y ** x) + (x * ((y ** x) * 5)))
;; ((5 * x * y ** x) + (x * ((y ** x) * 5)))

;; sicp.ch2.s3> (deriv (convert '(5 * x * y ** x * x + y)) 'x)
;; TRACE t12413: (sicp.ch2.s3/deriv ((((5 * x) * (y ** x)) * x) + y) x)
;; TRACE t12414: | (sicp.ch2.s3/deriv (((5 * x) * (y ** x)) * x) x)
;; TRACE t12415: | | (sicp.ch2.s3/deriv x x)
;; TRACE t12415: | | => 1
;; TRACE t12416: | | (sicp.ch2.s3/deriv ((5 * x) * (y ** x)) x)
;; TRACE t12417: | | | (sicp.ch2.s3/deriv (y ** x) x)
;; TRACE t12418: | | | | (sicp.ch2.s3/deriv y x)
;; TRACE t12418: | | | | => 0
;; TRACE t12417: | | | => 0
;; TRACE t12419: | | | (sicp.ch2.s3/deriv (5 * x) x)
;; TRACE t12420: | | | | (sicp.ch2.s3/deriv x x)
;; TRACE t12420: | | | | => 1
;; TRACE t12421: | | | | (sicp.ch2.s3/deriv 5 x)
;; TRACE t12421: | | | | => 0
;; TRACE t12419: | | | => 5
;; TRACE t12416: | | => ((y ** x) * 5)
;; TRACE t12414: | => (((5 * x) * (y ** x)) + (x * ((y ** x) * 5)))
;; TRACE t12422: | (sicp.ch2.s3/deriv y x)
;; TRACE t12422: | => 0
;; TRACE t12413: => (((5 * x) * (y ** x)) + (x * ((y ** x) * 5)))
;; (((5 * x) * (y ** x)) + (x * ((y ** x) * 5)))

;; sicp.ch2.s3> (deriv '(((5 * x) * (y ** x)) + (x * ((y ** x) * 5))) 'x)
;; TRACE t12450: (sicp.ch2.s3/deriv (((5 * x) * (y ** x)) + (x * ((y ** x) * 5))) x)
;; TRACE t12451: | (sicp.ch2.s3/deriv ((5 * x) * (y ** x)) x)
;; TRACE t12452: | | (sicp.ch2.s3/deriv (y ** x) x)
;; TRACE t12453: | | | (sicp.ch2.s3/deriv y x)
;; TRACE t12453: | | | => 0
;; TRACE t12452: | | => 0
;; TRACE t12454: | | (sicp.ch2.s3/deriv (5 * x) x)
;; TRACE t12455: | | | (sicp.ch2.s3/deriv x x)
;; TRACE t12455: | | | => 1
;; TRACE t12456: | | | (sicp.ch2.s3/deriv 5 x)
;; TRACE t12456: | | | => 0
;; TRACE t12454: | | => 5
;; TRACE t12451: | => ((y ** x) * 5)
;; TRACE t12457: | (sicp.ch2.s3/deriv (x * ((y ** x) * 5)) x)
;; TRACE t12458: | | (sicp.ch2.s3/deriv ((y ** x) * 5) x)
;; TRACE t12459: | | | (sicp.ch2.s3/deriv 5 x)
;; TRACE t12459: | | | => 0
;; TRACE t12460: | | | (sicp.ch2.s3/deriv (y ** x) x)
;; TRACE t12461: | | | | (sicp.ch2.s3/deriv y x)
;; TRACE t12461: | | | | => 0
;; TRACE t12460: | | | => 0
;; TRACE t12458: | | => 0
;; TRACE t12462: | | (sicp.ch2.s3/deriv x x)
;; TRACE t12462: | | => 1
;; TRACE t12457: | => ((y ** x) * 5)
;; TRACE t12450: => (((y ** x) * 5) + ((y ** x) * 5))
;; (((y ** x) * 5) + ((y ** x) * 5))

;; sicp.ch2.s3> (deriv (convert '((5 * x * y ** x) + (x * ((y ** x) * 5)))) 'x)
;; TRACE t11034: (sicp.ch2.s3/deriv (((5 * x) * (y ** x)) + (x * ((y ** x) * 5))) x)
;; TRACE t11035: | (sicp.ch2.s3/deriv ((5 * x) * (y ** x)) x)
;; TRACE t11036: | | (sicp.ch2.s3/deriv (y ** x) x)
;; TRACE t11037: | | | (sicp.ch2.s3/deriv y x)
;; TRACE t11037: | | | => 0
;; TRACE t11036: | | => 0
;; TRACE t11038: | | (sicp.ch2.s3/deriv (5 * x) x)
;; TRACE t11039: | | | (sicp.ch2.s3/deriv x x)
;; TRACE t11039: | | | => 1
;; TRACE t11040: | | | (sicp.ch2.s3/deriv 5 x)
;; TRACE t11040: | | | => 0
;; TRACE t11038: | | => 5
;; TRACE t11035: | => ((y ** x) * 5)
;; TRACE t11041: | (sicp.ch2.s3/deriv (x * ((y ** x) * 5)) x)
;; TRACE t11042: | | (sicp.ch2.s3/deriv ((y ** x) * 5) x)
;; TRACE t11043: | | | (sicp.ch2.s3/deriv 5 x)
;; TRACE t11043: | | | => 0
;; TRACE t11044: | | | (sicp.ch2.s3/deriv (y ** x) x)
;; TRACE t11045: | | | | (sicp.ch2.s3/deriv y x)
;; TRACE t11045: | | | | => 0
;; TRACE t11044: | | | => 0
;; TRACE t11042: | | => 0
;; TRACE t11046: | | (sicp.ch2.s3/deriv x x)
;; TRACE t11046: | | => 1
;; TRACE t11041: | => ((y ** x) * 5)
;; TRACE t11034: => (((y ** x) * 5) + ((y ** x) * 5))
;; (((y ** x) * 5) + ((y ** x) * 5))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Representing Sets - Sets as Unordered Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-set []
  '())

;; avoiding using the symbol 'set' as parameter because it is a clojrue keyword
(defn element-of-set? [x coll]
  (cond (empty? coll) false
        (= x (first coll)) true
        :else (element-of-set? x (next coll))))

(defn element-of-set? [x coll]
  (and (not (empty? coll))
       (or (= x (first coll))
           (element-of-set? x (next coll)))))

(defn adjoin-set [x coll]
  (if (element-of-set? x coll)
    coll
    (cons x coll)))

(defn intersection-set [set1 set2]
  (cond (or (empty? set1) (empty? set2))
        '()
        (element-of-set? (first set1) set2)
        (cons (first set1) (intersection-set (next set1) set2))
        :else
        (intersection-set (next set1) set2)))

(defn intersection-set [set1 set2]
  (reduce (fn [res e]
            (if (element-of-set? e set2)
              (cons e res)
              res))
          '()
          set1))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.59
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn union-set [set1 set2]
  (if (empty? set1)
    set2
    (union-set (next set1) (adjoin-set (first set1) set2))))

(defn union-set [set1 set2]
  (reduce (fn [res e] (adjoin-set e res))
          set2
          set1))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.60
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;element-of-set? does not change

;; adjoin-set will just be a cons because we do no have to check for membeship
(def adjoin-set cons)

;; union-set will simply be O(n) operation, same as append
(def union-set concat)

;; insersection-set is more interesting
;; we also have to keep track of how many times an element occurs in each set

;; The following is not correct
;; sicp.ch2.s3> (intersection-set '(4 3) '(5 3 6 7 3) )
;; (3)
;; sicp.ch2.s3> (intersection-set '(5 3 6 7 3) '(4 3) )
;; (3 3)

;; intersecion will remain O(n^2) operation
;; If check and then remove, it will be O(n*2n) operation
;; because we have to remove the first occurence of matching element from second set

;; We can improve efficiency by using move-to-front

(defn move-to-front [e coll]
  (if (or (empty? coll) (= e (first coll)))
    coll
    (let [result (move-to-front e (next coll))]
      (if (= e (first result))
        (cons (first result) (cons (first coll) (next result)))
        (cons (first coll) result)))))

(defn intersection-set [set1 set2]
  (if (or (empty? set1) (empty? set2))
    '()
    (let [modified-set2 (move-to-front (first set1) set2)]
      (if (= (first set1) (first modified-set2))
        (cons (first set1) (intersection-set (next set1) (next modified-set2)))
        (intersection-set (next set1) set2)))))

;; sicp.ch2.s3> (intersection-set '(5 3 6 7 3) '(4 3) )
;; (3)
;; sicp.ch2.s3> (intersection-set '(5 3 6 7 3) '(4 3) )
;; (3
;; sicp.ch2.s3> (intersection-set '(5 3 6 7 3 5 5) '(4 3 3 3 5 5) )
;; (5 3 3 5)

;; sets with duplicates are pretty much arbitrary lists.
;; not sure why we'd use this particular represention instead of
;; representing data as straight up lists and use regular list operations.

;; After reading online, this datastructure is called a "multiset" officially. 
;; I suppose conceptually, multisets are different from lists becuase order
;; does not matter with multisets

;; https://oeis.org/wiki/Multisets
;; Above website mentions a use - set of prime factors dividing a number
;; we may wish to compare prime factors for various large numbers
;; see what they have in common using intersect-set

;; I suppose one use is to count number of occurences of an event every day of the
;; week -- like how many times I brush my teeth per day .
;; A multiset represents a week. Then we could use union to collect the occurences
;; per year. Intersect could be used to determine how many times I was consistent
;; in brushing my teeth and on those consistent days how many times I brushed.
;; element-of-set can be used to determine if I happened to brush my teeth
;; more than twice per day on any given week and if that happened,
;; on how many days did that happen (a rather unlikely occurence by any means)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Sets as Ordered Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn element-of-set? [e set1]
  (cond (empty? set1) false
        (= e (first set1)) true
        (< e (first set1)) false
        :else (element-of-set? e (next set1))))

(defn insersection-set [set1 set2]
  (if (or (empty? set1) (empty? set2))
    '()
    (let [x1 (first set1)
          x2 (first set2)]
      (cond 
      (= x1 x2)  (cons x1 (intersection-set (next set1) (next set2)))
      (< x1 x2)  (intersection-set (next set1) set2)
      :else (intersection-set set1 (next set2))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.61
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn adjoin-set [e set1]
  (cond (or (empty? set1) (< e (first set1))) (cons e set1)
        (= e (first set1)) set1
        :else (cons (first set1) (adjoin-set e (next set1)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.62
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn union-set [set1 set2]
  (cond (empty? set1) set2
        (empty? set2) set1
        :else (let [x1 (first set1)
                    x2 (first set2)]
                (cond 
                  (= x1 x2) (cons x1 (union-set (next set1) (next set2)))
                  (< x1 x2) (cons x1 (union-set (next set1) set2))
                  :else (cons x2 (union-set set1 (next set2)))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Sets as Binary Trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn entry [tree] (first tree))
(defn left-branch [tree] (fnext tree))
(defn right-branch [tree] (first (nnext tree)))
(defn make-tree [e left right] (list e left right))

(defn element-of-set? [x s]
  (cond (empty? s) false
        (= x (entry s)) true
        (< x (entry s)) (element-of-set? x (left-branch s))
        :else (element-of-set? x (right-branch s))))


(defn adjoin-set [x s]
  (cond (empty? s) (make-tree x '() '())
        (< x (entry s))))
