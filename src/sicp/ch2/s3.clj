

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
        (= x (entry s)) s
        (< x (entry s)) (make-tree (entry s)
                                   (adjoin-set x (left-branch s))
                                   (right-branch s))
        :else (make-tree (entry s)
                         (left-branch s)
                         (adjoin-set x (right-branch s)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.63
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(defn append [l1 l2] (concat l1 l2))

(defn mycons [e l] (cons e l))

(defn append [l1 l2]
  (if (empty? l1)
    l2
    (mycons (first l1) (append (next l1) l2))))

(defn tree->list-1 [tree]
  (if (empty? tree) '()
      (append (tree->list-1 (left-branch tree))
              (mycons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(defn copy-to-list [t res]
  (if (empty? t)
    res
    (copy-to-list (left-branch t) (mycons (entry t)
                                        (copy-to-list (right-branch t) res)))))

(defn tree->list-2 [tree]    
    (copy-to-list tree '()))


;;;; a ;;;;

;; Yes, same result for all trees

;; (def t1 (reduce (fn [r e] (adjoin-set e r)) '() '(7 3 9 1 5 11)))
;; ;; (7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))

;; (def t2 (reduce (fn [r e] (adjoin-set e r)) '() '(3 1 7 5 9 11)))
;; ;; (3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))

;; (def t3 (reduce (fn [r e] (adjoin-set e r)) '() '(5 3 9 1 7 11)))
;; ;; (5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))

;; (tree->list-1 t1)
;; ;; => (1 3 5 7 9 11)
;; (tree->list-2 t1)
;; ;; => (1 3 5 7 9 11)

;; (tree->list-1 t2)
;; ;; => (1 3 5 7 9 11)
;; (tree->list-2 t2)
;; ;; => (1 3 5 7 9 11)

;; (tree->list-1 t3)
;; ;; => (1 3 5 7 9 11)
;; (tree->list-2 t3)
;; ;; => (1 3 5 7 9 11)



;; NOTE: list->tree defined below, in next exercise.
;; (def bt1 (list->tree [5]))
;; (def bt2 (list->tree [3 5]))
;; (def bt3 (list->tree [5 7]))
;; (def bt4 (list->tree [3 5 7]))
;; (def bt5 (list->tree [2 3 4 5 7 8]))
;; (def bt6 (list->tree [2 3 4 5 6 7 8]))
;; (def bt7 (list->tree (range 1 11)))
;; (def bt8 (list->tree (range 1 21)))
  

;;;; b ;;;;

;; tree->list-2 grows linearly in terms of number of steps.
;; For every element, there are 3 extra steps
;; For n elements, there are (3n+2) total method calls

;; For instance
;; abbreviating tree->list-2 as ttl2, copy-to-list as ctl
;; (ttl2 (5 (3 nil nil) (7 nil nil))))
;; (ctl (5 (3 nil nil) (7 nil nil)))
;; (ctl (3 nil nil) (cons 5 (ctl (7 nil nil) nil)))
;; (ctl (3 nil nil) (cons 5 (ctl nil (cons 7 (ctl nil nil)))))
;; (ctl (3 nil nil) (cons 5 (ctl nil (cons 7 nil))))
;; (ctl (3 nil nil) (cons 5 (ctl nil (7))))
;; (ctl (3 nil nil) (cons 5 (7)))
;; (ctl (3 nil nil) (5 7))
;; (ctl nil (cons 3 (ctl nil (5 7))))
;; (ctl nil (cons 3 (5 7)))
;; (ctl nil (3 5 7))
;; ==> (3 5 7)


;; sicp.ch2.s3> (tree->list-2 (list->tree [3 5 7]))
;; TRACE t10838: (sicp.ch2.s3/tree->list-2 (5 (3 () ()) (7 () ())))
;; TRACE t10839: | (sicp.ch2.s3/copy-to-list (5 (3 () ()) (7 () ())) ())
;; TRACE t10840: | | (sicp.ch2.s3/copy-to-list (7 () ()) ())
;; TRACE t10841: | | | (sicp.ch2.s3/copy-to-list () ())
;; TRACE t10841: | | | => ()
;; TRACE t10842: | | | (sicp.ch2.s3/mycons 7 ())
;; TRACE t10842: | | | => (7)
;; TRACE t10843: | | | (sicp.ch2.s3/copy-to-list () (7))
;; TRACE t10843: | | | => (7)
;; TRACE t10840: | | => (7)
;; TRACE t10844: | | (sicp.ch2.s3/mycons 5 (7))
;; TRACE t10844: | | => (5 7)
;; TRACE t10845: | | (sicp.ch2.s3/copy-to-list (3 () ()) (5 7))
;; TRACE t10846: | | | (sicp.ch2.s3/copy-to-list () (5 7))
;; TRACE t10846: | | | => (5 7)
;; TRACE t10847: | | | (sicp.ch2.s3/mycons 3 (5 7))
;; TRACE t10847: | | | => (3 5 7)
;; TRACE t10848: | | | (sicp.ch2.s3/copy-to-list () (3 5 7))
;; TRACE t10848: | | | => (3 5 7)
;; TRACE t10845: | | => (3 5 7)
;; TRACE t10839: | => (3 5 7)
;; TRACE t10838: => (3 5 7)
;; (3 5 7)



;; tree->list-1 has O(n*logn) growth.
;; tree->list-1 is harder to analyze since growth is not linear with number
;; of elements and growth depends on the structure of tree as well.
;; The number of append operations and the size of list being appended have a 
;; significant influence on the rate of growth and append related operations
;; eclipse the other steps. The number of append related operations depend
;; on how the tree is structured (is it balanced?, are all leaf positions filled?)

;; Since when alalyzing complexity, we look at worst case scenario
;; analysis will be done for a completely balanced tree with all leaf nodes filled.
;; That means we will look at trees with sizes of 1, 3, 7, 15 and so on.
;; These trees have sizes connected to powers of two. That is, 2^1-1 , 2^2-1, 2^3-1
;; and so on

;; Note that append is a 2n+1 operation!

;; Abbreviating tree->list-1 as ttl1, append as 'a'
;; (ttl1 (5 () ())) ;; one step to expand
;; (a (ttl1 ()) (cons 5 (ttl1 ()))) ;; eval left branch
;; (a () (cons 5 (ttl1 ()))) ;; eval right branch
;; (a () (cons 5 ())) ;; cons step
;; (a () (5)) ;; append step
;; (5)
;; So total of 5 steps

;; (ttl1 (5 (3 () ()) (7 () ())))
;; (a (ttl (3 () ())) (cons 5 (ttl (7 () ()))))
;; from earlier example, (ttl (3 () ()) is 5 steps
;; (a (3) (cons 5 (ttl (7 () ()))))
;; Similarly, (ttl (7 () ())) will be 5 steps
;; (a (3) (cons 5 (7)))
;; (a (3) (5 7))
;; Since append is 2n+1 operation, this is 3 steps
;; (3 5 7)
;; total of 15 steps

;; The general pattern is the number of steps will be the
;; expansion step + number of steps in branches + cons step + append steps

;; It will be easier to separate out the analysis for append operations from total
;; number of steps. If append steps are ignored, tree->list-1 is linear (similar to
;; tree->list-2).
;; Expansion step + cons step + left branch steps + right branch steps
;; If either branch is empty, that is one step, otherwise 4 steps each.
;; 3 extra steps for each element
;; Total steps, ignoring append, is 3n+1.

;; For a fully balanced and filled tree of n elements, the last append step
;; consists of appending (n-1)/2 elements.
;; Prior to that, recursively proceeding backwards,
;; there will be 2 append calls with (((n-1)/2)-1)/2 elements
;; because there are two branches with (n-1)/2 elements in each branch

;; Since, as noted before, the number of elemengts in a aa fully balanced tree
;; with all leaf nodes filled will be in steps of 2^m-1, where m = 1,2,3,....

;; Let n = 2^m-1
;; m = log(n+1)
;; Let append[x] stand for no. of steps in appending x elements

;; No. of append operations in last level = 2^0 * ap[(n-1)/2]
;;  = 2^0 * append[((2^m-1)-1)/2]
;;  = 2^0 * append[(2^m-2)/2)]
;;  = 2^0 * append[2^(m-1)-1]

;; No. of append operations in 2nd last level = 2^1 * append[(2^(m-1)-1)/2]
;; = 2^1 * append[2^(m-2)-1]

;; No. of append operations in 3rd last level = 2^2 * append[2^(m-3)-1]

;; We know that we append zero elements in the first level
;; Contnuing the above laid out process,
;; No. of append operations in first level = 2^(m-1) * append[2^(m-m)-1]

;; Total no of append operations
;; = 2^0 *  append[2^(m-1)-1] +
;;   2^1 *  append[2^(m-2)-1] +
;;   2^2 *  append[2^(m-3)-1] +
;;   ------------------------
;;   ------------------------ +
;;   2^(m-1) *  append[2^(m-m)-1]


;; Since append is 2z+1 operation for z elements,
;; No of steps related to append
;; = 2^0 *  (2 * [2^(m-1)-1] +1) +
;;   2^1 *  (2 * [2^(m-2)-1] +1) +
;;   2^2 *  (2 * [2^(m-3)-1] +1) +
;;   ------------------------
;;   ------------------------ +
;;   2^(m-1) * 2 * [2^(m-m)-1] + 1


;; No of steps related to append
;; = 2^0 * (2^m-1) +
;;   2^1 * (2^(m-1)-1) +
;;   2^2 * (2^(m-2)-1) +
;;   ------------
;;   ------------  +
;;  2^(m-1) * (2^1-1)


;; No of steps related to append
;; = 2^m - 2^0 +
;;   2^m - 2^1 +
;;   2^m - 2^2 +
;;   ---------
;;   --------- +
;;   2^m - 2^(m-1)


;; No of steps related to append
;; = m*2^m - [2^0 + 2^1 + 2^2 + ....... + 2^(m-1)]
;; = m*2^m - [2^(m-1+1)-1]
;; = m*2^m - [2^m-1] 
;; = log(n+1) * (n+1) - [n+1 - 1]
;; = [log(n+1) * (n+1)] - n


;;;;;;;;;;;;;;;;;;;;;;;;;
;; So total no. of steps = (3n + 1) +  [log(n+1) * (n+1)] - n
;; = (2n + 1) + log(n+1) * (n+1)
;; For large values of n, log(n+1) * (n+1) will dominate.
;; This is a n*log(n) complexity operation.
;;;;;;;;;;;;;;;;;;;;;;;;;






;; sicp.ch2.s3> (tree->list-1 bt6)
;; TRACE t10996: (sicp.ch2.s3/tree->list-1 (5 (3 (2 () ()) (4 () ())) (7 (6 () ()) (8 () ()))))
;; TRACE t10997: | (sicp.ch2.s3/tree->list-1 (3 (2 () ()) (4 () ())))
;; TRACE t10998: | | (sicp.ch2.s3/tree->list-1 (2 () ()))
;; TRACE t10999: | | | (sicp.ch2.s3/tree->list-1 ())
;; TRACE t10999: | | | => ()
;; TRACE t11000: | | | (sicp.ch2.s3/tree->list-1 ())
;; TRACE t11000: | | | => ()
;; TRACE t11001: | | | (sicp.ch2.s3/mycons 2 ())
;; TRACE t11001: | | | => (2)
;; TRACE t11002: | | | (sicp.ch2.s3/append () (2))
;; TRACE t11002: | | | => (2)
;; TRACE t10998: | | => (2)
;; TRACE t11003: | | (sicp.ch2.s3/tree->list-1 (4 () ()))
;; TRACE t11004: | | | (sicp.ch2.s3/tree->list-1 ())
;; TRACE t11004: | | | => ()
;; TRACE t11005: | | | (sicp.ch2.s3/tree->list-1 ())
;; TRACE t11005: | | | => ()
;; TRACE t11006: | | | (sicp.ch2.s3/mycons 4 ())
;; TRACE t11006: | | | => (4)
;; TRACE t11007: | | | (sicp.ch2.s3/append () (4))
;; TRACE t11007: | | | => (4)
;; TRACE t11003: | | => (4)
;; TRACE t11008: | | (sicp.ch2.s3/mycons 3 (4))
;; TRACE t11008: | | => (3 4)
;; TRACE t11009: | | (sicp.ch2.s3/append (2) (3 4))
;; TRACE t11010: | | | (sicp.ch2.s3/append nil (3 4))
;; TRACE t11010: | | | => (3 4)
;; TRACE t11011: | | | (sicp.ch2.s3/mycons 2 (3 4))
;; TRACE t11011: | | | => (2 3 4)
;; TRACE t11009: | | => (2 3 4)
;; TRACE t10997: | => (2 3 4)
;; TRACE t11012: | (sicp.ch2.s3/tree->list-1 (7 (6 () ()) (8 () ())))
;; TRACE t11013: | | (sicp.ch2.s3/tree->list-1 (6 () ()))
;; TRACE t11014: | | | (sicp.ch2.s3/tree->list-1 ())
;; TRACE t11014: | | | => ()
;; TRACE t11015: | | | (sicp.ch2.s3/tree->list-1 ())
;; TRACE t11015: | | | => ()
;; TRACE t11016: | | | (sicp.ch2.s3/mycons 6 ())
;; TRACE t11016: | | | => (6)
;; TRACE t11017: | | | (sicp.ch2.s3/append () (6))
;; TRACE t11017: | | | => (6)
;; TRACE t11013: | | => (6)
;; TRACE t11018: | | (sicp.ch2.s3/tree->list-1 (8 () ()))
;; TRACE t11019: | | | (sicp.ch2.s3/tree->list-1 ())
;; TRACE t11019: | | | => ()
;; TRACE t11020: | | | (sicp.ch2.s3/tree->list-1 ())
;; TRACE t11020: | | | => ()
;; TRACE t11021: | | | (sicp.ch2.s3/mycons 8 ())
;; TRACE t11021: | | | => (8)
;; TRACE t11022: | | | (sicp.ch2.s3/append () (8))
;; TRACE t11022: | | | => (8)
;; TRACE t11018: | | => (8)
;; TRACE t11023: | | (sicp.ch2.s3/mycons 7 (8))
;; TRACE t11023: | | => (7 8)
;; TRACE t11024: | | (sicp.ch2.s3/append (6) (7 8))
;; TRACE t11025: | | | (sicp.ch2.s3/append nil (7 8))
;; TRACE t11025: | | | => (7 8)
;; TRACE t11026: | | | (sicp.ch2.s3/mycons 6 (7 8))
;; TRACE t11026: | | | => (6 7 8)
;; TRACE t11024: | | => (6 7 8)
;; TRACE t11012: | => (6 7 8)
;; TRACE t11027: | (sicp.ch2.s3/mycons 5 (6 7 8))
;; TRACE t11027: | => (5 6 7 8)
;; TRACE t11028: | (sicp.ch2.s3/append (2 3 4) (5 6 7 8))
;; TRACE t11029: | | (sicp.ch2.s3/append (3 4) (5 6 7 8))
;; TRACE t11030: | | | (sicp.ch2.s3/append (4) (5 6 7 8))
;; TRACE t11031: | | | | (sicp.ch2.s3/append nil (5 6 7 8))
;; TRACE t11031: | | | | => (5 6 7 8)
;; TRACE t11032: | | | | (sicp.ch2.s3/mycons 4 (5 6 7 8))
;; TRACE t11032: | | | | => (4 5 6 7 8)
;; TRACE t11030: | | | => (4 5 6 7 8)
;; TRACE t11033: | | | (sicp.ch2.s3/mycons 3 (4 5 6 7 8))
;; TRACE t11033: | | | => (3 4 5 6 7 8)
;; TRACE t11029: | | => (3 4 5 6 7 8)
;; TRACE t11034: | | (sicp.ch2.s3/mycons 2 (3 4 5 6 7 8))
;; TRACE t11034: | | => (2 3 4 5 6 7 8)
;; TRACE t11028: | => (2 3 4 5 6 7 8)
;; TRACE t10996: => (2 3 4 5 6 7 8)
;; (2 3 4 5 6 7 8)


;; if n=3, steps = 7 + log4 * 4 = 15. This checks out as seen from
;; substitution model steps expanded above for tree (5 (3 () ()) (7 () ()))

;; if n=7, steps = 15 + log8 * 8 = 39. This checks out as seen from trace above
;; for tree (5 (3 (2 () ()) (4 () ())) (7 (6 () ()) (8 () ())))

;; if n=15, steps = 31 + log16 * 16 = 31 + 64 = 95. This checks out because
;; no of steps generally is 2 + 2 * [steps for (n-1)/2] + append[(n-1)/2]
;; = 2 + 2* 39 + append [7]
;; = 2 + 78 + 15
;; = 95








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn partial-tree [elts n]
  (if (= n 0)
    (cons '() elts)
    (let [left-size (quot (dec n) 2)
          left-result (partial-tree elts left-size)
          left-tree (first left-result)
          non-left-elts (next left-result)
          right-size (- n (inc left-size))
          this-entry (first non-left-elts)
          right-result (partial-tree (next non-left-elts) right-size)
          right-tree (first right-result)
          remaing-elts (next right-result)]
      (cons (make-tree this-entry left-tree right-tree)
            remaing-elts))))

(defn list->tree [elems]
  (first (partial-tree elems (count elems))))



;;;; a ;;;;

;; This is a recursive algorithm that breaks the given list into roughly equal sized
;; parts and calls itself with the separate parts as inputs. The result from first
;; part of size (qout (/ (dec n 2)) will form the left branch and the result from
;; second part of size (- n (inc left-size0) will make right-branch. Parent will
;; the element in the middle obtained as first element after the left branch is
;; processed. An importan thing to notice about the algorithm is that the input
;; list is traversed only once -- a clever trick that utilizes the nature of
;; list datastructure and processes left subtrees first.
 
;; Tree for (1 3 5 7 9 11)
;;                5
;;           3         9
;;        1         7    11



;;;; b ;;;;

;; Order of growth in the number of steps is O(n)

;; Let the number of elements in input list to partial-tree procedure be n
;; and let
;; n = 2^m-1
;; ls = (quot (/ (dec) 2))
;; rs = (-n (inc ls)) 
;; Notice that, regrdless of whether n is even or odd, ls+rs will always be n-1

;; Let the following be the values for input argument 'n' at each recursive level
;; So, b will be left-size when n=a, c will be right-size
;; d will be left-size when n=b, g will be rightsize when n=c and so on
;;                a
;;           b         c
;;         d   e     f   g
;;        h i j k   l m n o

;; Sums of values of input parameter 'n' at each recurisve level
;; a                            = a-0   = a+1 - 2^0
;; b+c                          = a-1   = a+1 - 2^1
;; d+e+f+g= b-1 + c -1          = a-3   = a+1 - 2^2
;; h+i+j+k+l+m+n+o = d+e+f+g-4  = a-7   = a+1 - 2^3
;; and so on

;; We are interested in the sum of the values of 'n' at each level
;; in order to determine how many levels deep recursive calls will be made.
;; Recursion stops when the value for 'n' is 0.

;; Let x be the number of steps performed with each call to partial-tree
;; Since the procedure recursively calls itself twice each time,

;; Recursion level | Times partial-tree called | Total value of n across level
;;           1     |          2^0              |      n === 2^m-2^0
;;           2     |          2^1              |    n-1 === 2^m-2^1
;;           3     |          2^2              |            2^m-2^2
;;           4     |          2^3              |            2^m-2^3
;; -----------------------------------------------------------------
;; -----------------------------------------------------------------
;;           m     |         2^m-1             |            2^m-2^(m-1)
;;          m+1    |         2^m               |            2^m-2^m 

;; Since recursion stops when total value of n across a level is 0,
;; and number of steps is 1 when argument value n=0 and x otherwise, 

;; Total number of steps = x * [2^0 + 2^1 + 2^2 + 2^3 + ..... + 2^(m-1)]+ 2^m
;;     =  x * [2^m-1] + 2^m
;;     = (x * n) + (n + 1)

;; So, including the first call to list->tree, total steps will be
;; n(x+1) + 2
;; This is linear in n, where x is number of fixed steps per call to partial-tree.






