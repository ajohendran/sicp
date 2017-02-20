(ns sicp.ch2.s1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Pairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; In Common Lisp and Scheme, consing a symbol onto another one
;; creates a strcuture called pair, represented as "dotted pair".
;; Clojure does not have the concept of dotted pairs.
;; Can't use cons unless the second argument is a sequence
;; So instead, we will define the following procedures
;; make-pair instead of cons
;; car 
;; cdr
;; Internally, they will be represented by a regular list.
;; Hopefully these won't have to be used much and we will instead be using
;; sequence data structures in following chapters and sections


(defn make-pair [elm1 elm2]
  (list elm1 elm2)) ;; list will be explained in next section

(defn car [pair]
  (first pair))

(defn cdr [pair]
  (second pair))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Rational Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-rat [n d]
  (make-pair n d))

(defn numer [x] (car x))

(defn denom [x] (cdr x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))


(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))


(defn print-rat [x]
  (print (numer x))
  (print "/")
  (print (denom x)))


(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))


(defn make-rat [n d]
  (let [g (gcd n d)]
    (make-pair (/ n g)
            (/ d g ))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-rat [n d]
  (let [abs-n (Math/abs n)
        abs-d (Math/abs d)
        g (gcd abs-n abs-d)
        sign (cond (and (< n 0) (> d 0)) -1
                   (and (> n 0) (< d 0)) -1
                   :else 1)]
    (make-pair (* sign (/ abs-n g))
            (/ abs-d g ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn make-point [x y]
  (make-pair x y))

(defn x-point [point]
  (car point))

(defn y-point [point]
  (cdr point))


(defn make-segment [start-point end-point]
  (make-pair start-point end-point))

(defn start-segment [segment]
  (car segment))

(defn end-segment [segment]
  (cdr segment))

;; String literals are bing used in book without proper itroduction
;; Might as well use str procedure to concatenate them
(defn print-point [p]
  (println (str "(" (x-point p) "," (y-point p) ")")))

(defn print-segment [s]
  (println (str "[" (start-segment s) "," (end-segment s) "]")))

(defn avg [a b]
  (/ (+ a b) 2.0))

(defn midpoint-segment [s]
  (let [midpoint-x (avg (x-point (start-segment s)) (x-point (end-segment s)))
        midpoint-y (avg (y-point (start-segment s)) (y-point (end-segment s)))]
    (make-point midpoint-x midpoint-y)))


;; (def point-a (make-point -2 -1))
;; (def point-b (make-point 5 7))
;; (def seg-ab (make-segment point-a point-b))
;; (print-point (midpoint-segment seg-ab))


;; Some segment level helper functions
(defn square [x] (* x x))

(defn x-length-segment [seg]
  (Math/abs (- (x-point (start-segment seg))
               (x-point (end-segment seg)))))

(defn y-length-segment [seg]
  (Math/abs (- (y-point (start-segment seg))
               (y-point (end-segment seg)))))

;; not necesary for this exercise but good to have
(defn length-segment [seg]
  (Math/sqrt (+ (square (x-length-segment seg))
                (square (y-length-segment seg)))))


;; (def point-a (make-point -7 1))
;; (def point-b (make-point 1 -5))
;; (def seg-ab (make-segment point-a point-b))
;; sicp.ch2.s1> (length-segment seg-ab)
;; 10.0








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;; First rectangle representation ;;;;;;;;;

;; First representation is as as two points, essentially the digonal segment
;; The smaller x value will always come first
;; though not necessary for this exercise
(defn make-rectangle [point1 point2]
  (cond (or (= (x-point point1) (x-point point2))
            (= (y-point point1) (y-point point2)))
        (ex-info "Rectangle improperly defined" {point1, point2})
        (< (x-point point1) (x-point point2))
        (make-segment point1 point2)
        :else
        (make-segment point2 point1)))

(defn width-rectangle [rect]
  (x-length-segment rect))

(defn height-rectangle [rect]
  (y-length-segment rect))

(defn left-top-corner-rectangle [rect]
  (if (> (y-point (start-segment rect))
         (y-point (end-segment rect)))
    (start-segment rect)
    (make-point (x-point (start-segment rect))
                (+ (height-rectangle rect) (y-point (start-segment rect))))))

;; (def right-bottom-corner (make-point 1 -5))
;; (def left-top-corner (make-point -7 1))
;; (def my-rect (make-rectangle right-bottom-corner left-top-corner))
;; (height-rectangle my-rect)
;; 6
;; (width-rectangle my-rect)
;; 8
;; (corners-rectangle my-rect)
;; (((-7 1) (1 1)) ((-7 -5) (1 -5)))


;;;;;;;;;;;; Second rectangle representation ;;;;;;;;;;

;; Represented by the upper left corner and width and height
(defn make-rectangle [left-upper-corner-point, width, height]
  (make-pair left-upper-corner-point (make-pair width height)))

(defn width-rectangle [rect]
  (car (cdr rect)))

(defn height-rectangle [rect]
  (cdr (cdr rect)))

(defn left-top-corner-rectangle [rect]
  (car rect))


;; (def left-top-corner (make-point -7 1))
;; (def my-rect (make-rectangle left-top-corner 8 6))
;; (height-rectangle my-rect)
;; 6
;; (width-rectangle my-rect)
;; 8
;; (corners-rectangle my-rect)
;; (((-7 1) (1 1)) ((-7 -5) (1 -5)))

;;;;;;;;;;;; Area and Permieter functions  ;;;;;;;;;;

;; these do not chage with representartio
(defn area-rectangle [rect]
  (* (width-rectangle rect) (height-rectangle rect)))

(defn perimeter-rectangle [rect]
  (+ (* 2 (width-rectangle rect))
     (* 2 (height-rectangle rect))))


;; (area-rectangle my-rect)
;; 48
;; (perimeter-rectangle my-rect)
;; 28


;; Some additional functions, not asked for in book but illustrates the principles 
(defn corners-rectangle [rect]
  (let [width (width-rectangle rect)
        height (height-rectangle rect)
        left-top-corner (left-top-corner-rectangle rect)
        left-bottom-corner (make-point (x-point left-top-corner)
                                       (- (y-point left-top-corner) height))
        right-bottom-corner (make-point (+ (x-point left-bottom-corner) width) 
                                        (y-point left-bottom-corner))
        right-top-corner (make-point (+ (x-point left-top-corner) width) 
                                        (y-point left-top-corner))]
    (make-pair
     (make-pair left-top-corner right-top-corner)
     (make-pair left-bottom-corner right-bottom-corner))))

;; clockwise starting from left top corner
(defn print-rectangle [rect]
  (let [corners (corners-rectangle rect)]
    (print-segment (make-segment (car (car corners)) (cdr (car corners))))
    (print-segment (make-segment (cdr (car corners)) (cdr (cdr corners))))
    (print-segment (make-segment (cdr (cdr corners)) (car (cdr corners))))
    (print-segment (make-segment (car (cdr corners)) (car (car corners))))))

;; (print-rectangle my-rect)
;; [(-7 1),(1 1)]
;; [(1 1),(1 -5)]
;; [(1 -5),(-7 -5)]
;; [(-7 -5),(-7 1)]
;; nil


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  What Is Meant by Data?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-pair [x y]
  (fn [m] (cond (= m 0) x
                (= m 1) y
                :else (ex-info "Argument not 0 or 1" {}))))

(defn car [p]
  (p 0))

(defn cdr [p]
  (p 1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-pair [x y]
  (fn [m] (m x y)))

(defn car [p]
  (p (fn [a b] a)))

(defn cdr [p]
  (p (fn [a b] b)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; copied over from chapter 1.
;; another option would have been to use (long (Math/pow x y))
(defn expt
  ([b n] (expt b n 1))
  ([b n a]
   (cond (< n 1) a
         (even? n) (recur (* b b) (/ n 2) a)
         :else (recur b (dec n) (* a b)))))

;; determine how many times 'a' can be repeatedly be divided by 'b'.
(defn times-divisible-by [a b c]
  (if (= 0 (rem a b))
    (recur (/ a b) b (inc c))
    c))

;; any two co-prime numbers can be used.
;; Of course, mkes sense to use the smallest available co-prime numbers
(defn make-pair-i [a b]
  (* (expt 2 a)
     (expt 3 b)))

(defn car-i [p]
  (times-divisible-by p 2 0))

(defn cdr-i [p]
  (times-divisible-by p 3 0))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def zero (fn [f] (fn [x] x)))

(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))


(def one (add-1 zero))

;; (add-1 zero)
;; (add-1 (fn [f] (fn [x] x)))
;; ((fn [n] (fn [f] (fn [x] (f ((n f) x)))))
;;   (fn [f] (fn [x] x)))
;; (fn [f] (fn [x] (f (((fn [f] (fn [x] x)) f) x))))
;; (fn [f] (fn [x] (f ((fn [x] x) x))))
;; (fn [f] (fn [x] (f x)))

;; One -> (fn [f] (fn [x] (f x)))

(def two (add-1 one))

;; (add-1 one)
;; (add-1 (fn [f] (fn [x] (f x))))
;; ((fn [n] (fn [f] (fn [x] (f ((n f) x)))))
;;    (fn [f] (fn [x] (f x))))
;; (fn [f] (fn [x] (f (((fn [f] (fn [x] (f x))) f) x))))
;; (fn [f] (fn [x] (f ((fn [x] (f x)) x))))
;; (fn [f] (fn [x] (f (f x))))

;; Two -> (fn [f] (fn [x] (f (f x))))


;; pattern is clear. Going by above
;; Three -> (fn [f] (fn [x] (f (f (f x)))))


;; Zero  -> (fn [f] (fn [x] x))
;; One   -> (fn [f] (fn [x] (f x)))
;; Two   -> (fn [f] (fn [x] (f (f x))))
;; Three -> (fn [f] (fn [x] (f (f (f x)))))
;; add-1 -> (fn [n] (fn [f] (fn [x] (f ((n f) x)))))

;; Essentially numbers are represented by two functions - outer function with parameter f
;; that returns an inner function with parameter x
;; A number's value is represented by how many times parameter to outer function
;; f is repeatedly applied to parameter of inner function.


;; Notice the construction for add-1. It is very similar to one.
;; At the innermost level, 'one' has (f x), whereas
;; add-1 has (f ((n f) x))

;; One is represented by one application of outer function parameter f.
;; So defintion of add-1 makes sense.
;; Adding one to zero must return one. So (f ((n f) x)) must return (f x).
;; Adding one to one must return two, that is, (f (f x))

;; So add-two would be (fn [n] (fn [f] (fn [x] (f (f ((n f) x)))))

;; what (n f) does is the key here. It must return the value of the actual
;; number being added

;; It is easy to see (n f) would just return x for zero and (f x) for one
;; and (f (f x)) for two and so on

;; When n is zero
;; ((n f) x) where n is zero
;; (((fn [f] (fn [x] x)) f) x)
;; ((fn [x] x) x)
;; x

;; when n is one
;; ((n f) x) where n is one
;; (((fn [f] (fn [x] (f x))) f) x)
;; ((fn [x] (f x)) x)
;; (f x)

;; when n is two
;; ((n f) x) where n is two
;; (((fn [f] (fn [x] (f (f x)))) f) x)
;; ((fn [x] (f (f x))) x)
;; (f (f x))

;; So adding two numbers m and n

(def add-mn
  (fn [m n] (fn [f] (fn [x] ((m f) ((n f) x))))))


;;or

(defn add-mn
  (fn [f] (fn [x] ((m f) ((n f) x)))))

;; Only difference from add-1 is that inner (f ((n f) x))
;; has been changed to ((m f) ((n f) x))


;; Let us test by adding m->two and n->one
((fn [m n] (fn [f] (fn [x] ((m f) ((n f) x)))))
 two
 one)

;; ((fn [m n] (fn [f] (fn [x] ((m f) ((n f) x)))))
;;  (fn [f] (fn [x] (f (f x))))
;;  (fn [f] (fn [x] (f x))))

;; (fn [f] (fn [x] (((fn [f] (fn [x] (f (f x)))) f)
;;                  (((fn [f] (fn [x] (f x))) f) x))))

;; (fn [f] (fn [x] (((fn [f] (fn [x] (f (f x)))) f)
;;                  (((fn [f] ) f) x))))

;; (fn [f] (fn [x] (((fn [f] (fn [x] (f (f x)))) f)
;;                  ((fn [x] (f x)) x))))

;; (fn [f] (fn [x] (((fn [f] (fn [x] (f (f x)))) f)
;;                  (f x))))

;; (fn [f] (fn [x] ((fn [x] (f (f x))) (f x)))) 

;; (fn [f] (fn [x] (f (f (f x)))))

;; which is three




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Interval Arithmetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



