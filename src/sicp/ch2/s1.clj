
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



;; (defprotocol Pair
;;   "Protocol to represent scheme's pair data structure"
;;   (car [this])
;;   (cdr [this]))

;; ;; implementation of Pair protocol using as type
;; (deftype PairType [first second]
;;   Pair
;;   (car [_] first)
;;   (cdr [_] second)
;;   (toString [_] (str "(" first " . " second ")")))

;; (defn make-pair [elm1 elm2]
;;   (->PairType elm1 elm2))

;; ;; implementation of Pair protocol as record
;; (deftype PairRecord [first second]
;;   Pair
;;   (car [_] first)
;;   (cdr [_] second)
;;   (toString [_] (str "(" first " . " second ")")))

;; (defn make-pair [elm1 elm2]
;;   (->PairRecord elm1 elm2))

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

(defn add-mn [m n]
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
;;;;  Ex 2.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-pair [elm1 elm2]
  (list elm1 elm2)) 

(defn car [pair]
  (first pair))

(defn cdr [pair]
  (second pair))

;; modified from version in book to make sure right object is construced
;; even if parameters are specified in  wrong order
(defn make-interval [lb ub]
  (make-pair (min lb ub) (max lb ub)))

(defn lower-bound [i]
  (car i))

(defn upper-bound [i]
  (cdr i))



(defn add-interval [x y]
  (println "Add" x "to" y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(defn mul-interval [x y]
  (println "Mul" x "with" y)
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(defn div-interval [x y]
  (println "Div" x "by" y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unlike multiplication we needn't worry about sign here.
(defn sub-interval [x y]
  (println "Sub" y "from" x)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Interval Arithmetic Test Set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; These exercises are not that well defined - a bit confusing

;; (def i1pp (make-interval 4.5 5.5))
;; (def i2pp (make-interval 5.0 7.0))

;; (def i1nn (make-interval -5.5 -4.5))
;; (def i2nn (make-interval -7.0 -5.0))

;; (def i1np (make-interval -14.5 5.5))
;; (def i2np (make-interval -15.0 7.0))


;; (defn show-muls [x y]
;;   (let [p1 (* (lower-bound x) (lower-bound y))
;;         p2 (* (lower-bound x) (upper-bound y))
;;         p3 (* (upper-bound x) (lower-bound y))
;;         p4 (* (upper-bound x) (upper-bound y))]
;;     (vector p1 p2 p3 p4)))


;; (defn show-adds [x y]
;;   (let [p1 (+ (lower-bound x) (lower-bound y))
;;         p2 (+ (lower-bound x) (upper-bound y))
;;         p3 (+ (upper-bound x) (lower-bound y))
;;         p4 (+ (upper-bound x) (upper-bound y))]
;;     (vector p1 p2 p3 p4)))


;; (defn show-subs [x y]
;;   (let [p1 (- (lower-bound x) (lower-bound y))
;;         p2 (- (lower-bound x) (upper-bound y))
;;         p3 (- (upper-bound x) (lower-bound y))
;;         p4 (- (upper-bound x) (upper-bound y))]
;;     (vector p1 p2 p3 p4)))



;; (doseq [i1 [i1pp i1nn i1np]
;;   i2 [i2pp i2nn i2np]
;;   proc [sub-interval]]
;;   (println (proc i1 i2))
;;   (println (show-subs i1 i2))
;;   (newline))

;; (doseq [i1 [[3 4]]
;;   i2 [[-5 -4]]
;;   proc [sub-interval]]
;;   (println (proc i1 i2))
;;   (println (show-subs i1 i2))
;;   (newline))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn width-interval [i]
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

;; Consider two intervals (a . b) and (c . d)
;; a < b and c < d
;; Widths are (b-a)/2 and (d-c)/2. Let's call them m and n.
;; That is m -> (b-a)/2 and n -> (d-c)/2

;; Addition yields (a+c . b+d). Width would be ((b+d) - (a+c))/2
;; ((b-a) + (d-c))/2
;; (b-a)/2 + (d-c)/2
;; m + n

;; Subtraction yields (a-d . b-c). Width would be ((b-c) - (a-d))/2
;; ((b-a) + (d-c))/2
;; (b-a)/2 + (d-c)/2
;; m + n



;; Examples for Addition

;; Add (4.5 5.5) to (5.0 7.0)
;; (9.5 12.5)
;; widths: 0.5 1.0 1.5

;; Add (4.5 5.5) to (-7.0 -5.0)
;; (-2.5 0.5)
;; widths: 0.5 1.0 1.5

;; Add (4.5 5.5) to (-15.0 7.0)
;; (-10.5 12.5)
;; widths: 0.5 11.0 11.5

;; Add (-5.5 -4.5) to (5.0 7.0)
;; (-0.5 2.5)
;; widths: 0.5 1.0 1.5

;; Add (-5.5 -4.5) to (-7.0 -5.0)
;; (-12.5 -9.5)
;; widths: 0.5 1.0 1.5

;; Add (-5.5 -4.5) to (-15.0 7.0)
;; (-20.5 2.5)
;; widths: 0.5 11.0 11.5

;; Add (-14.5 5.5) to (5.0 7.0)
;; (-9.5 12.5)
;; widths: 10.0 1.0 11.0

;; Add (-14.5 5.5) to (-7.0 -5.0)
;; (-21.5 0.5)
;; widths: 10.0 1.0 11.0

;; Add (-14.5 5.5) to (-15.0 7.0)
;; (-29.5 12.5)
;; widths: 10.0 11.0 21.0



;; Examples for Subtraction

;; Sub (5.0 7.0) from (4.5 5.5)
;; (-2.5 0.5)
;; widths: 0.5 1.0 1.5

;; Sub (-7.0 -5.0) from (4.5 5.5)
;; (9.5 12.5)
;; widths: 0.5 1.0 1.5

;; Sub (-15.0 7.0) from (4.5 5.5)
;; (-2.5 20.5)
;; widths: 0.5 11.0 11.5

;; Sub (5.0 7.0) from (-5.5 -4.5)
;; (-12.5 -9.5)
;; widths: 0.5 1.0 1.5

;; Sub (-7.0 -5.0) from (-5.5 -4.5)
;; (-0.5 2.5)
;; widths: 0.5 1.0 1.5

;; Sub (-15.0 7.0) from (-5.5 -4.5)
;; (-12.5 10.5)
;; widths: 0.5 11.0 11.5

;; Sub (5.0 7.0) from (-14.5 5.5)
;; (-21.5 0.5)
;; widths: 10.0 1.0 11.0

;; Sub (-7.0 -5.0) from (-14.5 5.5)
;; (-9.5 12.5)
;; widths: 10.0 1.0 11.0

;; Sub (-15.0 7.0) from (-14.5 5.5)
;; (-21.5 20.5)
;; widths: 10.0 11.0 21.0



;; Examples for Multiplication

;; Mul (4.5 5.5) with (5.0 7.0)
;; (22.5 38.5)
;; widths: 0.5 1.0 8.0

;; Mul (4.5 5.5) with (-7.0 -5.0)
;; (-38.5 -22.5)
;; widths: 0.5 1.0 8.0

;; Mul (4.5 5.5) with (-15.0 7.0)
;; (-82.5 38.5)
;; widths: 0.5 11.0 60.5

;; Mul (-5.5 -4.5) with (5.0 7.0)
;; (-38.5 -22.5)
;; widths: 0.5 1.0 8.0

;; Mul (-5.5 -4.5) with (-7.0 -5.0)
;; (22.5 38.5)
;; widths: 0.5 1.0 8.0

;; Mul (-5.5 -4.5) with (-15.0 7.0)
;; (-38.5 82.5)
;; widths: 0.5 11.0 60.5

;; Mul (-14.5 5.5) with (5.0 7.0)
;; (-101.5 38.5)
;; widths: 10.0 1.0 70.0

;; Mul (-14.5 5.5) with (-7.0 -5.0)
;; (-38.5 101.5)
;; widths: 10.0 1.0 70.0

;; Mul (-14.5 5.5) with (-15.0 7.0)
;; (-101.5 217.5)
;; widths: 10.0 11.0 159.5



;; Examples for Division

;; Div (4.5 5.5) by (5.0 7.0)
;; Mul (4.5 5.5) with (0.14285714285714285 0.2)
;; (0.6428571428571428 1.1)
;; widths: 0.5 1.0 0.22857142857142865

;; Div (4.5 5.5) by (-7.0 -5.0)
;; Mul (4.5 5.5) with (-0.2 -0.14285714285714285)
;; (-1.1 -0.6428571428571428)
;; widths: 0.5 1.0 0.22857142857142865

;; Div (-5.5 -4.5) by (5.0 7.0)
;; Mul (-5.5 -4.5) with (0.14285714285714285 0.2)
;; (-1.1 -0.6428571428571428)
;; widths: 0.5 1.0 0.22857142857142865

;; Div (-5.5 -4.5) by (-7.0 -5.0)
;; Mul (-5.5 -4.5) with (-0.2 -0.14285714285714285)
;; (0.6428571428571428 1.1)
;; widths: 0.5 1.0 0.22857142857142865

;; Div (-14.5 5.5) by (5.0 7.0)
;; Mul (-14.5 5.5) with (0.14285714285714285 0.2)
;; (-2.9000000000000004 1.1)
;; widths: 10.0 1.0 2.0

;; Div (-14.5 5.5) by (-7.0 -5.0)
;; Mul (-14.5 5.5) with (-0.2 -0.14285714285714285)
;; (-1.1 2.9000000000000004)
;; widths: 10.0 1.0 2.0



;; (doseq [i1 [i1pp i1nn i1np]
;;         i2 [i2pp i2nn i2np]
;;         proc [mul-interval]]
;;   (let [res (proc i1 i2)]
;;     (println res)
;;     (println "widths:" (width-interval i1) (width-interval i2) (width-interval res))
;;     (newline)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn zero-spanning? [i]
  (and (neg? (lower-bound i)) (pos? (upper-bound i))))


(defn div-interval [x y]
  (println "Div" x "by" y)
  (if (or (zero? (upper-bound y))
          (zero? (lower-bound y))
          (zero-spanning? y))
    (throw (RuntimeException. "Divisor Interval spans zero")))
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mul-interval-old [x y]
  (println "Mul Old" x "with" y)
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


;; sicp.ch2.s1> [i1pp i1nn i1np]
;; [(4.5 5.5) (-5.5 -4.5) (-14.5 5.5)]
;; sicp.ch2.s1> [i2pp i2nn i2np]
;; [(5.0 7.0) (-7.0 -5.0) (-15.0 7.0)]

(defn mul-interval [x y]
  (println "Mul" x "with" y)
  (let [xl (lower-bound x)
        xu (upper-bound x)
        yl (lower-bound y)
        yu (upper-bound y)]
    (cond
      (and (pos? xl) (pos? yl)) (make-interval (* xl yl) (* xu yu))
      (and (pos? xl) (neg? yu)) (make-interval (* xu yl) (* xl yu))
      (and (pos? xl) (zero-spanning? y)) (make-interval (* xu yl) (* xu yu))
      (and (neg? xu) (pos? yl)) (make-interval (* xl yu) (* xu yl))
      (and (neg? xu) (neg? yu)) (make-interval (* xu yu) (* xl yl))
      (and (neg? xu) (zero-spanning? y)) (make-interval (* xl yu) (* xl yl))
      (and (zero-spanning? x) (pos? yl)) (make-interval (* xl yu) (* xu yu))
      (and (zero-spanning? x) (neg? yu)) (make-interval (* xu yl) (* xl yl))
      (and (zero-spanning? x) (zero-spanning? y)) (make-interval (min (* xl yu) (* xu yl))
                                                                 (max (* xl yl) (* xu yu))))))



;; (doseq [i1 [i1pp i1nn i1np]
;;         i2 [i2pp i2nn i2np]
;;         proc [mul-interval-old mul-interval]]
;;   (let [res (proc i1 i2)]
;;     (println res)
;;     (newline)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn center-interval [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))


(defn width-interval [i]
  (/ (- (upper-bound i) (lower-bound i)) 2.0))


(defn make-center-percent  [c p]
  (let [w (/ (* c p) 100.0)]
    (make-center-width c w)))


(defn percent-interval [i]
  (let [w (width-interval i)]
    (/ (* 100.0 w) (center-interval i))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;(def ti1 (make-center-percent 10 1))
; (def ti2 (make-center-percent 50 2))


;; sicp.ch2.s1> (percent-interval (mul-interval (make-center-percent 100 0.05)
;;                                              (make-center-percent 5 0.05) ))
;; 0.09999997500001193

;; sicp.ch2.s1> (percent-interval (mul-interval (make-center-percent 100 0.05)
;;                                              (make-center-percent 5 0.1) ))
;; 0.14999992500003181

;; sicp.ch2.s1> (percent-interval (mul-interval (make-center-percent 100 0.02)
;;                                              (make-center-percent 5 0.5) ))
;; 0.5199994800005245

;; sicp.ch2.s1> (percent-interval (mul-interval (make-center-percent -100 0.02)
;;                                              (make-center-percent 5 0.5) ))
;; -0.5199994800005245

;; sicp.ch2.s1> (percent-interval (mul-interval (make-center-percent -100 0.02)
;;                                              (make-center-percent -5 0.5) ))
;; 0.5199994800005245

;; sicp.ch2.s1> (percent-interval (mul-interval (make-center-percent 100 0.02)
;;                                              (make-center-percent -5 0.5) ))
;; -0.5199994800005245


;; From testing, it is clear that the percent tolerance of multiplication result is
;; approximately the sum of individual percent tolerances


;; Let X, Y be two centers. Let p1 and p2 be percentage tolerances.
;; Let us assume the percentages p1 and p2 are expressed in decimal
;; Interval X -> [X-(X*p1) , (X+(X*p1)]
;; Can also be expressed as X ->  [X*(1-p1) , X*(1+p1)]
;; Interval Y -> [Y-(Y*p2) , (Y+(Y*p2)]
;; Can also be expressed as Y -> [Y*(1-p2) , Y*(1+p2)]

;; Multiplication
;; X * Y -> [ (X-(X*p1)) * (Y-(Y*p2)) , (X+(X*p1) * (Y+(Y*p2) ]
;; [ X*(1-p1) * Y*(1-p2) , X*(1+p1) * Y*(1+p2)]
;; [ X * Y * (1-p1)*(1-p2)  , X* Y * (1+p1)(1+p2) ]
;; [ X*Y* (1-p2-p1+p1p2) , X*Y* (1+p2+p1+p1p2) ]
;; If p1*p2 is small/negligible
;; [ X*Y* (1-(p1+p2))  ,  X*Y* (1+(p1+p2)) ]

;; So if p1*p2 is negligible, the tolerance for multiplication result is
;; the sum of individual tolerances

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.14
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))


(defn par2 [r1 r2]
  (let [one (make-interval 1 1)] 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; Using the resistor values given in the beginning of the exercise,
;; (def r1 (make-center-percent 6.8 10.0))
;; (def r2 (make-center-percent 4.7 5.0))

;; (par1 r1 r2)
;; (2.201031010873943 3.4873689182805854)
;; (par2 r1 r2)
;; (2.581558809636278 2.97332259363673)



;; (def r1-mulr2-divr2 (div-interval (mul-interval r1 r2) r2))
;; r1-mulr2-divr2
;; (5.537142857142857 8.267368421052632)
;; (center-interval r1-mulr2-divr2)
;; 6.902255639097745
;; (percent-interval r1-mulr2-divr2)
;; 19.77777777777778
;; (r1*r2)/r2 is algebraically quivalent to r1, but both center and tolerance are off

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (def A (make-center-percent 100 0.1))
;; (def B (make-center-percent 50 0.2))

;; (def A-div-A (div-interval A A))
;; A-div-A
;; (0.9980019980019981 1.002002002002002)
;; (center-interval A-div-A)
;; 1.000002000002
;; (percent-interval A-div-A)
;; 0.19999980000019435

;; We can see what happened , based on what we learned from  Ex 2.13
;; Div (99.9 100.1) by (99.9 100.1)
;; is executed as
;; Mul (99.9 100.1) with (0.00999000999000999 0.01001001001001001)
;; As seen in exercise 2.13, the tolerances get added for multiplication
;; Even though we would expect A/A to produce 1, the actual result
;; is different and has a higher tolerance than initial starting point


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A
;; (99.9 100.1)
;; (percent-interval A)
;; 0.09999999999999432
;; (width-interval A)
;; 0.09999999999999432

;; B
;; (49.9 50.1)
;; (percent-interval B)
;; 0.20000000000000284
;; (width-interval B)
;; 0.10000000000000142


;; (def A-div-B (div-interval A B))
;; A-div-B
;; (1.9940119760479043 2.006012024048096)

;; (center-interval A-div-B)
;; 2.000012000048

;; (percent-interval A-div-B)
;; 0.2999994000011958

;; Div (99.9 100.1) by (49.9 50.1)
;; is executed as 
;; Mul (99.9 100.1) with (0.01996007984031936 0.02004008016032064)
;; the tolerances got added

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A
;; (99.9 100.1)
;; (percent-interval A)
;; 0.09999999999999432
;; (width-interval A)
;; 0.09999999999999432

;; (def A-add-A (add-interval A A))
;; A-add-A
;; (199.8 200.2)
;; (center-interval A-add-A)
;; 200.0
;; (percent-interval A-add-A)
;; 0.09999999999999432
;; (width-interval A-add-A)
;; 0.19999999999998863


;; (def A-add-A-sub-A (sub-interval A-add-A A))
;; A-add-A-sub-A
;; (99.70000000000002 100.29999999999998)
;; (center-interval A-add-A-sub-A)
;; 100.0
;; (percent-interval A-add-A-sub-A)
;; 0.29999999999998295
;; (width-interval A-add-A-sub-A)
;; 0.29999999999998295

;; We added A to A and subtracted A from that result.
;; The end result has higher width and tolerance than A


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (A+B) * (A-B) is algebraically equivalent to A^2 - B^2

;; (A+B) * (A-B) --> 150 * 50 --> 7500
;; A^2 - B^2 --> 100^2 + 50^2 --> 10000 - 2500 = 7500

;; A
;; (99.9 100.1)
;; (percent-interval A)
;; 0.09999999999999432
;; (width-interval A)
;; 0.09999999999999432

;; B
;; (49.9 50.1)
;; (percent-interval B)
;; 0.20000000000000284
;; (width-interval B)
;; 0.10000000000000142

;; (def f1 (mul-interval (add-interval A B)
;;                       (sub-interval A B)))
;; (def f2 (sub-interval (mul-interval A A)
;;                       (mul-interval B B)))
;; f1
;; (7460.040000000001 7540.039999999999)
;; f2
;; (7470.000000000002 7529.999999999998)
;; (center-interval f1)
;; 7500.04
;; (center-interval f2)
;; 7500.0
;; (percent-interval f1)
;; 0.5333304889040471
;; (percent-interval f2)
;; 0.39999999999997576
;; (width-interval f1)
;; 39.99999999999909
;; (width-interval f2)
;; 29.99999999999818

;; (def A-add-B (add-interval A B))
;; A-add-B
;; (149.8 150.2)
;; (center-interval A-add-B)
;; 150.0
;; (percent-interval A-add-B)
;; 0.13333333333332575
;; (width-interval A-add-B)
;; 0.19999999999998863

;; (def A-sub-B (sub-interval A B))
;; A-sub-B
;; (49.800000000000004 50.199999999999996)
;; (center-interval A-sub-B)
;; 50.0
;; (percent-interval A-sub-B)
;; 0.3999999999999915
;; (width-interval A-sub-B)
;; 0.19999999999999574

;; (def A-mul-A (mul-interval A A))
;; A-mul-A
;; (9980.010000000002 10020.009999999998)
;; (center-interval A-mul-A)
;; 10000.01
;; (percent-interval A-mul-A)
;; 0.1999998000001818
;; (width-interval A-mul-A)
;; 19.99999999999818


;; (def B-mul-B (mul-interval B B))
;; B-mul-B
;; (2490.0099999999998 2510.01)
;; (center-interval B-mul-B)
;; 2500.01
;; (percent-interval B-mul-B)
;; 0.399998400006409
;; (width-interval B-mul-B)
;; 10.000000000000227



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Testing with (* C C) and (+ C C C), where C is 3

;; (def C (make-center-percent 3 0.1))

;; (def C-sq (mul-interval C C))
;; C-sq
;; (8.982009 9.018009000000001)
;; (center-interval C-sq)
;; 9.000009
;; (percent-interval C-sq)
;; 0.19999980000020756
;; (width-interval C-sq)
;; 0.018000000000000682

;; (def CaddCaddC (add-interval C (add-interval C C)))
;; CaddCaddC
;; (8.991 9.009)
;; (center-interval CaddCaddC)
;; 9.0
;; (percent-interval CaddCaddC)
;; 0.1000000000000038
;; (width-interval CaddCaddC)
;; 0.009000000000000341








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.15
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Eva Lu Ator is right. Each operation involving an interval ALWAYS increases the
;; tolerance/uncertainty. With par2, even though it involves more operations,
;; 1 is reprsented as (1 . 1) with no uncertainty at all and that will keep the
;; tolerances tighter.






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.16
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Answering the why? part

;; (1) Equivlent algebraic expressions may involve different number of arithmetic operations
;; Each arithmetic operation adds to uncertainty.

;; (2) Addition/Subtraction creates a different level of uncertainty
;; from multiplication/division. Equivalent aLgebraic expressions may be written in terms
;; of more additions/subtractions and less multiplications/divisions and vice-versa. So with
;; the end result, the uncertainty is different depending on how we arrived there.


;; Not attempting the second part. Lots of research papers and discuasion out there regarding
;; interval arithmetic
;; See the bexcellent article 'A Lucid Interval' by Brian Hayes on Scientific American.

