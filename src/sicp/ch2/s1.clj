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

;; (def point-a (make-point -2 1))
;; (def point-b (make-point -1 2))
;; (def seg-ab (make-segment point-a point-b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square [x] (* x x))

(defn x-length-segment [seg]
  (Math/abs (- (x-point (start-segment seg))
               (x-point (end-segment seg)))))

(defn y-length-segment [seg]
  (Math/abs (- (y-point (start-segment seg))
               (y-point (end-segment seg)))))

(defn length-segment [seg]
  (Math/sqrt (+ (square (x-length-segment seg))
                (square (y-length-segment seg)))))

;; First representation is as as two points, the digonal segment
(defn make-rectangle [diagonal-segment]
  )

(defn width-rectangle [r])

(defn height-rect [r])

(defn area-rectangle [r]
  (* (width-rectangle r) (height-rectangle r)))

(defn perimeter-rectangle [r]
  (+ (* 2 (width-rectangle r))
     (* 2 (height-rectangle r))))

