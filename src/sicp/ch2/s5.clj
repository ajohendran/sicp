
(ns sicp.ch2.s5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Systems with Generric Operations - Generic Arithmetic Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;  Tag plumbing ;;;;;;;;;;;;;;;

(defn attach-tag [type-tag contents]
  (list type-tag contents))

(defn type-tag [datum]
  (if (seq? datum)
    (first datum)
    (throw (Exception. (str "Bad tagged datum -- TYPE-TAG " datum)))))

(defn contents [datum]
  (if (seq? datum)
    (second datum)
    (throw (Exception. (str "Bad tagged datum -- CONTENTS " datum)))))



;;;;;;;;;;;;;;; Generic Ops plumbing ;;;;;;;;;;;;;;;

(def op-type-table (atom {}))

(defn put-proc [op type proc]
  (swap! op-type-table assoc-in [op type] proc))

(defn get-proc [op type]
  (get-in @op-type-table [op type]))

(defn apply-generic [op & args]
  (let [tags (map type-tag args) 
        proc (get-proc op tags)]
     (if proc
       (apply proc (map contents args))
       (throw
        (Exception.
         (str "No method for these types -- APPLY-GENERIC ") (list op args))))))



;;;;;;;;;;;;;;;  Generic arithmetic procedures ;;;;;;;;;;;;;;;

(defn add [arg1 arg2]
  (apply-generic 'add arg1 arg2))

(defn sub [arg1 arg2]
  (apply-generic 'sub arg1 arg2))

(defn mul [arg1 arg2]
  (apply-generic 'mul arg1 arg2))

(defn div [arg1 arg2]
  (apply-generic 'div arg1 arg2))



;;;;;;;;;;;;;;; Primitive integers package ;;;;;;;;;;;;;;;

(defn install-primitive-number-package []
  ;; interface to generic system
  (letfn [(tag [x] (attach-tag 'primitive x))]
      (put-proc 'add '(primitive primitive) (fn [x y] (tag (+ x y))))
      (put-proc 'sub '(primitive primitive) (fn [x y] (tag (- x y))))
      (put-proc 'mul '(primitive primitive) (fn [x y] (tag (* x y))))
      (put-proc 'div '(primitive primitive) (fn [x y] (tag (/ x y))))
      (put-proc 'make '(primitive) (fn [x] (tag x)))
      (put-proc 'value '(primitive) identity))
  'installed-primitive-package)

(defn make-primitive [x]
  ((get-proc 'make '(primitive)) x))
(defn get-value [x]
  (apply-generic 'value x))



;;;;;;;;;;;;;;; Rational number package ;;;;;;;;;;;;;;;

(defn install-rational-number-package []
  (letfn [;; internal procedures
          (gcd [a b]
            (if (= b 0)
              a
              (gcd b (rem a b))))
          (make-rat [n d]
            (let [g (gcd n d)]
              (list (/ n g) (/ d g ))))
          (numer [r] (first r))
          (denom [r] (second r))
          (add-rat [x y]
            (make-rat (+ (* (numer x) (denom y))
                         (* (numer y) (denom x)))
                      (* (denom x) (denom y))))
          (sub-rat [x y]
            (make-rat (- (* (numer x) (denom y))
                         (* (numer y) (denom x)))
                      (* (denom x) (denom y))))
          (mul-rat [x y]
            (make-rat (* (numer x) (numer y))
                      (* (denom x) (denom y))))
          (div-rat [x y]
            (make-rat (* (numer x) (denom y))
                      (* (denom x) (numer y))))
          (tag [x] (attach-tag 'rational x))]
    (put-proc 'numer '(rational) (fn [r] (numer r)))
    (put-proc 'denom '(rational) (fn [r] (denom r)))
    (put-proc 'add '(rational rational)
              (fn [x y] (tag (add-rat x y))))
    (put-proc 'sub '(rational rational)
              (fn [x y] (tag (sub-rat x y))))
    (put-proc 'mul '(rational rational)
              (fn [x y] (tag (mul-rat x y))))
    (put-proc 'div '(rational rational)
              (fn [x y] (tag (div-rat x y))))
    (put-proc 'make '(rational) (fn [x y] (tag (make-rat x y)))))
  'installed-rational-number-package)

(defn make-rational [x y]
  ((get-proc 'make '(rational)) x y))
(defn numer [r]
  (apply-generic 'numer r))
(defn denom [r]
  (apply-generic 'denom r))



;;;;;;;;;;;;;;; Complex number package ;;;;;;;;;;;;;;;

(defn install-polar-package []
  (letfn [;; internal procedures
          (square [x] (* x x))
          (magnitude [z] (first z))
          (angle [z] (second z))
          (real-part [z]
            (*
             (magnitude z)
             (Math/cos (angle z))))
          (imag-part [z]
            (* (magnitude z)
               (Math/sin (angle z))))
          (make-from-real-imag [real imag]
            (list (Math/sqrt (+ (square real)
                                (square imag)))
                  (Math/atan2 imag real)))
          (make-from-mag-ang [mag ang]
            (list mag ang))
          (tag [x] (attach-tag 'polar x))]
    ;; interface to system
    (put-proc 'real-part '(polar) real-part)
    (put-proc 'imag-part '(polar) imag-part)
    (put-proc 'magnitude '(polar) magnitude)
    (put-proc 'angle '(polar) angle)
    (put-proc 'make-from-real-imag '(polar)
              (fn [real imag] (tag (make-from-real-imag real imag))))
    (put-proc 'make-from-mag-ang '(polar)
              (fn [mag ang] (tag (make-from-mag-ang mag ang)))))
  'installed-polar-package)

(defn make-polar-from-real-imag [real imag]
  ((get-proc 'make-from-real-imag '(polar)) real imag))
(defn make-polar-from-mag-ang [mag ang]
  ((get-proc 'make-from-mag-ang '(polar)) mag ang))


(defn install-rectangular-package []
  (letfn [;; internal procedures
          (square [x] (* x x)) 
          (real-part [z] (first z))
          (imag-part [z] (second z))
          (magnitude [z] (Math/sqrt (+ (square (real-part z))
                                       (square (imag-part z)))))
          (angle [z] (Math/atan2 (imag-part z) (real-part z)))
          (make-from-real-imag [real imag]
            (list real imag))
          (make-from-mag-ang [mag ang]
            (list
             (* mag (Math/cos ang))
             (* mag (Math/sin ang))))
          (tag [z] (attach-tag 'rectangular z))]
    ;; interface to system
    (put-proc 'real-part '(rectangular) real-part)
    (put-proc 'imag-part '(rectangular) imag-part)
    (put-proc 'magnitude '(rectangular) magnitude)
    (put-proc 'angle '(rectangular) angle)
    (put-proc 'make-from-real-imag '(rectangular)
              (fn [real imag] (tag (make-from-real-imag real imag))))
    (put-proc 'make-from-mag-ang '(rectangular)
              (fn [mag ang] (tag (make-from-mag-ang mag ang)))))
  'installed-rectangular-package)

(defn make-rectangular-from-real-imag [real imag]
  ((get-proc 'make-from-real-imag '(rectangular)) real imag))
(defn make-rectangular-from-mag-ang [mag ang]
  ((get-proc 'make-from-mag-ang '(rectangular)) mag ang))


(defn install-complex-number-package []
  (letfn [;; internal procedures
          (make-complex-real-imag [real imag]
            ((get-proc 'make-from-real-imag '(rectangular)) real imag))
          (make-complex-mag-ang [mag ang]
            ((get-proc 'make-from-mag-ang '(polar)) mag ang))
          (real-part [z] (apply-generic 'real-part z))
          (imag-part [z] (apply-generic 'imag-part z))
          (magnitude [z] (apply-generic 'magnitude z))
          (angle [z] (apply-generic 'angle z))
          (add [z1 z2]
            (make-complex-real-imag (+ (real-part z1) (real-part z2))
                                    (+ (imag-part z1) (imag-part z2))))
          (sub [z1 z2]
            (make-complex-real-imag (- (real-part z1) (real-part z2))
                                    (- (imag-part z1) (imag-part z2))))
          (mul [z1 z2]
            (make-complex-mag-ang (* (magnitude z1) (magnitude z2))
                                  (+ (angle z1) (angle z2))))
          (div [z1 z2]
            (make-complex-mag-ang (/ (magnitude z1) (magnitude z2))
                                  (- (angle z1) (angle z2))))
          (tag [z] (attach-tag 'complex z))]
    ;; interface to system
    (put-proc 'real-part '(complex) real-part)
    (put-proc 'imag-part '(complex) imag-part)
    (put-proc 'magnitude '(complex) magnitude)
    (put-proc 'angle '(complex) angle)
    (put-proc 'add '(complex complex)
              (fn [z1 z2] (tag (add z1 z2))))
    (put-proc 'sub '(complex complex)
              (fn [z1 z2] (tag (sub z1 z2))))
    (put-proc 'mul '(complex complex)
              (fn [z1 z2] (tag (mul z1 z2))))
    (put-proc 'div '(complex complex)
              (fn [z1 z2] (tag (div z1 z2))))
    (put-proc 'make-complex-from-real-imag '(complex)
              (fn [real imag] (tag (make-complex-real-imag real imag))))
    (put-proc 'make-complex-from-mag-ang '(complex)
              (fn [mag ang] (tag (make-complex-mag-ang mag ang)))))
  'installed-complex-number-package)

(defn make-complex-from-real-imag [real imag]
  ((get-proc 'make-complex-from-real-imag '(complex)) real imag))
(defn make-complex-from-mag-ang [mag ang]
  ((get-proc 'make-complex-from-mag-ang '(complex)) mag ang))
(defn real-part [c]
  (apply-generic 'real-part c))
(defn imag-part [c]
  (apply-generic 'imag-part c))
(defn magnitude [c]
  (apply-generic 'magnitude c))
(defn angle [c]
 (apply-generic 'angle c))
