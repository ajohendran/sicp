
(ns sicp.ch2.s5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Systems
;;;;  with Generric Operations - Generic Arithmetic Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: There are some variations from the book in how the following procedure
;; are defined
;; (1) All types are defined in lists for put-proc and get-proc, even when just
;;     one type.This helps with consistency - code doesn't change if dispatch is
;;     on one or more types
;; (2) Selectors are defined for each data type even if not defined explicitly
;;     in the book


;;;;;;;;;;;;;;;  Type tag plumbing ;;;;;;;;;;;;;;;

(defn attach-tag [type-tag contents]
  (list type-tag contents))

(defn type-tag [datum]
  (if (seq? datum)
    (first datum)
    (throw (ex-info (str "Bad tagged datum -- TYPE-TAG " datum) {}))))

(defn contents [datum]
  (if (seq? datum)
    (second datum)
    (throw (ex-info (str "Bad tagged datum -- TYPE-TAG " datum) {}))))


;;;;;;;;;;;;;;; Generic Ops plumbing ;;;;;;;;;;;;;;;

;; The book doesn't provde code to create maps at this stage.
;; Not sure what the value is in writing code without being able
;; to execute and poke around
;; So using clojure atom and persistence map to implement generic methods

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
        (ex-info
         (str "No method for these types -- APPLY-GENERIC " (list op args)) {})))))


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
(install-primitive-number-package)

;; constructor
(defn make-primitive [x]
  ((get-proc 'make '(primitive)) x))

;; selector
(defn get-value [x]
  (apply-generic 'value x))

;; (def pv1 (make-primitive 5))
;; ;; => #'sicp.ch2.s5/pv1
;; (get-value pv1)
;; ;; => 5
;; (def pv2 (make-primitive 6))
;; ;; => #'sicp.ch2.s5/pv2
;; (get-value pv2)
;; ;; => 6
;; (add pv1 pv2)
;; ;; => (primitive 11)
;; (mul pv1 pv2)
;; ;; => (primitive 30)


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

(install-rational-number-package)

;; constructor
(defn make-rational [x y]
  ((get-proc 'make '(rational)) x y))

;; selectors
(defn numer [r]
  (apply-generic 'numer r))
(defn denom [r]
  (apply-generic 'denom r))

;; (def rn1 (make-rational 2 4))
;; ;; => #'sicp.ch2.s5/r1
;; (numer rn1)
;; ;; => 1
;; (denom rn1)
;; ;; => 2
;; (def rn2 (make-rational 1 4))
;; ;; => #'sicp.ch2.s5/r2
;; (numer rn2)
;; ;; => 1
;; (denom rn2)
;; ;; => 4
;; (add rn1 rn2)
;; ;; => (rational (3 4))
;; (mul rn1 rn2)
;; ;; => (rational (1 8))


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

(install-polar-package)

;; constructors
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

(install-rectangular-package)

;; constructors
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

(install-complex-number-package)

;; constructors
(defn make-complex-from-real-imag [real imag]
  ((get-proc 'make-complex-from-real-imag '(complex)) real imag))

(defn make-complex-from-mag-ang [mag ang]
  ((get-proc 'make-complex-from-mag-ang '(complex)) mag ang))

;; selectors
(defn real-part [c]
  (apply-generic 'real-part c))

(defn imag-part [c]
  (apply-generic 'imag-part c))

(defn magnitude [c]
  (apply-generic 'magnitude c))

(defn angle [c]
 (apply-generic 'angle c))


;; (def c1 (make-complex-from-mag-ang 14.142135623730951 0.7853981633974483))
;; ;; => #'sicp.ch2.s5/c1
;; (real-part c1)
;; ;; => 10.000000000000002
;; (imag-part c1)
;; ;; => 10.0
;; (magnitude c1)
;; ;; => 14.142135623730951
;; (angle c1)
;; ;; => 0.7853981633974483

;; (def c2 (make-complex-from-real-imag 10 10))
;; ;; => #'sicp.ch2.s5/c2
;; (real-part c2)
;; ;; => 10
;; (imag-part c2)
;; ;; => 10
;; (magnitude c2)
;; ;; => 14.142135623730951
;; (angle c2)
;; ;; => 0.7853981633974483

;; (add c1 c2)
;; ;; => (complex (rectangular (20.0 20.0)))
;; (mul c1 c2)
;; ;; => (complex (polar (200.00000000000003 1.5707963267948966)))
;; (* 14.142135623730951 14.142135623730951)
;; ;; => 200.00000000000003
;; (* 2  0.7853981633974483)
;; ;; => 1.5707963267948966





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.77
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def z (make-complex-from-real-imag 3 4))

;; => #'sicp.ch2.s5/z

;; z
;; => (complex (rectangular (3 4)))

;; (magnitude z)
;; => 5.0

;;;;;;;  TRACE (magnitude z)  ;;;;;;;
;; (magnitude z)
;; (magnitude (complex (rectangular (3 4))))
;; (apply-generic 'magnitude (complex (rectangular (3 4))))
;; (apply-generic 'magnitude (rectangular (3 4)))
;; (Math/sqrt (+ (square (real-part (3 4)))
;;               (square (imag-part (3 4)))))
;; (Math/sqrt (+ (square 3)
;;               (square 4)))
;; (Math/sqrt (+ 9 16))
;; (math/sqrt 25)
;; 5

;; (use 'clojure.pprint)
;; (pprint (get @op-type-table 'magnitude))
;; {(polar)
;;  #function[sicp.ch2.s5/install-polar-package/magnitude--8337],
;;  (rectangular)
;;  #function[sicp.ch2.s5/install-rectangular-package/magnitude--8383],
;;  (complex)
;;  #function[sicp.ch2.s5/install-complex-number-package/magnitude--8567]}


;; apply-generic is invoked twice
;; In op-type-table, for operation 'magnitude', 3 type-procedure combo are defined
;; One procedure for type 'complex', one for type 'rectangular' and one for 'polar'
;; First the procedure defined for type 'complex' is dispatched.
;; This procedure is defined in complex number package
;; That in turn calls apply-generic for operation 'magnitude' on type 'rectangular'
;; So the procedure defined for operation 'magnitude' and type 'retangular' is
;; dispatched next. The latter is defined in 'rectangular' package

;; Not sure how to answer the question in book -- 'Describe in detail why this works.'
;; I just started defining selectors for every type along with the package definitions
;; before encountering this question.
;; Curiously, the book doesn't define any selectors. It asks to add code to the
;; complex number package but doesn't say anything about defining selectors such as
;; real-part, magnitude and so on.





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.78
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; attach-tag needn't be modified

(defn attach-tag [type-tag contents]
  (if (= type-tag 'primitive)
    contents
    (list type-tag contents)))

(defn type-tag [datum]
  (cond
    (seq? datum) (first datum)
    (number? datum) 'primitive
    :else (throw (ex-info (str "Bad tagged datum -- TYPE-TAG " datum) {}))))

(defn contents [datum]
  (cond
    (seq? datum) (second datum)
    (number? datum) datum
    :else (throw (ex-info (str "Bad tagged datum -- TYPE-TAG " datum) {}))))


;; (attach-tag 'primitive 5)
;; ;; => 5

;; (type-tag 5)
;; ;; => primitive

;; (contents 6)
;; ;; => 6

;; (add 5 6)
;; ;; => 11

;; (mul 7 8)
;; ;; => 56





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.79 && 2.80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Note: Evaluating the solutions still depends on functions described above
;;        Meaning, solutions provided below are not self-contained.
;;        Defining new packages for brevity's sake. Ideally, all methods go
;;        in single package
;; Eg.: Defintions for make-rational, make-complex and so on are used from above

(defn equ? [arg1 arg2]
  (apply-generic 'equ? arg1 arg2))

(defn =zero? [arg]
  (apply-generic '=zero? arg))


(put-proc 'equ? '(primitive primitive) (fn [n1 n2] (= n1 n2)))
;; (equ? 4 5)
;; ;; => false
;; (equ? 5 5)
;; ;; => true

;;;;;; The following will not work! 
;;;;;; (assuming numer and denom are generic procedures) ;;;;;

;; (put-proc 'equ? '(rational rational)
;;           (fn [r1 r2] (and (= (numer r1) (numer r2))
;;                            (= (denom r1) (denom r2)))))
;; (def r1 (make-rational 1 2))
;; (def r2 (make-rational 3 4))
;; (equ? r1 r2)

;; So I understand now why the book does not define general selectors outside of the
;; package install methods --
;; the outer type tag is stripped with the first generic method dispatch!
;; So, by the time the generic numer procedure is called,
;; the argument will be (1 2) instead of (rational (1 2))
;; The 'rational' tag was stripped when equ? was called
;; Not sure this tagging of data elements with type is such a great idea --
;; seems brittle
;; We cannot chain dispatches unless we account for successive removal of type tags

;; Operations for types such as polar and rectagular will also have to be
;; defined wthin a 'package'.

(defn install-primitive-number-package []
  ;; interface to generic system
  (letfn [(tag [x] (attach-tag 'primitive x))]
      (put-proc 'add '(primitive primitive) (fn [x y] (tag (+ x y))))
      (put-proc 'sub '(primitive primitive) (fn [x y] (tag (- x y))))
      (put-proc 'mul '(primitive primitive) (fn [x y] (tag (* x y))))
      (put-proc 'div '(primitive primitive) (fn [x y] (tag (/ x y))))
      (put-proc 'make '(primitive) (fn [x] (tag x)))
      (put-proc 'value '(primitive) identity)
      (put-proc 'equ? '(primitive primitive) (fn [n1 n2] (= n1 n2)))
      (put-proc '=zero? '(primitive) (fn [n] (= n 0))))
'installed-primitive-package)
  
(install-primitive-number-package)

;; (equ? 5 6)
;; ;; => false

;; (equ? 7 7)
;; ;; => true

;; (=zero? 0)
;; ;; => true

;; (=zero? 4)
;; ;; => false

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
    (put-proc 'make '(rational) (fn [x y] (tag (make-rat x y))))
    (put-proc 'equ? '(rational rational)
              (fn [r1 r2] (and (= (numer r1) (numer r2))
                               (= (denom r1) (denom r2)))))
    (put-proc '=zero? '(rational)
              (fn [r] (= (numer r) 0))))
  'installed-rational-number-package)

(install-rational-number-package)

;; Can also define a short version
;; (defn install-rational-number-package-2 []
;;   (letfn [;; Unable to use generic procedures already-defined because of
;;           ;; type stripping. So defining relevant helper functions again
;;           (numer [r] (first r))
;;           (denom [r] (second r))]
;;     (put-proc 'equ? '(rational rational)
;;               (fn [r1 r2] (and
;;                            (= (numer r1) (numer r2))
;;                            (= (denom r1) (denom r2)))))
;;     (put-proc '=zero? '(rational)
;;               (fn [r] (= (numer r) 0))))
;;   'installed-rational-number-package-2)

;; (equ? (make-rational 1 2) (make-rational 1 4))
;; ;; => false

;; (equ? (make-rational 1 2) (make-rational 1 2))
;; ;; => true

;; (equ? (make-rational 4 2) (make-rational 8 4))
;; ;; => true

;; (=zero? (make-rational 4 5))
;; ;; => false

;; (=zero? (make-rational 0 5))
;; ;; => true


;; NOTE: Using == to check number equality
;; Also, easier to add it to base package instead of redefining procedures like
;; for rational number 2 package
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
              (fn [mag ang] (tag (make-complex-mag-ang mag ang))))
    (put-proc 'equ? '(complex complex)
              (fn [z1 z2] (and (== (real-part z1) (real-part z2)) 
                               (== (imag-part z1) (imag-part z2)))))
    (put-proc '=zero? '(complex)
              (fn [z] (== (magnitude z) 0))))
  'installed-complex-number-package)

(install-complex-number-package)

;; There is a very interesting problem of how to handle when mixed complex types
;; are provided as arguments to equ?
;; Best to handle by considering one representation as basis. For equality check,
;; will consider only real and imag parts
;; For zero check, need to check only the magnitude
;; Of course, can send the numbers off to specific package to compare
;; depending on type but tag stripping makes that approach complex. Also which
;; package will then be considered for mixed types? 

;; (equ? (make-complex-from-real-imag 4 5) (make-complex-from-real-imag 5 4))
;; ;; => false

;; (equ? (make-complex-from-real-imag 4 5) (make-complex-from-real-imag 4 5))
;; ;; => true

;; (equ? (make-complex-from-mag-ang 4 0.98765) (make-complex-from-mag-ang 5 0.34567))
;; ;; => false

;; (equ? (make-complex-from-mag-ang 4 0.98765) (make-complex-from-mag-ang 4 0.98765))
;; ;; => true

;; (equ? (make-complex-from-real-imag 10.000000000000004 9.999999999999996)
;;       (make-complex-from-mag-ang 14.142135623730951 0.785398163397448))
;; ;; => true

;; (equ? (make-complex-from-mag-ang 14.142135623730951 0.785398163397448)
;;       (make-complex-from-real-imag 10.000000000000004 9.999999999999996))
;; ;; => true

;; (=zero? (make-complex-from-mag-ang 14.142135623730951 0.785398163397448))
;; ;; => false

;; (=zero? (make-complex-from-mag-ang 14.142135623730951 0))
;; ;; => false

;; (=zero? (make-complex-from-mag-ang 0 0.785398163397448))
;; ;; => true

;; (=zero? (make-complex-from-real-imag 10.000000000000004 9.999999999999996))
;; ;; => false

;; (=zero? (make-complex-from-real-imag 10 0))
;; ;; => false

;; (=zero? (make-complex-from-real-imag 0 10))
;; ;; => false

;; (=zero? (make-complex-from-real-imag 0 0))
;; ;; => true





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Combining Data of Different Types -- Coercion & Hierarchies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def coercion-table (atom {}))

(defn put-coercion [from-type to-type proc]
  (swap! coercion-table assoc-in [from-type to-type] proc))

(defn get-coercion [from-type to-type]
  (get-in @coercion-table [from-type to-type]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.81
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; a. ;;;

;; If exp is called witah with two complex numbers as arguments, 
;; error will be thrown "No method for these types (exp complex complex)"


;;; b. ;;;

;; No, Louis is wrong -- apply-generic will work as is without coercing
;; arguments of same type


;;; c ;;;
(defn apply-generic [op & args]
  (let [tags (map type-tag args)
        proc (get-proc op tags)]
    (cond
      proc
      (apply proc (map contents args))
      (and (= (count tags) 2)
           (not= (first tags) (second tags))) ;; main part of answer
      (let [type1 (first tags)
            type2 (second tags)
            arg-val1 (first args)
            arg-val2 (second args)
            t1->t2 (get-coercion type1 type2)
            t2->t1 (get-coercion type2 type1)]
        (cond
          t1->t2 (apply-generic op (t1->t2 arg-val1) arg-val2)
          t2->t1 (apply-generic op arg-val1 (t2->t1 arg-val2))
          :else  (throw (ex-info
                         (str "No method for these types -- APPLY-GENERIC " (list op args)) {}))))
      :else
      (throw (ex-info
              (str "No method for these types -- APPLY-GENERIC " (list op args)) {})))))


;; (add 5 (make-rational 4 5))
;; ;; Unhandled java.lang.Exception
;; ;; No method for these types -- APPLY-GENERIC (add 5 (rational (4 5)))
(defn primitive-number->rational-number [n]
  (make-rational n 1))

(put-coercion 'primitive 'rational primitive-number->rational-number)


;; (add 5 (make-rational 4 5))
;; ;; => (rational (29 5))

;; (add (make-rational 4 5) 5)
;; ;; => (rational (29 5))


(defn exp [x y] (apply-generic 'exp x y))

;; cheating here a bit -- 
(put-proc 'exp '(primitive primitive)
          (fn [x y] (attach-tag 'primitive (int (Math/pow x y)))))

;; (exp 2 4)
;; ;; => 16

;; (exp (make-rational 1 2) (make-rational 3 4))
;; Execution error at sicp.ch2.s5/apply-generic (REPL:749).
;; No method for these types -- APPLY-GENERIC (exp (rational (1 2)) (rational (3 4)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.82
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add [arg1 arg2 & more]
  (let [two-arg-fn (fn [x y] (apply-generic 'add x y))]
    (reduce two-arg-fn (two-arg-fn arg1 arg2) more)))


(defn find-coercions-to [to-type from-types]
  (if (empty? from-types)
    '()
    (let [coercion-fn (if (= (first from-types) to-type)
                        identity
                        (get-coercion (first from-types) to-type))]
      (if coercion-fn
        (let [rest-of-results (find-coercions-to to-type (next from-types))]
          (if rest-of-results
            (cons coercion-fn rest-of-results)
            nil))
        nil))))

(defn find-possible-coercions [tags-1 tags-2]
  (if (empty? tags-2)
    nil
    (let [coercions (find-coercions-to (first tags-2)
                                       (concat tags-1 tags-2))]
      (if (not-empty coercions) 
        coercions
        (find-possible-coercions (concat tags-1 (list (first tags-2)))
                                 (next tags-2))))))

(defn apply-generic [op & args]
  (let [tags (map type-tag args)
        proc (get-proc op tags)]
    (cond
      proc
      (apply proc (map contents args))
      (> (count args) 1)
      (let [coercions (find-possible-coercions '() tags)]
        (if coercions
          (apply apply-generic (cons op (map (fn [coercion-fn arg] (coercion-fn arg))
                                             coercions
                                             args)))
          (throw (ex-info
                  (str "No method for these types -- APPLY-GENERIC " (list op args)) {}))))
      :else
      (throw (ex-info
              (str "No method for these types -- APPLY-GENERIC " (list op args)) {})))))


;; sicp.ch2.s5> (add (make-rational 1 2) 5 (make-rational 4 5) 6)
;; TRACE t7853: (sicp.ch2.s5/find-possible-coercions () (rational primitive))
;; TRACE t7854: | (sicp.ch2.s5/find-coercions-to rational (rational primitive))
;; TRACE t7855: | | (sicp.ch2.s5/find-coercions-to rational (primitive))
;; TRACE t7856: | | | (sicp.ch2.s5/find-coercions-to rational nil)
;; TRACE t7856: | | | => ()
;; TRACE t7855: | | => (#function[sicp.ch2.s5/primitive-number->rational-number])
;; TRACE t7854: | => (#function[clojure.core/identity] #function[sicp.ch2.s5/primitive-number->rational-number])
;; TRACE t7853: => (#function[clojure.core/identity] #function[sicp.ch2.s5/primitive-number->rational-number])
;; TRACE t7857: (sicp.ch2.s5/find-possible-coercions () (rational primitive))
;; TRACE t7858: | (sicp.ch2.s5/find-coercions-to rational (rational primitive))
;; TRACE t7859: | | (sicp.ch2.s5/find-coercions-to rational (primitive))
;; TRACE t7860: | | | (sicp.ch2.s5/find-coercions-to rational nil)
;; TRACE t7860: | | | => ()
;; TRACE t7859: | | => (#function[sicp.ch2.s5/primitive-number->rational-number])
;; TRACE t7858: | => (#function[clojure.core/identity] #function[sicp.ch2.s5/primitive-number->rational-number])
;; TRACE t7857: => (#function[clojure.core/identity] #function[sicp.ch2.s5/primitive-number->rational-number])
;; (rational (123 10))



;; sicp.ch2.s5> (find-possible-coercions '() '(primitive rational primitive rational))
;; TRACE t7863: (sicp.ch2.s5/find-possible-coercions () (primitive rational primitive rational))
;; TRACE t7864: | (sicp.ch2.s5/find-coercions-to primitive (primitive rational primitive rational))
;; TRACE t7865: | | (sicp.ch2.s5/find-coercions-to primitive (rational primitive rational))
;; TRACE t7865: | | => nil
;; TRACE t7864: | => nil
;; TRACE t7866: | (sicp.ch2.s5/find-possible-coercions (primitive) (rational primitive rational))
;; TRACE t7867: | | (sicp.ch2.s5/find-coercions-to rational (primitive rational primitive rational))
;; TRACE t7868: | | | (sicp.ch2.s5/find-coercions-to rational (rational primitive rational))
;; TRACE t7869: | | | | (sicp.ch2.s5/find-coercions-to rational (primitive rational))
;; TRACE t7870: | | | | | (sicp.ch2.s5/find-coercions-to rational (rational))
;; TRACE t7871: | | | | | | (sicp.ch2.s5/find-coercions-to rational nil)
;; TRACE t7871: | | | | | | => ()
;; TRACE t7870: | | | | | => (#function[clojure.core/identity])
;; TRACE t7869: | | | | => (#function[sicp.ch2.s5/primitive-number->rational-number] #function[clojure.core/identity])
;; TRACE t7868: | | | => (#function[clojure.core/identity] #function[sicp.ch2.s5/primitive-number->rational-number] #function[clojure.core/identity])
;; TRACE t7867: | | => (#function[sicp.ch2.s5/primitive-number->rational-number] #function[clojure.core/identity] #function[sicp.ch2.s5/primitive-number->rational-number] #function[clojure.core/identity])
;; TRACE t7866: | => (#function[sicp.ch2.s5/primitive-number->rational-number] #function[clojure.core/identity] #function[sicp.ch2.s5/primitive-number->rational-number] #function[clojure.core/identity])
;; TRACE t7863: => (#function[sicp.ch2.s5/primitive-number->rational-number] #function[clojure.core/identity] #function[sicp.ch2.s5/primitive-number->rational-number] #function[clojure.core/identity])
;; (#function[sicp.ch2.s5/primitive-number->rational-number]
;;  #function[clojure.core/identity]
;;  #function[sicp.ch2.s5/primitive-number->rational-number]
;;  #function[clojure.core/identity])


;; sicp.ch2.s5> (add (make-rational 1 2) 5 (make-complex-from-real-imag 4 5) 6)
;; TRACE t7874: (sicp.ch2.s5/find-possible-coercions () (rational primitive))
;; TRACE t7875: | (sicp.ch2.s5/find-coercions-to rational (rational primitive))
;; TRACE t7876: | | (sicp.ch2.s5/find-coercions-to rational (primitive))
;; TRACE t7877: | | | (sicp.ch2.s5/find-coercions-to rational nil)
;; TRACE t7877: | | | => ()
;; TRACE t7876: | | => (#function[sicp.ch2.s5/primitive-number->rational-number])
;; TRACE t7875: | => (#function[clojure.core/identity] #function[sicp.ch2.s5/primitive-number->rational-number])
;; TRACE t7874: => (#function[clojure.core/identity] #function[sicp.ch2.s5/primitive-number->rational-number])
;; TRACE t7878: (sicp.ch2.s5/find-possible-coercions () (rational complex))
;; TRACE t7879: | (sicp.ch2.s5/find-coercions-to rational (rational complex))
;; TRACE t7880: | | (sicp.ch2.s5/find-coercions-to rational (complex))
;; TRACE t7880: | | => nil
;; TRACE t7879: | => nil
;; TRACE t7881: | (sicp.ch2.s5/find-possible-coercions (rational) (complex))
;; TRACE t7882: | | (sicp.ch2.s5/find-coercions-to complex (rational complex))
;; TRACE t7882: | | => nil
;; TRACE t7883: | | (sicp.ch2.s5/find-possible-coercions (rational complex) nil)
;; TRACE t7883: | | => nil
;; TRACE t7881: | => nil
;; TRACE t7878: => nil
;; Execution error at sicp.ch2.s5/apply-generic (REPL:839).
;; No method for these types -- APPLY-GENERIC (add (rational (11 2)) (complex (rectangular (4 5))))

(defn primitive-number->complex-number [n]
  (make-complex-from-real-imag n 0))

(put-coercion 'primitive 'complex primitive-number->complex-number)

(defn rational-number->complex-number [r]
  (make-complex-from-real-imag r 0))

(put-coercion 'rational 'complex rational-number->complex-number)

;; (find-possible-coercions '()
;;                          '(primitive rational complex primitive rational complex))
;; ;; => (#function[sicp.ch2.s5/primitive-number->complex-number] #function[sicp.ch2.s5/rational-number->complex-number] #function[clojure.core/identity] #function[sicp.ch2.s5/primitive-number->complex-number] #function[sicp.ch2.s5/rational-number->complex-number] #function[clojure.core/identity])


;;;; A versoin that finds corecions only for unique set of types
(defn foldl [op init s]
  (loop [res init
         remaining s]
    (if (empty? remaining)
      res
      (recur (op res (first remaining))
             (next remaining)))))

;; hopelessly slow but it is OK.
;; We haven't yet dealt with comparing/ordering symbols
(defn get-member [pred lst]
  (cond (empty? lst) nil
        (pred (first lst)) (first lst)
        :else (get-member pred (next lst))))

(defn uniques [lst]
  (foldl (fn [res elem] (if (get-member (fn [entry] (= entry elem)) res)
                          res
                          (cons elem res)))
         '()
         lst))


(defn find-coercions-to [to-type from-types]
  (if (empty? from-types)
    '()
    (let [coercion-fn (if (= (first from-types) to-type)
                        identity
                        (get-coercion (first from-types) to-type))]
      (if coercion-fn
        (let [rest-of-results (find-coercions-to to-type (next from-types))]
          (if rest-of-results
            (cons (list (first from-types) coercion-fn) rest-of-results)
            nil))
        nil))))

(defn find-possible-coercions [tags-1 tags-2]
  (if (empty? tags-2)
    nil
    (let [coercions (find-coercions-to (first tags-2)
                                       (concat tags-1 tags-2))]
      (if (not-empty coercions) 
        coercions
        (find-possible-coercions (concat tags-1 (list (first tags-2)))
                                 (next tags-2))))))

(defn apply-generic [op & args]
  (let [tags (map type-tag args)
        proc (get-proc op tags)
        unique-tags (uniques tags)]
    (cond
      proc
      (apply proc (map contents args))
      (> (count unique-tags) 1)
      (let [coercions (find-possible-coercions '() unique-tags)
            get-coercion-for-type (fn [tag]
                                    (second (get-member
                                             (fn [entry]
                                               (= (first entry) tag))
                                             coercions)))]
        (if coercions
          (apply apply-generic
                 (cons op (map (fn [arg]
                                 ((get-coercion-for-type (type-tag arg)) arg))
                               args)))
          (throw (ex-info
                  (str "No method for these types -- APPLY-GENERIC " (list op args)) {}))))
      :else
      (throw (ex-info
                  (str "No method for these types -- APPLY-GENERIC " (list op args)) {})))))



;; (find-possible-coercions '()
;;                          (uniques '(primitive rational complex
;;                                               primitive rational complex)))
;; ;; => ((complex #function[clojure.core/identity]) (rational #function[sicp.ch2.s5/rational-number->complex-number]) (primitive #function[sicp.ch2.s5/primitive-number->complex-number]))


;; (add (make-rational 1 2) 5 (make-rational 4 5) 6)
;; ;; => (rational (123 10))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.83
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Errr ..... we haven't dealt with 'real' numbers yet.
;; Skipping that as it really doesn't add value here

(defn raise [arg]
  (apply-generic 'raise arg))


(defn install-primitive-number-package []
  ;; interface to generic system
  (letfn [(tag [x] (attach-tag 'primitive x))]
      (put-proc 'add '(primitive primitive) (fn [x y] (tag (+ x y))))
      (put-proc 'sub '(primitive primitive) (fn [x y] (tag (- x y))))
      (put-proc 'mul '(primitive primitive) (fn [x y] (tag (* x y))))
      (put-proc 'div '(primitive primitive) (fn [x y] (tag (/ x y))))
      (put-proc 'make '(primitive) (fn [x] (tag x)))
      (put-proc 'value '(primitive) identity)
      (put-proc 'equ? '(primitive primitive) (fn [n1 n2] (= n1 n2)))
      (put-proc '=zero? '(primitive) (fn [n] (= n 0)))
      (put-proc 'raise '(primitive) (fn [n] (make-rational n 1))))
'installed-primitive-package)


;; Once again, due to tag stripping, raise has to be defined within a package
;; for rational types
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
    (put-proc 'make '(rational) (fn [x y] (tag (make-rat x y))))
    (put-proc 'equ? '(rational rational)
              (fn [r1 r2] (and (= (numer r1) (numer r2))
                               (= (denom r1) (denom r2)))))
    (put-proc '=zero? '(rational)
              (fn [r] (= (numer r) 0)))
    (put-proc 'raise '(rational)
              (fn [r] (make-complex-from-real-imag
                       (tag (make-rat (numer r) (denom r))) 0))))
  'installed-rational-number-package)


;; (raise 5)
;; ;; => (rational (5 1))

;; (raise (raise 5))
;; ;; => (complex (rectangular ((rational (5 1)) 0)))

;; (raise (make-rational 5 8))
;; ;; => (complex (rectangular ((rational (5 8)) 0)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.84
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Exercise 2.84.  Using the raise operation of exercise 2.83, modify the 
;; apply-generic procedure so that it coerces its arguments to have the same 
;; type by the method of successive raising, as discussed in this section. You 
;; will need to devise a way to test which of two types is higher in the tower. 
;; Do this in a manner that is ``compatible'' with the rest of the system and 
;; will not lead to problems in adding new levels to the tower.

