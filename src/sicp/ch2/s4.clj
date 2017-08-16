(ns sicp.ch2.s4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Multiple Representations for Abstract Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generic Procedures -- procedures that can operate on data that may be represented in more than one way.

;; Type Tags -- Main technique for building generic procedures will be to work in terms of data objects that have type tags, that is, data objects that include explicit information about how they are to be processed

;; Data-directed Programming -- a powerful and convenient implementation strategy for additively assembling systems with generic operations





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  2.4.1  Representations for Complex Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def make-from-real-imag)
;; (def make-from-mag-ang)

;; (def real-part)
;; (def imag-part)

;; (def magnitude)
;; (def angle)

;; (defn add-complex [z1 z2]
;;   (make-from-real-imag (+ (real-part z1) (real-part z2))
;;                        (+ (imag-part z1) (imag-part z2))))

;; (defn sub-comlpex [z1 z2]
;;   (make-from-real-imag (- (real-part z1) (real-part z2))
;;                        (- (imag-part z1) (imag-part z2))))

;; (defn mul-complex [z1 z2]
;;   (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;;                      (+ (angle z1) (angle z2))))

;; (defn div-complex [z1 z2]
;;   (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
;;                      (- (angle z1) (angle z2))))


;; ;;;;; Ben's Representation ;;;;;

;; (defn square [x] (* x x))

;; (def real-part first)

;; (def imag-part second)

;; (defn magnitude [z]
;;   (Math/sqrt (+ (square (real-part z))
;;                 (square (imag-part z)))))

;; (defn angle [z]
;;   (Math/atan2 (imag-part z) (real-part z)))

;; (defn make-from-real-imag [real imag]
;;   (list real imag))

;; (defn make-from-mag-ang [mag ang]
;;   (list (* mag (Math/cos ang)) (* mag (Math/sin ang))))


;; ;;;;; Alyssa's Representation ;;;;;

;; (defn real-part [z]
;;   (* (magnitude z)
;;      (Math/cos (angle z))))

;; (defn imag-part [z]
;;   (* (magnitude z)
;;      (Math/sin (angle z))))

;; (def magnitude first)

;; (def angle [z] second)

;; (defn make-from-real-imag [real imag]
;;   (list (Math/sqrt (+ (square (real-part z))
;;                       (square (imag-part z))))
;;         (Math/atan2 (imag-part z) (real-part z))))

;; (defn make-from-mag-ang [mag ang]
;;   (list mag ang))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  2.4.2 Tagged Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;; Ben's Representation ;;;;;

(defn square [x] (* x x))

(def real-part-rectangular first)

(def imag-part-rectangular second)

(defn magnitude-rectangular [z]
  (Math/sqrt (+ (square (real-part-rectangular z))
                (square (imag-part-rectangular z)))))

(defn angle-rectangular [z]
  (Math/atan2 (imag-part-rectangular z)
              (real-part-rectangular z)))

(defn make-from-real-imag-rectangular [real imag]
  (attach-tag 'rectangular (list real imag)))

(defn make-from-mag-ang-rectangular [mag ang]
  (attach-tag 'rectuangular
              (list (* mag (Math/cos ang))
                    (* mag (Math/sin ang)))))


;;;;; Alyssa's Representation ;;;;;

(def magnitude-polar first)

(def angle-polar second)

(defn real-part-polar [z]
  (* (magnitude-polar z)
     (Math/cos (angle-polar z))))

(defn imag-part-polar [z]
  (* (magnitude-polar z)
     (Math/sin (angle-polar z))))

(defn make-from-real-imag-polar [real imag]
  (attach-tag 'polar
              (list (Math/sqrt (+ (square real)
                                  (square imag)))
                    (Math/atan2 imag real))))

(defn make-from-mag-ang-polar [mag ang]
  (attach-tag 'polar (list mag ang)))


;;;;; Generic Procedures ;;;;;

(defn rectangular? [z]
  (= (type-tag z) 'rectangular))

(defn polar? [z]
  (= (type-tag z) 'polar))

(defn real-part [z]
  (cond (rectangular? z) (real-part-rectangular (contents z))
        (polar? z) (real-part-polar (contents z))
        :else (throw (Exception. (str "Unknown type -- REAL-PART " z)))))

(defn imag-part [z]
  (cond (rectangular? z) (imag-part-rectangular (contents z))
        (polar? z) (imag-part-polar (contents z))
        :else (throw (Exception. (str "Unknown type -- IMAG-PART " z)))))

(defn magnitude [z]
  (cond (rectangular? z) (magnitude-rectangular (contents z))
        (polar? z) (magnitude-polar (contents z))
        :else (throw (Exception. (str "Unknown type -- MAGNITUDE " z)))))

(defn angle [z]
  (cond (rectangular? z) (angle-rectangular (contents z))
        (polar? z) (angle-polar (contents z))
        :else (throw (Exception. (str "Unknown type -- ANGLE " z)))))


;;;;; Constructors ;;;;;

(defn make-from-real-imag [real imag]
  (make-from-real-imag-rectangular real imag))

(defn make-from-mag-ang [mag ang]
  (make-from-mag-ang-polar mag ang))


;; sicp.ch2.s4> (make-from-mag-ang 3 Math/PI)
;; (polar (3 3.141592653589793))
;; sicp.ch2.s4> (magnitude (make-from-mag-ang 3 Math/PI))
;; 3
;; sicp.ch2.s4> (angle (make-from-mag-ang 3 Math/PI))
;; 3.141592653589793
;; sicp.ch2.s4> (real-part (make-from-mag-ang 3 Math/PI))
;; -3.0
;; sicp.ch2.s4> (imag-part (make-from-mag-ang 3 Math/PI))
;; 3.6739403974420594E-16

;; sicp.ch2.s4> (make-from-real-imag (/ 1 (Math/sqrt 2)) (/ 1 (Math/sqrt 2)))
;; (rectangular (0.7071067811865475 0.7071067811865475))
;; sicp.ch2.s4> (real-part (make-from-real-imag (/ 1 (Math/sqrt 2)) (/ 1 (Math/sqrt 2))))
;; 0.7071067811865475
;; sicp.ch2.s4> (imag-part (make-from-real-imag (/ 1 (Math/sqrt 2)) (/ 1 (Math/sqrt 2))))
;; 0.7071067811865475
;; sicp.ch2.s4> (magnitude (make-from-real-imag (/ 1 (Math/sqrt 2)) (/ 1 (Math/sqrt 2))))
;; 0.9999999999999999
;; sicp.ch2.s4> (angle (make-from-real-imag (/ 1 (Math/sqrt 2)) (/ 1 (Math/sqrt 2))))
;; 0.7853981633974483





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  2.4.3 Data-Directed Programming and Additivity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defn square [x] (* x x))

(def op-type-table (atom {}))

(defn put-proc [op type proc]
  (swap! op-type-table assoc-in [op type] proc))

(defn get-proc [op type]
  (get-in @op-type-table [op type]))

(defn install-rectangular-package []
  (letfn [;; internal-procedures
          (real-part [z] (first z))
          (imag-part [z] (second z))
          (magnitude [z] (Math/sqrt (+ (square (real-part z))
                                       (square (imag-part z)))))
          (angle [z] (Math/atan2 (imag-part z) (real-part z)))
          (make-from-real-imag [real imag] (list real imag))
          (make-from-mag-ang [mag ang]
            (list (* mag (Math/cos ang)) (* mag (Math/sin ang))))
          (tag [x] (attach-tag 'rectangular x))]
    ;; interface to system
    (put-proc 'real-part 'rectangular real-part)
    (put-proc 'imag-part 'rectangular imag-part)
    (put-proc 'magnitude 'rectangular magnitude)
    (put-proc 'angle 'rectangular angle)
    (put-proc 'make-from-real-imag 'rectangular
              (fn [real imag] (tag (make-from-real-imag real imag))))
    (put-proc 'make-from-mag-ang 'rectangular
              (fn [mag ang] (tag (make-from-mag-ang mag ang))))))


(defn install-polar-package []
  (letfn [;; internal-procedures
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
         (put-proc 'real-part 'polar real-part)
  (put-proc 'imag-part 'polar imag-part)
  (put-proc 'magnitude 'polar magnitude)
  (put-proc 'angle 'polar angle)
  (put-proc 'make-from-real-imag 'polar
            (fn [real imag] (tag (make-from-real-imag real imag))))
  (put-proc 'make-from-mag-ang 'polar
            (fn [mag ang] (tag (make-from-mag-ang mag ang))))))

(defn apply-generic [op arg]
  (let [proc (get-proc op (type-tag arg))]
     (if proc
       (proc (contents arg))
       (throw
        (Exception.
         (str "No method for these types -- APPLY-GENERIC ") (list op arg))))))

(defn real-part [z] (apply-generic 'real-part z))
(defn imag-part [z] (apply-generic 'imag-part z))
(defn magnitude [z] (apply-generic 'magnitude z))
(defn angle [z] (apply-generic 'angle z))

(defn make-from-real-imag [real imag]
  ((get-proc 'make-from-real-imag 'rectangular) real imag))
(defn make-from-mag-ang [mag ang]
  ((get-proc 'make-from-mag-ang 'polar) mag ang))


;; sicp.ch2.s4> (def z1 (make-from-mag-ang 100 (/ Math/PI 4) ))
;; sicp.ch2.s4> (magnitude z1)
;; 100
;; sicp.ch2.s4> (angle z1)
;; 0.7853981633974483
;; sicp.ch2.s4> (real-part z1)
;; 70.71067811865476
;; sicp.ch2.s4> (imag-part z1)
;; 70.71067811865474
;; sicp.ch2.s4> 
;; sicp.ch2.s4> (def z2 (make-from-real-imag 8 6))
;; #'sicp.ch2.s4/z2
;; sicp.ch2.s4> 
;; sicp.ch2.s4> (magnitude z2)
;; 10.0
;; sicp.ch2.s4> (angle z2)
;; 0.6435011087932844
;; sicp.ch2.s4> (real-part z2)
;; 8
;; sicp.ch2.s4> (imag-part z2)
;; 6





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.73
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; a ;;;;;

;; deriv has been updated to use generic procedures. If a new operator type
;; is to be added added to the system, all we have to do is install
;; a new package wih the procedure to handle the derivation for new operator

;; number? and same-variable? can't be assimilated because they
;; do not have the 'operator & operand in list' structure used for generic dispatch

;; if operator and operand procedures are updated with the conditionals,
;; number? and variable? can be assimilated


;;;;; b & c ;;;;;


(def op-type-table (atom {}))

(defn put-proc [op type proc]
  (swap! op-type-table assoc-in [op type] proc))

(defn get-proc [op type]
  (get-in @op-type-table [op type]))


(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(def variable? symbol?)

(defn same-variable? [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))

(defn make-sum [a b]
  (cond (=number? a 0) b
        (=number? b 0) a
        (and (number? a) (number? b)) (+ a b)
        :else (list '+ a b)))

(defn make-difference [a b]
  (cond (=number? b 0) a
        (and (number? a) (number? b)) (- a b)
        :else (list '- a b)))

(defn make-product [a b]
  (cond (or (=number? a 0) (=number? b 0)) 0
        (=number? a 1) b
        (=number? b 1) a
        (and (number? a) (number? b)) (* a b)
        :else (list '* a b)))

(defn make-exponentiation [a b]
  (cond (=number? b 0) 1
        (=number? a 0) 0
        (=number? b 1) a
        (=number? a 1) 1
        (and (number? a) (number? b)) (Math/pow a b)
        :else (list '** a b)))

(defn operator [exp] (first exp))
(defn operands [exp] (next exp))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        :else ((get-proc 'deriv (operator exp)) (operands exp) var)))

(defn install-deriv-add-mult []
  (letfn [(deriv-sum [args var]
            (let [addend (first args)
                  augend (second args)]
              (make-sum (deriv addend var) (deriv augend var))))
          (deriv-product [args var]
            (let [multiplier (first args)
                  multiplicand (second args)]
              (make-sum
               (make-product multiplier (deriv multiplicand var))
               (make-product multiplicand (deriv multiplier var)))))]
    (put-proc 'deriv '+ deriv-sum)
    (put-proc 'deriv '* deriv-product)))

(defn install-deriv-exponentiation []
  (letfn [(deriv-exponentiation [args var]
            (let [base (first args)
                  exponent (second args)]
              (make-product
               (make-product exponent
                             (make-exponentiation base
                                                  (make-difference exponent 1)))
               (deriv base var))))]
    (put-proc 'deriv '** deriv-exponentiation)))

;; sicp.ch2.s4> (deriv '(** x y) 'x)
;; (* y (** x (- y 1)))
;; sicp.ch2.s4> (deriv '(+ (* (* (* 5 x) (** y x)) x) y) 'x)
;; (+ (* (* 5 x) (** y x)) (* x (* (** y x) 5)))


;;;;; d ;;;;;

;; All that needs to be changed is the order of elements in calls to put-proc.
;; For example: (put-proc 'deriv '+ deriv-sum) has to be changed to
;; (put-proc '+ 'deriv deriv-sum)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.74
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn attach-tag [type-tag contents]
  (list type-tag contents))

;; modified from book. Use 'NO-TYPE for when typw is not needed
(defn type-tag [datum]
  (if (seq? datum)
    (first datum)
    'NO-TYPE))

;; modified from book. If datum isn't a list, just return it
(defn contents [datum]
  (if (seq? datum)
    (second datum)
    datum))

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


;;;;;;;;;; a ;;;;;;;;;;

(defn get-record [name personnel-file]
  (apply-generic 'get-record name personnel-file))

;; Each personnel file should be tagged with corresponding division-id as tag
;; (attach-tag 'div-1045 personnel-file)
;; The division's accessors must be 'installed' to the operation-type table
;; For example, (put-proc 'get-record '(NO_TYPE division-1045) get-emp-record)


;;;;;;;;;; b ;;;;;;;;;;

(defn get-salary [employee-record]
  (apply-generic 'get-salary employee-record))

;; Every record in each personnel file must be tagged with division-id as tag

;; Since the division personnel file is tagged, we needn't really tag the individual
;; records if we can always assume we will keep the tag asspciated with the
;; personnel-file whenever we access a record from it and then something from the
;; record. Or the generic get salary must take the personnel file as second argument


;;;;;;;;;; c ;;;;;;;;;;

(defn find-employee-record [name personnel-files]
  (if (empty? personnel-files)
    (println "Employee not found -- " name)
    (let [rec (get-record name (first personnel-files))]
      (if (nil? rec)
        (find-employee-record name (next personnel-files))
        rec))))


;;;;;;;;;; d ;;;;;;;;;;

;; Every record in the file and the file itself must be tagged with division-id
;; All the accessor methods must be 'installed' in the generic procedure table





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Message Passing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-from-real-imag [x y]
  (let [dispatch (fn [op]
                   (cond (= op 'real-part) x
                         (= op 'imag-part) y
                         (= op 'magnitude) (Math/sqrt (+ (square x) (square y)))
                         (= op 'angle) (Math/atan2 y x)
                         :else (throw
                                (Exception.
                                 (str"Unknown op -- MAKE-FROM-REAL-IMAG " op)))))]
    dispatch))


(defn apply-generic [op arg]
  (arg op))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.75
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-from-mag-ang [r a]
  (let [dispatch (fn [op]
                   (cond (= op 'magnitude) r
                         (= op 'angle) a
                         (= op 'real-part) (* r (Math/cos a))
                         (= op 'imag-part) (* r (Math/sin a))
                         :else (throw
                                (Exception.
                                 (str"Unknown op -- MAKE-FROM-MAG-ANG " op)))))]
    dispatch))

;; sicp.ch2.s4> (def z2 (make-from-mag-ang 25 (/ Math/PI 4)))
;; #'sicp.ch2.s4/z2
;; sicp.ch2.s4> (z2 'magnitude)
;; 25
;; sicp.ch2.s4> (z2 'angle)
;; 0.7853981633974483
;; sicp.ch2.s4> (z2 'real-part)
;; 17.67766952966369
;; sicp.ch2.s4> (z2 'imag-part)
;; 17.677669529663685





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.76
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; explicit dispatch
;; => new types      -- all operation procedures must add a 
;;                      clause to the cond statement
;;                      for each type and include handlers 
;;                      specific to the various types
;; => new operations -- each new operation procedure should 
;;                      add cond clauses for every existing 
;;                      type with appropriate handler



;; data-directed style
;; => new types      -- Add handling procedures to the 
;;                      generic operations table for the 
;;                      specific combinations of types and 
;;                      operations
;;=> new operations  -- for every applicable type, add 
;;                      handler procedurees to the generics
;;                      tables and then create an "interface" 
;;                      procedure for the operation that in 
;;                      turn will call the generic-ops 
;;                      procedure with op-type hard coded


;; message-passing-style
;;=> new types       -- Create an "object" procedure for every 
;;                      type and add cond clauses with
;;                      handlers for every applicable type
;;=> new operations  -- Adjust every "object" procedure to add 
;;                      cond clause with handler for 
;;                      applicable types


;; Whether new types are added often or new opereations are
;; added more often, data driven method may be the best
;; approach because
;; (i) Avoidance of code duplication and minimize copy-paste 
;;     errors
;; (ii) Employing proper abstraction improves maintainability 
;;      and makes code easier to read
;; (iii) Additive -- Modules can be incorporated additvely
;;      without having to modify existing code repeatedly. 
;;      This helps with decpoupling/decentralization. 
;; (iv) Related to additivity -- there couple be hundres of
;;      types or hundreds of operations - no one person may
;;      be familiar with them all



;; Ultimately the choice of method/approach will rest on
;; (i) Are we dispatching on more than one type. If yes, 
;;     choose data driven style
;; (ii) Do the operations need access to shared state? If so
;;      message passing style will be needed


;; This is a very important question/issue and needs a lot of 
;; thought/understanding and ability to visualize.
;; Ideas behind Polymorphism, object oriented programming,
;; etc. arose from this and related questions. 

;; The answer to this question will likely be updated upon
;; further thought, experience and understanding.