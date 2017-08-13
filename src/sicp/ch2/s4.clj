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
  (if (list? datum)
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
