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



(def make-from-real-imag)
(def make-from-mag-ang)

(def real-part)
(def imag-part)

(def magnitude)
(def angle)


(defn add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defn sub-comlpex [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))


(defn mul-complex [z1 z2]
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(defn div-complex [z1 z2]
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


;;;;; Ben's Representation ;;;;;

(defn square [x] (* x x))

(defn make-from-real-imag [real imag]
  (list real imag))

(def real-part first)
(def imag-part second)

(defn magnitude [z]
  (Math/sqrt (+ (square (real-part z))
                (square (imag-part z)))))

(defn angle [z]
  (Math/atan2 (imag-part z) (real-part z)))

(defn make-from-mag-ang [mag ang]
  (cons (* mag (Math/cos ang)) (* mag (/Math/sin ang))))
