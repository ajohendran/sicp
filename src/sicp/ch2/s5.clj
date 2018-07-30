
(ns sicp.ch2.s5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Systems with Generric Operations - Generic Arithmetic Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def op-type-table (atom {}))

(defn put-proc [op type proc]
  (swap! op-type-table assoc-in [op type] proc))

(defn get-proc [op type]
  (get-in @op-type-table [op type]))



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


(defn install-primitive-number-package []
  ;; interface to generic system
  (put-proc 'add '(primitive primitive) +)
  (put-proc 'sub '(primitive primitive) -)
  (put-proc 'mult '(primitive primitive) *)
  (put-proc 'div '(primitive primitive) /)
  (put-proc 'make 'primitive (fn [x] (attach-tag 'primitive x)))
  'done)

(defn make-primitive [x]
  ((get-proc 'make 'primitive) x))

(defn install-rational-number-package []
  (letfn [;; internal procedures
          (make-rat [n d] (list n d))
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
    (put-proc 'add '(rational rational)
              (fn [x y] (tag (add-rat x y))))
    (put-proc 'sub '(rational rational)
              (fn [x y] (tag (sub-rat x y))))
    (put-proc 'mul '(rational rational)
              (fn [x y] (tag (mul-rat x y))))
    (put-proc 'div '(rational rational)
              (fn [x y] (tag (div-rat x y))))
    (put-proc 'make 'rational (fn [x y] (tag (make-rat x y)))))
  'done)

(defn make-rational [x y]
  ((get-proc 'make 'rational) x y))


(defn add [x y] ((get 'add (list (type-tag x) (type-tag y))) (contents x) (contents y)))
