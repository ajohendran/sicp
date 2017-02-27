(ns sicp.ch2.s2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  List Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defn list-ref [items n]
;;   (if (= n 0)
;;     (first items)
;;     (recur (next items) (dec n))))


;; NOTE: It is better to empty? instead of nil?
;; when cdr-ing down lists because in clojure
;; () is not same is nil

;; sicp.ch2.s2> (empty? (list))
;; true
;; sicp.ch2.s2> (empty? nil)
;; true
;; sicp.ch2.s2> (nil? (list))
;; false


(defn list-ref [lst n]
  (cond
    (empty? lst) nil
    (zero? n) (first lst)
    :else (recur (next lst) (dec n))))


;; (def squares (list 1 4 9 16 25))
;; #'sicp.ch2.s2/squares
;; (list-ref squares 3)
;; 16


(defn length [items]
  (if (empty? items)
    0
    (inc (length (next items)))))

;; (length squares)
;; 5

(defn length
  ([items]
   (length items 0))
  ([items n]
   (if (empty? items)
    n
    (recur (next items) (inc n)))))

(def odds (list 1 3 5 7))

;; (length odds)
;; 4

(defn append [l1 l2]
  (if (empty? l1)
    l2
    (cons (first l1) (append (next l1) l2))))

;; (append squares odds)
;; (1 4 9 16 25 1 3 5 7)



;; following iterative definition is wrong. It reverses l1
;; (defn append [l1 l2]
;;   (if (empty? l1)
;;     l2
;;     (recur (next l1) (cons (first l1) l2))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.17
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn last-pair [items]
  (if (empty? (next items))
    items
    (recur (next items))))

;; (last-pair (list 23 72 149 34))
;; (34)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.18
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; note: reverse is a keyword in clojure
(defn reverser
  ([items]
   (reverser items (list)))
  ([items ans]
   (if (empty? items)
     ans
     (recur (next items) (cons (first items) ans)))))

(reverser odds)
(7 5 3 1)


;; A crazy recursive definition obtained from internet
;; (defn reverser2 [items]
;;   (if (empty? items)
;;     (list)
;;     (append (reverser2 (rest items))
;;             (list (first items)))))

;; sicp.ch2.s2> (reverser2 odds)
;; TRACE t10785: (sicp.ch2.s2/reverser2 (1 3 5 7))
;; TRACE t10786: | (sicp.ch2.s2/reverser2 (3 5 7))
;; TRACE t10787: | | (sicp.ch2.s2/reverser2 (5 7))
;; TRACE t10788: | | | (sicp.ch2.s2/reverser2 (7))
;; TRACE t10789: | | | | (sicp.ch2.s2/reverser2 ())
;; TRACE t10789: | | | | => ()
;; TRACE t10790: | | | | (sicp.ch2.s2/append () (7))
;; TRACE t10790: | | | | => (7)
;; TRACE t10788: | | | => (7)
;; TRACE t10791: | | | (sicp.ch2.s2/append (7) (5))
;; TRACE t10792: | | | | (sicp.ch2.s2/append nil (5))
;; TRACE t10792: | | | | => (5)
;; TRACE t10791: | | | => (7 5)
;; TRACE t10787: | | => (7 5)
;; TRACE t10793: | | (sicp.ch2.s2/append (7 5) (3))
;; TRACE t10794: | | | (sicp.ch2.s2/append (5) (3))
;; TRACE t10795: | | | | (sicp.ch2.s2/append nil (3))
;; TRACE t10795: | | | | => (3)
;; TRACE t10794: | | | => (5 3)
;; TRACE t10793: | | => (7 5 3)
;; TRACE t10786: | => (7 5 3)
;; TRACE t10796: | (sicp.ch2.s2/append (7 5 3) (1))
;; TRACE t10797: | | (sicp.ch2.s2/append (5 3) (1))
;; TRACE t10798: | | | (sicp.ch2.s2/append (3) (1))
;; TRACE t10799: | | | | (sicp.ch2.s2/append nil (1))
;; TRACE t10799: | | | | => (1)
;; TRACE t10798: | | | => (3 1)
;; TRACE t10797: | | => (5 3 1)
;; TRACE t10796: | => (7 5 3 1)
;; TRACE t10785: => (7 5 3 1)
;; (7 5 3 1)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.19
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(defn no-more? [coins]
  (empty? coins))

(defn except-first-denomination [coins]
  (next coins))

(defn first-denomination [coins]
  (first coins))

(defn single-denomination? [coins]
  (empty? (rest coins)))


(defn cc [amount coin-values]
  (cond  (or (< amount 0) (no-more? coin-values)) 0
         (= amount 0) 1
        :else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values))))
;; sicp.ch2.s2> (cc 100 us-coins)
;; 292
;; sicp.ch2.s2> (cc 100 uk-coins)
;; 104561


;; Order doesn't affect the answer. It takes longer with ascending order but
;; it still goes through all the coins





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.20
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn filter-pred [predicate items]
  (cond
    (empty? items) (list)
    (predicate (first items)) (cons (first items)
                                    (filter-pred predicate (next items)))
    :else (filter-pred predicate (next items))))

(defn same-parity [elem & params]
  (cond (empty? params) (list elem)
        (odd? elem) (cons elem (filter-pred odd? params))
        :else (cons elem (filter-pred even? params))))


;; (same-parity 1 2 3 4 5 6 7)
;; (1 3 5 7)

;; (same-parity 2 3 4 5 6 7)
;; (2 4 6)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (square-list items)
  (if (null? items)
      nil
      (cons <??> <??>)))

(define (square-list items)
  (map <??> <??>))

(defn square [n] (* n n))

(defn square-list [items]
  (if (empty? items)
    (list)
    (cons (square (first items)) (square-list (next items)))))

(defn square-list [items]
  (map square items))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.22
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square-list [items]
  (loop [things items
         answer (list)]
    (if (empty? things)
      answer
      (recur (next things)
             (cons (square (first things)) answer)))))

;; a) recursive definition preserves the order because the first cons executed will
;; be of the square of last element and then square second last element and so on
;; recursive definition defers the cpnsing operation till end of list is reached
;; iterative definition conses immediately with the first encountered element and
;; then the second element and so on. Since cons adds to the first of the list, the
;; result is reversed with iterative method

(defn make-pair [elm1 elm2]
  (vector elm1 elm2)) ;; list will be explained in next section


(defn square-list [items]
  (loop [things items
         answer (list)]
    (if (empty? things)
      answer
      (recur (next things)
             (make-pair answer (square (first things)))))))
;; Creates a nested dotted pair structure because we are continually consing
;; lists to an element



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.23
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn for-each [proc items]
  (defn continue [items]
    (proc (first items))
    (for-each proc (next items)))
  (if (empty? items)
    nil
    (continue items)))

(defn for-each [proc items]
  (defn continue [items]
    (proc (first items))
    (for-each proc (next items)))
  (if (not (empty? items))
    (continue items)))

(defn for-each [proc items]
  (if (not (empty? items))
    (let []
      (proc (first items))
      (for-each proc (next items)))))


(for-each (fn [x] (newline) (print x))
          (list 57 321 88))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
