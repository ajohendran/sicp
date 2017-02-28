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

;; note: reverse is a keyword in clojure, usinf revrs
(defn revrs
  ([items]
   (revrs items (list)))
  ([items ans]
   (if (empty? items)
     ans
     (recur (next items) (cons (first items) ans)))))

;; (revrs odds)
;; (7 5 3 1)


;; A crazy recursive definition obtained from internet
;; (defn revrs2 [items]
;;   (if (empty? items)
;;     (list)
;;     (append (revrs2 (rest items))
;;             (list (first items)))))

;; sicp.ch2.s2> (revrs2 odds)
;; TRACE t10785: (sicp.ch2.s2/revrs2 (1 3 5 7))
;; TRACE t10786: | (sicp.ch2.s2/revrs2 (3 5 7))
;; TRACE t10787: | | (sicp.ch2.s2/revrs2 (5 7))
;; TRACE t10788: | | | (sicp.ch2.s2/revrs2 (7))
;; TRACE t10789: | | | | (sicp.ch2.s2/revrs2 ())
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

;; (list 1 (list 2 (list 3 4)))
;; (1 (2 (3 4)))

;; skipping the box-pointer and tree diagrams




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.25
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def lst (list 1 3 (list 5 7) 9))

;; lst
;; (1 3 (5 7) 9)

;; (first (next (first (next (next lst)))))
;; 7

;; (def lst (list (list 7)))

;; lst
;; ((7))

;; (first (first lst))
;; 7



;; (def lst (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;; lst
;; (1 (2 (3 (4 (5 (6 7))))))

;; (first (next (first (next (first (next (first (next
;;                                                (first
;;                                                 (next
;;                                                  (first
;;                                                   (next lst))))))))))))
;; 7





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.26
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def x (list 1 2 3))
;; (def y (list 4 5 6))

;; (append x y)
;; (1 2 3 4 5 6)

;; (cons x y)
;; ((1 2 3) 4 5 6)

;; (list x y)
;; ((1 2 3) (4 5 6))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.27
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def x (list (list 1 2) (list 3 4)))

;; x
;; ((1 2) (3 4))

(defn revrs
  ([l]
   (revrs l (list)))
  ([l ans]
   (if (empty? l)
    ans
    (revrs (next l) (cons (first l) ans)))))

;; (revrs x)
;; ((3 4) (1 2))


(defn deep-revrs
  ([t]
   (deep-revrs t (list)))
  ([t ans]
   (cond
     (empty? t) ans
     (seq? (first t)) (deep-revrs (next t) (cons (deep-revrs (first t) (list)) ans))
     :else (deep-revrs (next t) (cons (first t) ans)))))


(defn deep-revrs
  ([t]
   (deep-revrs t (list)))
  ([t ans]
   (cond (nil? t) ans
         (seq? t) (deep-revrs (next t) (cons (deep-revrs (first t) (list)) ans))
         :else t)))


;; (deep-revrs x)
;; ((4 3) (2 1))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.28
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (def x (list 5 (list 1 2) (list 3 4) 6))

;; x
;; (5 (1 2) (3 4) 6)

;; may be more appropriate to name parameter tree instead of items
;; the param to procedure  will always be a tree
(defn fringe [tree]
  (cond (empty? tree) (list)
        (seq? (first tree)) (append (fringe (first tree)) (fringe (next tree)))
        :else (cons (first tree) (fringe (next tree)))))


;; following two versions were obtained from net -- worth studying/understanding

;; this one is similar to how count-leaves is constructed in the book
;; not sure how appropriate it is to call the param tree since
;; successive applications of first could result in param being a leaf
(defn fringe [elem]
  (cond (nil? elem) (list)
        (seq? elem) (append (fringe (first elem)) (fringe (next elem)))
        :else (list elem)))

 
;; this approach more is for when the result is added to seq supplied as param
;; compare this to procedure deep-revrs defined above for Ex 2.27
(defn fringe
  ([tree]
   (fringe tree (list)))
  ([tree res]
   (cond (nil? tree) res
         (seq? tree) (fringe (first tree) (fringe (next tree) res))
         :else (cons tree res))))

;; similiar to above, except cond checks for (first tree)
(defn fringe
  ([tree]
   (fringe tree (list)))
  ([tree res]
   (cond (empty? tree) res
         (seq? (first tree)) (fringe (first tree) (fringe (next tree) res))
         :else (cons (first tree) (fringe (next tree) res)))))


;; (fringe x)
;; (5 1 2 3 4 6)

;; (fringe (list x x))
;; (5 1 2 3 4 6 5 1 2 3 4 6)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Count Leaves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def x (cons (list 1 2) (list 3 4)))

;; x
;; ((1 2) 3 4)


;; There are 4 different ways to work down a tree,
;; 2 completely recursive and 2 partly iterative

;; param will always be a seq
;; cond test is mostly on first element
(defn count-leaves [tree]
  (cond (empty? tree) 0
        (seq? (first tree)) (+ (count-leaves (first tree))
                               (count-leaves (next tree)))
        :else (inc (count-leaves (next tree)))))


;; Method provided in the book
;; note: first test in cond uses nil? instead of seq?
;; because we are recursively taking first and will eventually
;; encounter a non-seq element
;; Again, not sure is tree is the most appropriate name for param here
(defn count-leaves [tree]
  (cond (nil? tree) 0
        (not (seq? tree)) 1
        :else (+ (count-leaves (first tree))
                 (count-leaves (next tree)))))


;; For partly iterative approach
;; comparable to deep-revrs , see Ex 2.27
(defn count-leaves
  ([tree]
   (count-leaves tree 0))
  ([tree ans]
   (cond (nil? tree) ans
         (seq? tree) (count-leaves (first tree) (count-leaves (next tree) ans))
         :else (inc ans))))



(defn count-leaves
  ([tree]
   (count-leaves tree 0))
  ([tree ans]
   (cond (empty? tree) ans
         (seq? (first tree)) (count-leaves (first tree) (count-leaves (next tree) ans))
         :else (count-leaves (next tree) (inc ans)))))


;; (length x)
;; 3
;; (count-leaves x)
;; 4

;; (length (list x x))
;; 2
;; (count-leaves (list x x))
;; 8




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.29
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))


;;;;;;; a ;;;;;;;

(def left-branch first)
(def right-branch second)

(def branch-length first)
(def branch-structure second)


;;;;;;; additional data abstraction procedures ::::::::
(defn mobile-structure? [structure]
  (seq? structure))


;;;;;;;; data for testing;;;;;;;

;; Unbalanced mobile structure
;; (def b3L (make-branch 1 8))
;; (def b3R (make-branch 7 6))
;; (def m3 (make-mobile b3L b3R))

;; (def b2L (make-branch 2 10))
;; (def b2R (make-branch 4 m3))
;; (def m2 (make-mobile b2L b2R))

;; (def b4L (make-branch 9 11))
;; (def b4R (make-branch 3 12))
;; (def m4 (make-mobile b4L b4R))

;; (def b1L (make-branch 3 m2))
;; (def b1R (make-branch 5 m4))
;; (def m1 (make-mobile b1L b1R))


;; balanced mobile structure
;; (def b7L (make-branch 2 5))
;; (def b7R (make-branch 10 1))
;; (def m7 (make-mobile b7L b7R))

;; (def b6L (make-branch 12 2))
;; (def b6R (make-branch 4 m7))
;; (def m6 (make-mobile b6L b6R))

;; (def b8L (make-branch 6 3))
;; (def b8R (make-branch 2 9))
;; (def m8 (make-mobile b8L b8R))

;; (def b5L (make-branch 3 m6))
;; (def b5R (make-branch 2 m8))
;; (def m5 (make-mobile b5L b5R))


;;;;;;; b ;;;;;;;

;; several ways to write this
;; Assuming the parameter to total-weight can only be a mobile

(defn total-weight [mobile]
  (let [left-structure (branch-structure (left-branch mobile))
        right-structure (branch-structure (right-branch mobile))]
    (+
     (if (mobile-structure? left-structure)
       (total-weight left-structure)
       left-structure)
     (if (mobile-structure? right-structure)
       (total-weight right-structure)
       right-structure))))


(defn total-weight [mobile]
  (let [struct-weight (fn [s] (if (mobile-structure? s)
                                (total-weight s)
                                s))]
    (+
     (struct-weight (branch-structure (left-branch mobile)))
     (struct-weight (branch-structure (right-branch mobile))))))



;; a coceptually better way to define this is with mutual recursion
;; each branch has a weight or mobile

(def weight-mobile) ;; instantiate variable

(defn weight-branch [b]
  (let [s (branch-structure b)]
    (if (mobile-structure? s)
      (weight-mobile s)
      s)))


(defn weight-mobile [m]
  (+ (weight-branch (left-branch m))
     (weight-branch (right-branch m))))

(defn total-weight [m]
  (weight-mobile m))


;;; or use letfn to define mutually recursive functions

(defn total-weight [m]
  (letfn [(weight-branch [b]
            (let [s (branch-structure b)]
              (if (mobile-structure? s)
                (weight-mobile s)
                s)))
          (weight-mobile [m]
            (+ (weight-branch (left-branch m))
               (weight-branch (right-branch m))))]
    (weight-mobile m)))


;; A simple definition - can take either branch and mobile as parameter

(defn total-weight [s]
  (if (mobile-structure? s)
    (+ (total-weight (branch-structure (left-branch s)))
       (total-weight (branch-structure (right-branch s))))
    s))


;;;;;;; c ;;;;;;;

(defn torque-branch [b]
  (* (weight-branch b) (branch-length b)))


;; mutually recursive version
(def balanced-mobile?)

(defn balanced-branch? [b]
  (let [s (branch-structure b)]
    (if (mobile-structure? s)
      (balanced-mobile? s)
      true)))

(defn balanced-mobile? [m]
  (and
   (= (torque-branch (left-branch m))
      (torque-branch (right-branch m)))
   (balanced-branch? (left-branch m))
   (balanced-branch? (right-branch m))))


;; Sweet short version. Takes a structure as parameter
(defn balanced? [s]
  (if (mobile-structure? s)
    (and
     (= (torque-branch (left-branch s))
        (torque-branch (right-branch s)))
     (balanced? (branch-structure (left-branch s)))
     (balanced? (branch-structure (right-branch s))))
    true))


;; The problem with above solutions is thet
;; they walk down the tree several times - once when computing torque
;; and then again when checking for balance of sub-mobile
;; See trace below

;; sicp.ch2.s2> (balanced? m5)
;; TRACE t11341: (sicp.ch2.s2/balanced? ((3 ((12 2) (4 ((2 5) (10 1))))) (2 ((6 3) (2 9)))))
;; TRACE t11342: | (sicp.ch2.s2/weight-branch (3 ((12 2) (4 ((2 5) (10 1))))))
;; TRACE t11343: | | (sicp.ch2.s2/weight-mobile ((12 2) (4 ((2 5) (10 1)))))
;; TRACE t11344: | | | (sicp.ch2.s2/weight-branch (12 2))
;; TRACE t11344: | | | => 2
;; TRACE t11345: | | | (sicp.ch2.s2/weight-branch (4 ((2 5) (10 1))))
;; TRACE t11346: | | | | (sicp.ch2.s2/weight-mobile ((2 5) (10 1)))
;; TRACE t11347: | | | | | (sicp.ch2.s2/weight-branch (2 5))
;; TRACE t11347: | | | | | => 5
;; TRACE t11348: | | | | | (sicp.ch2.s2/weight-branch (10 1))
;; TRACE t11348: | | | | | => 1
;; TRACE t11346: | | | | => 6
;; TRACE t11345: | | | => 6
;; TRACE t11343: | | => 8
;; TRACE t11342: | => 8
;; TRACE t11349: | (sicp.ch2.s2/weight-branch (2 ((6 3) (2 9))))
;; TRACE t11350: | | (sicp.ch2.s2/weight-mobile ((6 3) (2 9)))
;; TRACE t11351: | | | (sicp.ch2.s2/weight-branch (6 3))
;; TRACE t11351: | | | => 3
;; TRACE t11352: | | | (sicp.ch2.s2/weight-branch (2 9))
;; TRACE t11352: | | | => 9
;; TRACE t11350: | | => 12
;; TRACE t11349: | => 12
;; TRACE t11353: | (sicp.ch2.s2/balanced? ((12 2) (4 ((2 5) (10 1)))))
;; TRACE t11354: | | (sicp.ch2.s2/weight-branch (12 2))
;; TRACE t11354: | | => 2
;; TRACE t11355: | | (sicp.ch2.s2/weight-branch (4 ((2 5) (10 1))))
;; TRACE t11356: | | | (sicp.ch2.s2/weight-mobile ((2 5) (10 1)))
;; TRACE t11357: | | | | (sicp.ch2.s2/weight-branch (2 5))
;; TRACE t11357: | | | | => 5
;; TRACE t11358: | | | | (sicp.ch2.s2/weight-branch (10 1))
;; TRACE t11358: | | | | => 1
;; TRACE t11356: | | | => 6
;; TRACE t11355: | | => 6
;; TRACE t11359: | | (sicp.ch2.s2/balanced? 2)
;; TRACE t11359: | | => true
;; TRACE t11360: | | (sicp.ch2.s2/balanced? ((2 5) (10 1)))
;; TRACE t11361: | | | (sicp.ch2.s2/weight-branch (2 5))
;; TRACE t11361: | | | => 5
;; TRACE t11362: | | | (sicp.ch2.s2/weight-branch (10 1))
;; TRACE t11362: | | | => 1
;; TRACE t11363: | | | (sicp.ch2.s2/balanced? 5)
;; TRACE t11363: | | | => true
;; TRACE t11364: | | | (sicp.ch2.s2/balanced? 1)
;; TRACE t11364: | | | => true
;; TRACE t11360: | | => true
;; TRACE t11353: | => true
;; TRACE t11365: | (sicp.ch2.s2/balanced? ((6 3) (2 9)))
;; TRACE t11366: | | (sicp.ch2.s2/weight-branch (6 3))
;; TRACE t11366: | | => 3
;; TRACE t11367: | | (sicp.ch2.s2/weight-branch (2 9))
;; TRACE t11367: | | => 9
;; TRACE t11368: | | (sicp.ch2.s2/balanced? 3)
;; TRACE t11368: | | => true
;; TRACE t11369: | | (sicp.ch2.s2/balanced? 9)
;; TRACE t11369: | | => true
;; TRACE t11365: | => true
;; TRACE t11341: => true
;; true



;; It would be better if there was a way to just recurse down just once
;; and build up needed information

(defn make-tree-info [weight balance]
  (list weight balance))
(def weight-info first)
(def balance-info second)

(defn get-subtree-info [s]
  (if (mobile-structure? s)
    (let [lb (left-branch s)
          rb (right-branch s)
          lb-info (get-subtree-info (branch-structure lb))
          rb-info (get-subtree-info (branch-structure rb))
          lb-wt (weight-info lb-info)
          rb-wt (weight-info rb-info)
          lb-balance (balance-info lb-info)
          rb-balance (balance-info rb-info)]
      (make-tree-info
       (+ lb-wt rb-wt)
       (and (= (* lb-wt (branch-length lb)) (* rb-wt (branch-length rb)))
            lb-balance
            rb-balance)))
    (make-tree-info s true)))


(defn total-weight [m]
  (weight-info (get-subtree-info m)))

(defn balanced? [m]
  (balance-info (get-subtree-info m)))




;; sicp.ch2.s2> (balanced? m5)
;; TRACE t11615: (sicp.ch2.s2/balanced? ((3 ((12 2) (4 ((2 5) (10 1))))) (2 ((6 3) (2 9)))))
;; TRACE t11616: | (sicp.ch2.s2/get-subtree-info ((3 ((12 2) (4 ((2 5) (10 1))))) (2 ((6 3) (2 9)))))
;; TRACE t11617: | | (sicp.ch2.s2/get-subtree-info ((12 2) (4 ((2 5) (10 1)))))
;; TRACE t11618: | | | (sicp.ch2.s2/get-subtree-info 2)
;; TRACE t11618: | | | => (2 true)
;; TRACE t11619: | | | (sicp.ch2.s2/get-subtree-info ((2 5) (10 1)))
;; TRACE t11620: | | | | (sicp.ch2.s2/get-subtree-info 5)
;; TRACE t11620: | | | | => (5 true)
;; TRACE t11621: | | | | (sicp.ch2.s2/get-subtree-info 1)
;; TRACE t11621: | | | | => (1 true)
;; TRACE t11619: | | | => (6 true)
;; TRACE t11617: | | => (8 true)
;; TRACE t11622: | | (sicp.ch2.s2/get-subtree-info ((6 3) (2 9)))
;; TRACE t11623: | | | (sicp.ch2.s2/get-subtree-info 3)
;; TRACE t11623: | | | => (3 true)
;; TRACE t11624: | | | (sicp.ch2.s2/get-subtree-info 9)
;; TRACE t11624: | | | => (9 true)
;; TRACE t11622: | | => (12 true)
;; TRACE t11616: | => (20 true)
;; TRACE t11615: => true
;; true



;; sicp.ch2.s2> (total-weight m1)
;; TRACE t11627: (sicp.ch2.s2/total-weight ((3 ((2 10) (4 ((1 8) (7 6))))) (5 ((9 11) (3 12)))))
;; TRACE t11628: | (sicp.ch2.s2/get-subtree-info ((3 ((2 10) (4 ((1 8) (7 6))))) (5 ((9 11) (3 12)))))
;; TRACE t11629: | | (sicp.ch2.s2/get-subtree-info ((2 10) (4 ((1 8) (7 6)))))
;; TRACE t11630: | | | (sicp.ch2.s2/get-subtree-info 10)
;; TRACE t11630: | | | => (10 true)
;; TRACE t11631: | | | (sicp.ch2.s2/get-subtree-info ((1 8) (7 6)))
;; TRACE t11632: | | | | (sicp.ch2.s2/get-subtree-info 8)
;; TRACE t11632: | | | | => (8 true)
;; TRACE t11633: | | | | (sicp.ch2.s2/get-subtree-info 6)
;; TRACE t11633: | | | | => (6 true)
;; TRACE t11631: | | | => (14 false)
;; TRACE t11629: | | => (24 false)
;; TRACE t11634: | | (sicp.ch2.s2/get-subtree-info ((9 11) (3 12)))
;; TRACE t11635: | | | (sicp.ch2.s2/get-subtree-info 11)
;; TRACE t11635: | | | => (11 true)
;; TRACE t11636: | | | (sicp.ch2.s2/get-subtree-info 12)
;; TRACE t11636: | | | => (12 true)
;; TRACE t11634: | | => (23 false)
;; TRACE t11628: | => (47 false)
;; TRACE t11627: => 47
;; 47

;;;;;; d ;;;;;;;

;;Changing constructors would only mean, changing the corresponding selectors




