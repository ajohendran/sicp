
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Building Abstractions with Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Complex data is needed because we want to process several pieces of
;; simple data as a unit. This elevates the abstraction. Otherwise we will
;; have to individually keep track of the seperate pieces

;; Quoting from book

;; "Programs are typically designed to model complex phenomena,
;; and more often than not one must construct computational objects
;; that have several parts in order to model real-world phenomena
;; that have several aspects."

;; "Just as the ability to define procedures enables us to deal with processes
;; at a higher conceptual level than that of the primitive operations of the
;; language, the ability to construct compound data objects enables us to deal
;; with data at a higher conceptual level than that of the primitive data
;; objects of the language."



;; The authors provide the example of rational numbers. But think of a person
;; and all the attributes that define a person. Name, age, sex, nationality ,
;; etc. We need to be able to have a concept of a 'person' that includes these
;; attributes, that is, be able to group these attributes and give the
;; grouping a name. We can then write programs in terms of this grouping


;; Quoting again
;; "The general technique of isolating the parts of a program that deal with
;; how data objects are represented from the parts of a program that deal
;; with how data objects are used is a powerful design methodology called
;; data abstraction."

;;(define (linear-combination a b x y)     
;;  (add (mul a x) (mul b y)))
;; Here the the procedure doesn't have to know what  a,b,x and y
;; are and how they are constructed. Only things it needs to know is that add
;; and mul will do the rigt thing.


;; Qoting again

;; "One key idea in dealing with compound data is the notion of closure --
;; that the glue we use for combining data objects should allow us to combine
;; not only primitive data objects, but compound data objects as well.
;; Another key idea is that compound data objects can serve as conventional
;; interfaces for combining program modules in mix-and-match ways."

;; "We will find that, just as a given numerical function can be computed
;; by many different computational processes, there are many ways in which
;; a given data structure can be represented in terms of simpler objects,
;; and the choice of representation can have significant impact on the time
;; and space requirements of processes that manipulate the data."

;; "Maintaining modularity in the presence of generic operations requires more
;; powerful abstraction barriers than can be erected with simple data 
;; abstraction alone. In particular, we introduce data-directed programming
;; as a technique that allows individual data representations to be designed
;; in isolation and then combined additively (i.e., without modification)"

