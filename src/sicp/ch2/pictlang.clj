
(ns sicp.ch2.pictlang
  (:require [quil.core :as q]))


;; The exercises will be done out of order to make sure the basic elements
;; needed are in place first

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Basic proceduees needed to keep this file self contained
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn foldl [proc res sequence]
  (loop [ans res
         s sequence]
    (if (empty? s)
      ans
      (recur (proc ans (first s))
             (next s)))))

(defn foldr [op init s]
  (if (empty? s)
    init
    (op (first s) (foldr op init (next s)))))

(defn for-each [proc sequence]
  (foldl (fn [res elem] (proc elem)) nil sequence))

(defn compose [f g]
  (fn [arg]
    (f (g arg))))

(defn repeated [f n]
  (cond (= 0 n) identity
        (= 1 n) f
        :else (compose f (repeated f (dec n)))))

(defn append [l1 l2]
  (foldr (fn [e res] (cons e  res)) l2 l1))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.46 Vector Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-vect [x y]
  (list x y))

(defn xcor-vect [v]
  (first v))

(defn ycor-vect [v]
  (second v))

(defn add-vect [v1 v2]
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(defn sub-vect [v1 v2]
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(defn scale-vect [s v]
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(def zero-vector (make-vect 0 0))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.47
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ignoring the second constructor in exercise problem statement!
;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))

(defn make-frame [origin edge1 edge2]
  (list origin edge1 edge2))

(defn origin-frame [frame]
  (first frame))

(defn edge1-frame [frame]
  (second frame))

(defn edge2-frame [frame]
  (second (next frame)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Frame Coordinate Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v)
                                    (edge1-frame frame))
                        (scale-vect (ycor-vect v)
                                    (edge2-frame frame))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.48
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-segment [start-vector end-vector]
  (list start-vector end-vector))

(defn start-segment [seg]
  (first seg))

(defn end-segment [seg]
  (second seg))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Painting Primitives - Using Quil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn quil-draw-lines [coordinates-list]
  (for-each
     (fn [coordinates]
       (q/line (first coordinates)
               (first (next coordinates))))
     coordinates-list))

(defn quil-paint [quil-draw-method]
  (q/sketch
       :size [500 500]
       :draw (fn []
               (q/with-translation [0.0 (q/height)]
                 (q/background 255)
                 (q/scale 1.0 -1.0)
                 (quil-draw-method)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Helper Functions for Painters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def frame-sq-500 (make-frame (make-vect 0.0 0.0)
                             (make-vect 500.0 0.0)
                             (make-vect 0.0 500.0)))

;; coordinate represented by ((x1 y1) (x2 y2))
(defn make-line-coordinates [segment-list frame-mapper]
  (map
     (fn [segment]
       (let [start-vect (frame-mapper (start-segment segment))
             end-vect (frame-mapper (end-segment segment))]
         (list (list (xcor-vect start-vect)
                     (ycor-vect start-vect)) 
               (list (xcor-vect end-vect)
                     (ycor-vect end-vect)))))
     segment-list))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Painters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn simulate-line [start end]
  (println "Drawing Line -> Start:" start " End:" end))

(defn segments->simulator [segment-list]
  (fn [frame]
    (let [frame-mapper (frame-coord-map frame)
          draw-line (fn [start end]
                      (println "Drawing Line -> Start:" start " End:" end))]
      (for-each (fn [segment]
                  (simulate-line (frame-mapper (start-segment segment))
                                 (frame-mapper (end-segment segment))))
                segment-list))))


;; There is a problem with the painter concept/notion as such
;; Drawing lines and so on requires setting up a canvas/window for graphics
;; Each call to a painter call would setup a new canvas
;; Transformation of painters (using 'beside' for example)
;; means having multiple painters with each of them
;; drawing lines -- all the lines drawn by each painter have to be on
;; the same canvas
;; To ensure all the lines/images end up on same canvas there has to be set up
;; code before the 'complex' painter is called
;; Another option is to  set a global canvas up.
;; With a global canvas though, when and how do we reset it?
;; The mit scheme code circumvents this problem by
;; having a 'paint' method that takes a painter, takes care of
;; graphics setup/teardown and
;; calls the painter with a standard frame.
;; Rotation, etc, is handled not by providing a different frame but by
;; transforming the painter.

(defn paint-with-frame [painter frame]
  (quil-paint (fn [] (painter frame))))

(defn paint [painter]
  (paint-with-frame painter frame-sq-500))

(defn segments->painter [segment-list]
  (fn [frame]
    (quil-draw-lines
     (make-line-coordinates segment-list (frame-coord-map frame)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Ex 2.49
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; a ;;;;
(def segs-outline (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
                        (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
                        (make-segment (make-vect 1.0 1.0) (make-vect 0.0 1.0))
                        (make-segment (make-vect 0.0 1.0) (make-vect 0.0 0.0))))

(def painter-outline (segments->painter segs-outline))

;;;; b ;;;;
(def segs-x (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
                  (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0))))

(def painter-x (segments->painter segs-x))

;;;; c ;;;;
(def segs-diamond (list (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
                        (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
                        (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
                        (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))))

(def painter-diamond (segments->painter segs-diamond))

;;;; d ;;;;
(def segs-wave )

(def painter-wave (segments->painter segs-wave))

;;;; outline & pole ;;;;
;; Using this painter for testing and playing around
(def segs-pole (list (make-segment (make-vect 0.5 0.2) (make-vect 0.5 0.8))
                     (make-segment (make-vect 0.5 0.8) (make-vect 0.6 0.6))
                     (make-segment (make-vect 0.35 0.2) (make-vect 0.65 0.2))))

(def segs-pole-outline (append segs-pole segs-outline))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Transforming and Combining Painters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter (make-frame new-origin
                           (sub-vect (m corner1) new-origin)
                           (sub-vect (m corner2) new-origin))))))


(defn rotate90 [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;; Following produce same output
;; 1) ((rotate90 (segments->painter segs-pole-outline)) frame-sq-10)
;; 2) ((segments->painter segs-pole-outline) frame-ccw-90)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Test Data for Painters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def seg1 (make-segment (make-vect 0.2 0.2)
                        (make-vect 0.5 0.5)))

(def seg2 (make-segment (make-vect 0.3 0.3)
                        (make-vect 0.75 0.75)))

(def frame-sq-100 (make-frame (make-vect 0.0 0.0)
                             (make-vect 100.0 0.0)
                             (make-vect 0.0 100.0)))

(def frame-sq-200 (make-frame (make-vect 0.0 0.0)
                             (make-vect 200.0 0.0)
                             (make-vect 0.0 200.0)))

(def frame-rect-100-200 (make-frame (make-vect 0.0 0.0)
                                  (make-vect 100.0 0.0)
                                  (make-vect 0.0 200.0)))

;; ((segments->painter (list seg1 seg2)) frame-sq-10)
;; ((segments->painter (list seg1 seg2)) frame-sq-20)
;; ((segments->painter (list seg1 seg2)) frame-rect-10-20)


(def frame-ccw-90 (make-frame (make-vect 500.0 0.0)
                              (make-vect 0.0 500.0)
                              (make-vect -500.0 0.0)))


(def frame-rect-10-5 (make-frame (make-vect 60.0 0.0)
                                 (make-vect 40.0 30.0)
                                 (make-vect -60.0 80.0)))

(def frame-dmnd-x5y0 (make-frame (make-vect 250.0 0.0)
                                 (make-vect 250.0 250.0)
                                 (make-vect -250.0 250.0)))

(def frame-dmnd-x0y5 (make-frame (make-vect 0.0 250.0)
                                 (make-vect 250.0 -250.0)
                                 (make-vect 250.0 250.0)))

(def frame-dmnd-x10y5 (make-frame (make-vect 500.0 250.0)
                                  (make-vect -250.0 -250.0)
                                  (make-vect -250.0 250.0)))

(def frame-x10y5 (make-frame (make-vect 500.0 250.0)
                             (make-vect 0.0 -250.0)
                             (make-vect -250.0 250.0)))

(def frame-dmnd-x5y10 (make-frame (make-vect 250.0 500.0)
                                  (make-vect -250.0 -250.0)
                                  (make-vect 250.0 -250.0)))


;;((segments->painter segs-pole-outline) frame-sq-100)

