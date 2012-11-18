#lang racket/base

;-------------------------
; Coordinates in racket  ;
;-------------------------

(require "shared.rkt")

(define (make-grid x-spacing y-spacing)
  
  (define axis-pen (make-pen #:color "black" #:style 'dot))
  (define plot-pen (make-pen #:color "green"))
  (define coordinate-font (make-font #:size 8))
  (define spacing (2d 50 50))
  
  (define (draw-coordinate graphics x y)
    (set-font graphics coordinate-font)
    (draw-text graphics (format "(~a,~a)" x y) (+ x 2) (+ y 2)))
  
  (define (draw graphics)
    (use-pen graphics axis-pen)
    (let
        ((width (get-width graphics))
         (height (get-height graphics)))
      (do ((line (/ width (x spacing)) (- line 1)))
        ((= line 0))
        (draw-line graphics (* (x spacing) line) 0 (* (x spacing) line) height)
        (draw-coordinate graphics (* (x spacing) line) 0))
      (do ((line (/ height (x spacing)) (- line 1)))
        ((= line 0))
        (draw-line graphics 0 (* (y spacing) line) width (* (y spacing) line))
        (draw-coordinate graphics 0 (* (y spacing) line)))))
  
  (define (plot graphics f range-start range-end)
    (define scale (/ (get-width graphics) (- range-end range-start)))
    (use-pen graphics plot-pen)
    (do ((x range-start (+ x scale)))
      ((> x range-end))
      (console "Writing ~a with scale ~a" x (round scale))
      (draw-point graphics (- x range-start) (f x))))
  
  (dispatch (grid) draw plot))

;; Drawing some shapes

(define*
  [test-image (make-object bitmap% "../resources/smiley.jpg")]
  [test-brush (make-brush #:color "blue")])

(define (test-shapes)
  (use-brush graphics test-brush)
  (draw-rectangle graphics 100 100 50 50)
  (draw-ellipse graphics 100 500 50 100)
  (draw-ellipse graphics 300 100 100 50)
  (draw-text graphics "Hello!" 200 300)
  (draw-bitmap graphics test-image 200 200))

;; Plotting functions

(define (test-grid)
  (send-message grid (plot graphics (lambda (x) (expt x 2)) -10 10))
  (send-message grid (plot graphics (lambda (x) (+ (* 2 x) 1)) -20 5))
  (send-message grid (plot graphics (lambda (x) (log x)) 1 25)))

;; Using our new coordinate system

(define grid (make-grid 5 5))
(define graphics (make-graphics))

(set-document graphics)

(define (refresh)
  (test-shapes)
  (test-grid)
  (send-message grid (draw graphics))
  (update-graphics graphics))

(chain* (chain graphics events)
  (paint (add! refresh))
  (resize (add! (lambda (width height) refresh))))