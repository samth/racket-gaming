#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            ;;
;;     == CANVAS 2.0 ==       ;;
;;  Gemaakt door Sam Vervaeck ;;
;;                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "constants.rkt"
         "graphics.rkt")

(provide make-image
         red
         green
         blue
         fill-rectangle!
         fill-ellipse!
         draw-line!
         on-key!
         start-game-loop
         (rename-out [current-milliseconds current-time])
         make-color
         draw-text!
         put-pixel!)

;; Algemene definities

(define canvas (make-graphics))

(extract canvas [keyboard keyboard])

;; Kleuren en stijlen

(define* 
  [font (make-font
          #:size FONT-SIZE
          #:face FONT-FACE
          #:family FONT-FAMILY
          #:style FONT-STYLE
          #:weight FONT-WEIGHT
          #:underlined? FONT-UNDERLINED
          #:smoothing FONT-SMOOTHING
          #:size-in-pixels? FONT-SIZE-IN-PIXELS)]
  [brush (new brush%
           [style BRUSH-STYLE]
           [stipple BRUSH-STIPPLE]
           [gradient BRUSH-GRADIENT]
           [transformation BRUSH-TRANSFORMATION])]
  [pen (new pen%
         [width PEN-WIDTH]
         [style PEN-STYLE]
         [cap PEN-CAP]
         [join PEN-JOIN]
         [stipple PEN-STIPPLE])])

(define*
  [some-brush (new brush%)]
  [some-pen (new brush%)])

(struct color (native brush pen)) ; onthoud de brush en pen van een bepaalde kleur

(define (make-color r g b [alpha 1.0])
  (make-object color% (* r 16) (* g 16) (* b 16) alpha))

(define (use-brush! color) ; voor vormen die gevuld moeten worden
  (set-brush canvas some-brush)
  (send brush set-color color)
  (use-brush canvas brush))

(define (use-pen! color) ; voor vormen die enkel uit lijnen bestaan
  (set-pen canvas some-pen)
  (send pen set-color color)
  (use-pen canvas pen))

(define (use-font-color! color) ; alleen nodig voor tekst
  (set-text-foreground canvas color))

(set-font canvas font)

(define*
  [red (make-color 15 0 0)]
  [green (make-color 0 15 0)]
  [blue (make-color 0 0 15)])

;; Afbeeldingen

(define make-image (make-constructor bitmap%))

;; Afgeleide tekenoperates

(define (put-pixel! x y color)
  (use-pen! color)
  (draw-point canvas x y))

(define (draw-line! x1 y1 x2 y2 color)
  (use-pen! color)
  (draw-line canvas x1 y1 x2 y2))

(define (fill-rectangle! x y width height color)
  (use-brush! color)
  (draw-rectangle canvas x y width height))

(define (fill-ellipse! x y width height color)
  (use-brush! color)
  (draw-ellipse canvas x y width height))

(define (draw-text! x y text color)
  (use-font-color! color)
  (use-document canvas (thunk (draw-text canvas text x (- (get-height canvas) y)))))

(define (draw-image! x y image)
  (use-document canvas (thunk (draw-bitmap canvas image x (- (get-height canvas) y)))))

(define (test)
  (draw-image! 100 100 (make-image "examples/stars.jpg"))
  (update-graphics canvas))

;; Event listening

(define (on-key! code proc)
  (chain keyboard (get-key code) press (add! proc)))

;; Game loop en tijd

; deze wordt opnieuw geimplementeerd voor hogere performantie
(define (start-game-loop thunk)
  (let loop ()
    (yield)
    (thunk)
    (update-graphics canvas)
    (queue-callback loop #t)))