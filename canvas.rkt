#lang racket/base

(require "constants.rkt"
         (rename-in "graphics.rkt" [make-color make-native-color]))

(provide make-image
         make-color

         black
         white
         red
         green
         blue

         put-pixel!
         draw-line!
         draw-image!
         draw-text!
         fill-rectangle!
         fill-ellipse!

         on-key!
         on-release!

         start-game-loop
         (rename-out [current-milliseconds current-time]))

;; Algemene definities

(define graphics (make-graphics))
(extract graphics [keyboard keyboard])

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
  [some-pen (new pen%)])

(struct color (native brush pen)) ; onthoud de brush en pen van een bepaalde kleur

(define (make-color r g b [alpha 1.0])
  (make-object color% (* r 16) (* g 16) (* b 16) alpha))

(define (use-brush! color) ; voor vormen die gevuld moeten worden
  (set-brush graphics some-brush)
  (send brush set-color color)
  (use-brush graphics brush))

(define (use-pen! color) ; voor vormen die enkel uit lijnen bestaan
  (set-pen graphics some-pen)
  (send pen set-color color)
  (use-pen graphics pen))

(define (use-font-color! color) ; alleen nodig voor tekst
  (set-text-foreground graphics color))

(set-font graphics font)
(define current-char-height (get-char-height graphics))

(define*
  [black (make-color 0 0 0)]
  [white (make-color 15 15 15)]
  [red (make-color 15 0 0)]
  [green (make-color 0 15 0)]
  [blue (make-color 0 0 15)])

;; Afbeeldingen

(define make-image (make-constructor bitmap%))

;; Afgeleide tekenoperates

(define (put-pixel! x y color)
  (use-pen! color)
  (draw-point graphics x y))

(define (draw-line! x1 y1 x2 y2 color)
  (use-pen! color)
  (draw-line graphics x1 y1 x2 y2))

(define (fill-rectangle! x y width height color)
  (use-brush! color)
  (draw-rectangle graphics x y width height))

(define (fill-ellipse! x y width height color)
  (use-brush! color)
  (draw-ellipse graphics (- x (/ width 2)) (- y (/ height 2)) width height))

(define (draw-text! x y text color)
  (use-font-color! color)
  (use-document graphics (thunk (draw-text graphics text x (- (get-height graphics) y current-char-height)))))

(define (draw-image! x y image)
  (use-document graphics (thunk (draw-bitmap graphics image x (- (get-height graphics) y)))))

;; Event listening

(define (on-key! code proc)
  (chain keyboard (get-key code) press (add! proc)))

(define (on-release! code proc)
  (chain keyboard (get-key code) release (add! proc)))

;; Game loop en tijd

; deze wordt opnieuw geimplementeerd voor hogere performantie
(define (start-game-loop thunk)
  (let loop ()
    (yield)
    (thunk)
    (update-graphics graphics)
    (queue-callback loop #t)))
