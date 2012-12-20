#lang racket/base

; Canvas backport of the game library
; ===================================
;
; This file simulates the functionality of
; the first version of the graphics library.
; Above that, it adds some extra features, such as
; drawing images and providing more colors.

(require "constants.rkt"
         (rename-in "objective.rkt" [make-color make-native-color]))

(provide make-image
         make-color
         make-font

         image-width
         image-height
         
         get-canvas-width
         get-canvas-height
         
         find-color
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

         on-close!
         on-resize!
         off-close!
         off-resize!
         
         on-key!
         on-release!
         off-key!
         off-release!
         
         on-click!
         on-move!
         off-click!
         off-move!
        
         start-game-loop
         stop-game-loop
         (rename-out [current-milliseconds current-time]))

;; Generating a lot of generics

(define*
  [set-brush-color (generic brush% set-color)]
  [set-pen-color (generic pen% set-color)])

(define-generics*
  (game-canvas% get-width get-height update)
  (graphics% set-pen set-brush set-font set-text-foreground
             only-pen only-brush clear get-char-height
             draw-bitmap draw-ellipse draw-rectangle draw-text draw-point draw-line))

;; General definitions

(define game-loop 
  (new loop%
       [interval (/ 1000 CANVAS-FRAMERATE)]))

(define canvas
  (new game-canvas%
       [title CANVAS-TITLE]
       [width CANVAS-WIDTH]
       [height CANVAS-HEIGHT]))

(extract canvas
  [keyboard keyboard]
  [mouse mouse]
  [buffer buffer])

(define graphics
  (new graphics%
       [bitmap buffer]))

(define (get-canvas-width)
  (send-generic canvas get-width))
(define (get-canvas-height)
  (send-generic canvas get-height))

;; Transformation to cartesian coordinates

(define (cartesian-y y)
  (- (send-generic canvas get-height) y))

;; Colors and styles

(define* ; used to draw text and shapes
  [my-font (make-font
          #:size FONT-SIZE
          #:face FONT-FACE
          #:family FONT-FAMILY
          #:style FONT-STYLE
          #:weight FONT-WEIGHT
          #:underlined? FONT-UNDERLINED
          #:smoothing FONT-SMOOTHING
          #:size-in-pixels? FONT-SIZE-IN-PIXELS)]
  [my-brush (new brush%
           [style BRUSH-STYLE]
           [stipple BRUSH-STIPPLE]
           [gradient BRUSH-GRADIENT]
           [transformation BRUSH-TRANSFORMATION])]
  [my-pen (new pen%
         [width PEN-WIDTH]
         [style PEN-STYLE]
         [cap PEN-CAP]
         [join PEN-JOIN]
         [stipple PEN-STIPPLE])])

(define* ; in order to detach the real pen or brush
  [some-brush (new brush%)]
  [some-pen (new pen%)])

(define (make-color r g b [alpha 1.0])
  (make-object color% (* r 16) (* g 16) (* b 16) alpha))

(define (find-color name)
  (send the-color-database find-color name))

(define (only-brush-color! color) ; voor vormen die gevuld moeten worden
  (send-generic graphics set-brush some-brush)
  (send-generic my-brush set-brush-color color)
  (send-generic graphics only-brush my-brush))

(define (only-pen-color! color) ; voor vormen die enkel uit lijnen bestaan
  (send-generic graphics set-pen some-pen)
  (send-generic my-pen set-pen-color color)
  (send-generic graphics only-pen my-pen))

(define*
  [black (make-color 0 0 0)]
  [white (make-color 15 15 15)]
  [red (make-color 15 0 0)]
  [green (make-color 0 15 0)]
  [blue (make-color 0 0 15)])

;; Drawing with images

(define make-image (class-constructor bitmap%))
(define image-width (class-method-accessor bitmap% 'get-width))
(define image-height (class-method-accessor bitmap% 'get-height))

(define (draw-image! x y image)
  (send-generic graphics draw-bitmap image x (- (cartesian-y y) (image-height image))))

;; Standard drawing operations

(define (put-pixel! x y color)
  (only-pen-color! color)
  (send-generic graphics draw-point x (cartesian-y y)))

(define (draw-line! x1 y1 x2 y2 color)
  (only-pen-color! color)
  (send-generic graphics draw-line x1 (cartesian-y y1) x2 (cartesian-y y2)))

(define (fill-rectangle! x y width height color)
  (only-brush-color! color)
  (send-generic graphics draw-rectangle x (- (cartesian-y y) height) width height))

(define (fill-ellipse! x y width height color)
  (only-brush-color! color)
  (send-generic graphics draw-ellipse
                (- x (/ width 2))
                (- (cartesian-y y) (/ height 2)) width height))

(define (draw-text! x y text color [font my-font])
  (send-generic graphics set-text-foreground color)
  (send-generic graphics set-font font)
  (send-generic graphics draw-text text x
                (- (cartesian-y y)
                   (send-generic graphics get-char-height))))

;; Canvas meta events

(define (on-close! proc)
  (chain canvas hide listeners (add! proc)))

(define (on-resize! proc)
  (chain canvas resize (add! proc)))

(define (off-close! proc)
  (chain canvas close (delete! proc)))

(define (off-resize! proc)
  (chain canvas resize (delete! proc)))

;; Keyboard event listening

(define (on-key! code proc)
  (chain keyboard (get-key code) press listeners (add! proc)))

(define (on-release! code proc)
  (chain keyboard (get-key code) release listeners (add! proc)))

(define (off-key! code proc)
  (chain keyboard (get-key code) press listeners (delete! proc)))

(define (off-release! code proc)
  (chain keyboard (get-key code) release listeners (delete! proc)))

;; Mouse event listening

(define (on-click! proc)
  (chain mouse left press listeners (add! proc)))

(define (on-move! proc)
  (chain mouse move listeners (add! proc)))

(define (off-click! proc)
  (chain mouse left press listeners (delete! proc)))

(define (off-move! proc)
  (chain mouse move listeners (delete! proc)))

;; Game looping and timekeeping

(define (post-process)
  (send-generic canvas update)
  (send-generic graphics clear))

(define (start-game-loop thunk [pass-time-delta? #f])
  (set-field! callback game-loop
              (if pass-time-delta?
                  (lambda (delta)
                    (thunk delta)
                    (post-process))
                  (lambda (delta)
                    (thunk)
                    (post-process))))
  (send game-loop start))

(define (stop-game-loop)
  (send game-loop stop))

;; Finishing touch

(send canvas focus) ; give immediate focus to the window
(on-close! stop-game-loop) ; disable the gameloop when closed