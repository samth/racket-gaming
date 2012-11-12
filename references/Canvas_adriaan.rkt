#lang racket/gui

;;;;;;;;;;;;;;;;;;;;;;
;; CANVAS INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;
;
; Adriaan Leijnse's gemodificeerde code, gebaseerd op het
; origineel van het Software Languages Lab (zie canvas-soft.rkt).
;
; Geimplementeerd met toestemming van de eigenaar. 
;

(provide clear!
         suspend-flush!
         resume-flush!
         fill-rectangle!
         fill-ellipse!
         draw-line!
         on-key!
         draw-text!
         put-pixel!
         start-game-loop
         current-time
         red
         green
         blue
         make-color
         Pixels-x
         Pixels-y)

(define Pixels-x 600)
(define Pixels-y 400)

;; Windowing classes and objects
(define my-frame%
  (class frame%
    (define/augment (on-close)
      (exit))
    (super-new)))
(define keypresses '())
(define my-canvas%
  (class canvas%
    (define/override (on-char event)
      (define kc (send event get-key-code))
        (cond ((eq? kc 'release) 'dont-care)
        (else
         (execute kc action))))
    (super-new)))
(define frame (new my-frame%
                   [label "Jaarproject (2011-2012)"]
                   [min-width Pixels-x]
                   [min-height Pixels-y]
		   [stretchable-height #f]
		   [stretchable-width #f]))
(define canvas (new my-canvas% [parent frame]))
(define dc (send canvas get-dc))

;; Looping and timekeeping
(define (loop thunk)
  (yield)
  (thunk)
  (queue-callback (lambda() (loop thunk)) #t))
(define (start-game-loop thunk)
  (send frame show #t)
  (loop thunk))
(define (current-time)
  (current-milliseconds))

;; Keyboard handling
(define commands '())
(define (make-command l1 l2)
  (cons l1 l2))
(define (on-key! name function)
  (set! commands (cons (cons name function) commands)))
(define (execute command select)
  (let ([pair (assoc command commands)])
    (when pair
      ((cdr pair)))))
(define (action a) a)
(define (do-action a) (a))
 
;; Drawing functions
(struct color (color pen brush))
(define (make-color r g b)
  (let ([c (make-object color% (* 17 r) (* 17 g) (* 17 b))])
    (color c
           (new pen% [color c])
           (new brush% [color c]))))

(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))

(define red (make-color 15 0 0))
(define green (make-color 0 15 0))
(define blue (make-color 0 0 15))

(define (use-brush! color)
  (send dc set-pen no-pen)
  (send dc set-brush (color-brush color)))
(define (use-pen! color)
  (send dc set-pen (color-pen color))
  (send dc set-brush no-brush))
(define (set-font-color! color)
  (send dc set-text-foreground (color-color color)))

(define (fill-rectangle! x y w h color)
  (use-brush! color)
  (send dc draw-rectangle x (- Pixels-y (+ y h)) w h))
(define (draw-text! x y str color)
  (set-font-color! color)
  (send dc draw-text str x (- Pixels-y y)))
(define (put-pixel! x y color)
  (use-pen! color)
  (send dc draw-point x (- Pixels-y y)))
(define (fill-ellipse! x y w h color)
  (use-brush! color)
  (send dc draw-ellipse (- x (/ w 2)) (- Pixels-y (+ y (/ h 2))) w h))
(define (draw-line! x1 y1 x2 y2 color)
  (use-pen! color)
  (send dc draw-line x1 (- Pixels-y y1) x2 (- Pixels-y y2)))

;; Make canvas white
(define (clear!)
  (send dc clear))
;; Suspend refreshing of canvas
(define (suspend-flush!)
  (send canvas suspend-flush))
;; Resume refreshing of canvas (and draw)
(define (resume-flush!)
  (send canvas resume-flush)
  (send canvas flush))