#lang racket

(require "shared.rkt")

;; Canvas aanmaken

(define graphics (make-graphics 500 500))

(extract graphics ; zo moeten we niet altijd (chain graphics ...) gebruiken
  [the-keyboard keyboard]
  [the-mouse mouse]
  [the-animations animations])

(extract the-keyboard ; idem aan vorige: haal de toetsen op die we nodig gaan hebben
  [start-event (get-key #\space) press]
  [up-event (get-key 'up) press]
  [down-event (get-key 'down) press]
  [left-event (get-key 'left) press]
  [right-event (get-key 'right) press])

; een procedure die we veel gaan gebruiken
(define (add-animation! thunk)
  (chain the-animations (add! thunk)))

;; Kleurtjes en stijlen

(define*
  [no-pen
   (new pen%
        [style 'transparent])]
  [no-brush
   (new brush%
        [style 'transparent])]
  [full-black
   (new brush%
        [color "black"])]
  [full-yellow
   (new brush%
        [color "yellow"])]
  [weird-pen
   (new pen%
        [color "green"]
        [width 4]
        [style 'xor])]
  [mixed-pen
   (new pen%
        [color "blue"]
        [width 3]
        [style 'dot-dash])])

;; Een roterend vierkant

(define (rotating-rectangle position size speed brush pen)
  (define rotation 0)
  (lambda (delta)
    (set! rotation (+ rotation (* delta speed)))
    (use-rotation
     graphics
     (thunk
      (set-brush/pen graphics brush pen)
      (draw-rectangle graphics (- (/ (x size) 2)) (-  (/ (y size) 2)) (x size) (y size)))
     rotation
     (x position)
     (y position))))

(add-animation! (rotating-rectangle (2d 50 50) (2d 50 50) 2 full-black no-pen))
(add-animation! (rotating-rectangle (2d 300 400) (2d 50 50) 3 no-brush mixed-pen))
(add-animation! (rotating-rectangle (2d 350 150) (2d 50 50) 1 full-yellow weird-pen))

;; Een beweegbare ellips

(define (make-bullet position size brush pen)
  (define (move! delta)
    (set! position (+ position delta)))
  (define direction 1)
  (define (grow!)
    (when (or (and (positive? direction)
                   (> size 30))
              (and (negative? direction)
                   (< size 5)))
      (set! direction (- direction)))
    (set! size (+ size direction)))
  (define (draw!)
    (set-brush/pen graphics brush pen)
    (draw-ellipse graphics (- (/ size 2)) (- (/ size 2)) size size))
  (lambda (msg . args)
    (case msg
      ((move!) (apply move! args))
      ((grow!) (apply grow! args))
      ((draw!) (apply draw! args))
      (else (error 'mutable-ellipse "I have no idea what you mean ...")))))

(define my-bullet (make-bullet (2d 200 250) 20 full-black no-pen))

(chain up-event (add! (thunk (my-bullet 'move! (2d 0 -10)))))
(chain down-event (add! (thunk (my-bullet 'move! (2d 0 10)))))
(chain left-event (add! (thunk (my-bullet 'move! (2d -10 0)))))
(chain right-event (add! (thunk (my-bullet 'move! (2d 10 0)))))

(add-animation!
 (lambda (delta)
   (my-bullet 'grow!)
   (my-bullet 'draw!)))

;; Afwerking

(chain start-event (add! (thunk (chain the-animations (start)))))

(use-brush graphics full-black)
(draw-rectangle graphics 0 0 500 500)

(set-text-foreground graphics "white")
(draw-text graphics "Press space to start the animations!" 100 200)

(chain the-animations (add! (lambda (delta) (clear graphics))))
(update-graphics graphics)
