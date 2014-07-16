#lang racket/gui

; Game Canvas
; ===========
;
; This is an augmented version of the
; native canvas-object in racket/gui.
;
; Like the normal canvas%, it is an
; object that can handle both input
; (from the keyboard and mouse) and
; output (from a drawing container).

(require "shared.rkt"
         "keyboard.rkt"
         "mouse.rkt")

(provide game-canvas%)

; save method lookups for drawing
(define-generics*
  (dc<%> draw-bitmap)
  (event-handler% trigger))

;; Using the screen display

(define (get-display-width)
  (let-values ([(width height) (get-display-size)]) width))

(define (get-display-height)
  (let-values ([(width height) (get-display-size)]) height))

;; The object that glues it all together

(define game-canvas%
  (class canvas%
    
    ; some procedures of the parent we will need
    (inherit/super get-dc get-size)
    
    ; some default intitialization variables
    (init [width 800]
          [height 600]
          [title "Jaarproject (2012 - 2013)"]
          [shown? #t])
          
    ; optional child objects of the canvas
    (init-field [parent
                 (new frame%
                      [label title]
                      [width width]
                      [height height]
                      [style '()])]
                [mouse (new mouse%)]
                [buffer (make-screen-bitmap (get-display-width) (get-display-height))]
                [keyboard (new keyboard%)])
    
    ; all sorts of canvas events
    (field [paint (new event-handler%)]
           [show (new event-handler%)]
           [hide (new event-handler%)]
           [resize (new event-handler%)])
                   
    ; show or hide the screen
    (send parent show shown?)
    
    ; save field lookups on mouse-events
    (extract mouse
      [motion move]
      [wheel-roll wheel roll]
      [wheel-left wheel left]
      [wheel-right wheel right]
      [left-down left press]
      [left-up left release]
      [middle-down middle press]
      [middle-up middle release]
      [right-up right press]
      [right-down right release])
    
    ; we will be using generics on the most used operations
    (define-generics mouse-event% get-event-type get-x get-y)
    (define-generics key-event% get-key-code get-key-release-code)
    
    ; intialize the canvas with the optional window
    (super-new [parent parent])
      
    ; redefine the callback that runs when showing/hiding the canvas
    (define/override (on-superwindow-show shown?)
      (send-generic (if shown? show hide) trigger))
    
    ; redefine the callback that is executed upon resize
    (define/override (on-size width height)
      (send-generic resize trigger width height))
    
    ; redefine the callback that paints the canvas
    (define/override (on-paint)
      (send-generic paint trigger))
    
    ; process the input of the keyboard
    (define/override (on-char event) ; http://docs.racket-lang.org/gui/key-event_.html
      (let ((code (send-generic event get-key-code)))
        (case code
          ((wheel-up) (send-generic wheel-roll trigger +))
          ((wheel-down) (send-generic wheel-roll trigger -))
          ((wheel-left) (send-generic wheel-left trigger))
          ((wheel-right) (send-generic wheel-right trigger))
          ((release) (send-generic (chain keyboard (get-key (send-generic event get-key-release-code)) release) trigger))
          (else (send-generic (chain keyboard (get-key code) press) trigger)))))
    
    ; process the input of the mouse
    (define/override (on-event event) ; http://docs.racket-lang.org/gui/mouse-event_.html
      (let ((x  (send-generic event get-x))
            (y (send-generic event get-y)))
        (case (send-generic event get-event-type)
          ((motion) (send-generic motion trigger x y))
          ((left-down) (send-generic left-down trigger x y))
          ((left-up) (send-generic left-up trigger x y))
          ((middle-down) (send-generic middle-down trigger x y))
          ((middle-up) (send-generic middle-up trigger x y))
          ((right-up) (send-generic right-up trigger x y))
          ((right-down) (send-generic right-down trigger x y)))))
    
    ; get the drawing container of this canvas
    (define dc (get-dc))
    
    ; we won't be using racket's built-in buffer
    ;(send canvas suspend-flush) ; WRONG: we must
    
    ; push the content of the buffer to the screen
    (define/public (update)
      (send-generic dc draw-bitmap buffer 0 0))))