#lang racket/gui

(require "meta.rkt"
         "events.rkt"
         "looping.rkt"
         "../constants.rkt")

(provide graphics%)

;; Declaring generics for higher performance

(define-generics event-handler% trigger)
(define-generics keyboard-handler% get-key)
(define-generics canvas% flush)
(define-generics dc<%> draw-bitmap)

;; General variables

(define*
  [no-brush (new brush% [style 'transparent])] ; when we don't need a brush
  [no-pen (new pen% [style 'transparent])]) ; when we don't need a pen

;; Event-driven canvas

(define evented-canvas% ; for use with event handlers
  (class canvas%
    
    ; event handlers die zullen worden opgeroepen
    (init-field events mouse keyboard)
    
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
    
    (super-new)
    
    ; redefine the callback that runs when showing/hiding the canvas
    (define/override (on-superwindow-show shown?)
      (send-generic (if shown? (chain events show) (chain events hide)) trigger))
    
    ; redefine the callback that is executed upon resize
    (define/override (on-size width height)
      (send-generic (chain events resize) trigger width height))
    
    ; redefine the callback that paints the canvas
    (define/override (on-paint)
      (send-generic (chain events paint) trigger))
    
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
      (case (send-generic event get-event-type)
        ((motion) (send-generic motion trigger (send-generic event get-x) (send-generic event get-y)))
        ((left-down) (send-generic left-down trigger))
        ((left-up) (send-generic left-up trigger))
        ((middle-down) (send-generic middle-down trigger))
        ((middle-up) (send-generic middle-up trigger))
        ((right-up) (send-generic right-up trigger))
        ((right-down) (send-generic right-down trigger))))))

;; Graphics object

(define graphics%
  (class bitmap-dc% ; we take the class that needs the most optimizations
    
    ; methods of dc<%> we will be needing
    (inherit/super set-bitmap
                   clear
                   set-brush
                   set-pen
                   get-size
                   draw-text
                   get-transformation
                   set-transformation
                   get-initial-matrix 
                   set-initial-matrix
                   set-origin)
    
    ; initialisatiewaarden voor het venster
    (init [width 800]
          [height 600]
          [title "Jaarproject (2012 - 2013)"]
          [shown? #t]
          [framerate 30])
    
    ; keep all animation callbacks in an event-handler%
    (field [animations (new event-handler%)])
    
    ; the callback that triggers animations for this object
    (define (trigger-animations delta)
      (send-generic animations trigger (/ delta 1000)) ; call all of the animation functions
      (update)) ; and update the screen of the user
    
    ; create a new game loop with the according framerate
    (define the-game-loop
      (new game-loop%
           [callback trigger-animations]
           [framerate framerate]))
    
    ; het type assenstelsel dat moet gebruikt worden
    (init-field [mode 'cartesian]
                [window ; create a new window if none was provided
                 (new frame%
                      [label title]
                      [width width]
                      [height height]
                      [style '()])])
    
    ; abstraheer de input-events
    (field [mouse (new mouse-handler%)]
           [keyboard (new keyboard-handler%)]
           [events (new canvas-handler%)])
    
    ; create the canvas container object
    (field [canvas
            (new evented-canvas%
                 [events events]
                 [mouse mouse]
                 [keyboard keyboard]
                 [parent window]
                 [style '()])])
    
    ; show or hide the screen
    (send window show shown?)
    
    ; we will be drawing to this bitmap
    (define buffer (make-bitmap width height))
    
    ; make sure dc<%> draws to the buffer
    (super-new [bitmap buffer])
    
    ; get the dc<%> that draws to the screen
    (define target (send canvas get-dc))
    
    ; push the buffer to the screen
    (define/public (update)
      (send-generic target draw-bitmap buffer 0 0)
      (send-generic canvas flush))
    
    ; shortcut to show the screen
    (define/public (show)
      (send window show #t))
    
    ; shortcut to hide the screen
    (define/public (hide)
      (send window show #f))
    
    ; return the width of the buffer
    (define/public (get-width)
      (let-values ([(width height) (get-size)]) width))
    
    ; return the height of the buffer
    (define/public (get-height)
      (let-values ([(width height) (get-size)]) height))
    
    ; zet de stijl waarop een bepaald element moet getekend worden
    (define/public (set-brush/pen brush pen)
      (set-brush brush)
      (set-pen pen))
    
    ; gebruik alleen een pen om te  tekenen
    (define/public (use-pen pen)
      (set-brush/pen no-brush pen))
    
    ; gebruik alleen een penseel om te tekenen
    (define/public (use-brush brush)
      (set-brush/pen brush no-pen))
    
    ; zet het canvas naar het cartesisch assenstelsel
    (define/public (set-cartesian)
      (set-initial-matrix (vector 1 0 0 -1 0 (get-height))))
    
    ; zet het canvas naar documentmodus (compatibiliteit)
    (define/public (set-document)
      (set-initial-matrix (vector 1 0 0 1 0 0)))
    
    ; gebruik een bepaalde transformatie voor een tekenfunctie  
    (define/public (use-transformation thunk new-transformation)
      (let ((old-transformation (get-transformation)))
        (set-transformation new-transformation)
        (thunk)
        (set-transformation old-transformation)))
    
    ; de rotatie tijdelijk veranderen
    (define/public (use-rotation thunk rotation [centre-x 0] [centre-y 0])
      (use-transformation thunk (vector (get-initial-matrix) centre-x centre-y 1 1 rotation)))
    
    ; de schaal tijdelijk veranderen
    (define/public (use-scale thunk scale-x scale-y [centre-x 0] [centre-y 0])
      (use-transformation thunk (vector (get-initial-matrix) centre-x centre-y scale-x scale-y 0)))
    
    ; tijdelijk het cartesisch assenstelsel gebruiken
    (define/public (use-cartesian thunk)
      (use-transformation thunk (vector (vector 1 0 0 -1 0 (get-height)) 0 0 1 1 0)))
    
    ; tijdelijk de documenten-layout gebruiken
    (define/public (use-document thunk)
      (use-transformation thunk (vector (vector 1 0 0 1 0 0) 0 0 1 1 0)))
    
    ; stel het juiste assenstelsel in
    (case mode
      ((cartesian) (set-cartesian))
      ((document) (set-document))
      (else (error 'graphics "the mode ~a is not supported by this library" mode)))
    
    ; we won't be using racket's built-in buffer
    ;(send canvas suspend-flush)
    
    ; start up the animation loop
    (send the-game-loop start)
    
    ; a hack to make sure we can draw immediately
    (yield)))