#lang r5rs 
(#%require "Canvas.rkt")
(#%require (only racket/base error)) ;for error


; Auxiliary procedures
; --------------------

; sends a message (with optional parameters) to an oo-based implementation of an ADT
(define (send-message object message . parameters)
  (let ((procedure (object message)))
    ; Note that the object's dispatcher always has to return a procedure 
    (apply procedure parameters)))

; ADT tagged data
; ---------------

; META: Example of a procedure-based ADT

; Constructors
(define (make-tagged tag data)
  (cons tag data))

; Predicates
(define (tagged? tagged)
  (pair? tagged))

(define (tagged-as? tagged tag)
  (and (tagged? tagged)
       (eq? (tagged-tag tagged tag))))

; Selectors
(define (tagged-tag x)
  (car x))

(define (tagged-data x)
  (cdr x))

; Mutators
(define (tagged-data! x newvalue)
  (set-cdr! x newvalue))


; Coordinates
; -----------

; META: Example of a procedure-based ADT

;Creates a cartesian coordinates pair
(define (make-coordinates x y)
  (make-tagged 'coordinates (cons x y)))

;Checks whether the argument represents coordinates
(define (coordinates? coos)
  (tagged-as? coos 'coordinates))

;Returns the x coordinate of the argument
(define (coordinates-x coos)
  (car (tagged-data coos)))

;Returns the y coordinate
(define (coordinates-y coos)
  (cdr (tagged-data coos)))

(define (coordinates-x! coos x)
  (set-car! (tagged-data coos) x))

(define (coordinates-y! coos y)
  (set-cdr! (tagged-data coos) y))

;META: the following functions are very similar, this could be improved
(define (coordinates-inc-y! coos increase)
   (coordinates-y! coos (+ (coordinates-y coos) increase)))
   
(define (coordinates-dec-y! coos decrease)
   (coordinates-y! coos (- (coordinates-y coos) decrease)))



; Canvas UI
; ---------

; META: Example of an OO-based ADT

; UI that draws on a window using the Canvas.rkt library
(define (make-canvas-ui)
  (let ((window-w 800)
        (window-h 600)
        (black (make-color 0 0 0))
        (green (make-color 0 15 0)))
    
    ;clears the window by painting it black
    (define (clear)
      (fill-rectangle! 0 0 window-w window-h black))
    
    ;draws the given ball in green
    (define (draw-ball ball)
      (let ((position (send-message ball 'position))
            (radius (send-message ball 'radius)))
        (fill-ellipse! (coordinates-x position)
                       (coordinates-y position)
                       radius
                       radius
                       green)))
    
    (define (dispatch message)
      (case message ;uses cases instead of cond to avoid repeating eq? 
        ((clear) clear)
        ((draw-ball) draw-ball)
        (else (error 'canvas-ui "unknown message ~a" message))))
    dispatch))

; Ball
; ----

; Instantiates a ball with the given radius and x-coordinate
(define (make-ball radius x)
  (let ((dy 10) ;change in vertical position
        (position (make-coordinates x 100))) ;initial position
    
    (define (get-position)
      position)
    
    (define (get-radius)
      radius)
    
    ;Adjusts the ball's position upwards
    (define (up!)
      (coordinates-inc-y! position dy))
      
    ;Adjusts the ball's position downwards
    (define (down!)
      (coordinates-dec-y! position dy))
    
    ;Draws the ball on the given game UI
    (define (draw ui)
      ;does not draw directly, but asks the UI to draw the ball instead
      ;this way, the game can be configured with a different UI
      (send-message ui 'draw-ball dispatch))
    
    ;Processes the events (= user input, sensor input) recorded by event-recorder
    (define (process-events event-recorder)
      (let ((event (send-message event-recorder 'last-recorded-event)))
        ;TODO: this might be slow as there are many events not related to a ball     
        (case event
          ((up) (up!)) ;Key-up event was recorded, move ball upwards
          ((down) (down!)) ;Key-down event was recorded, move ball upwards
          (else 'do-nothing)))) ;Not an event a ball has to react to
    
    (define (dispatch message)
      (case message
        ((position) get-position)
        ((radius) get-radius)
        ((process-events) process-events)
        ((draw) draw)
        ((up!) up!)
        ((down!) down!)
        (else (error 'ball "unknown message ~a" message))))
    dispatch))

; Canvas Event Recorder
; ---------------------

; Using Canvas.rkt, converts the last keyboard input to a game event
(define (make-canvas-event-recorder)
  (let ((event 'no-event)) 
    
    ;Initializes the recorder by linking it to Canvas.rkt
    (define (initialize)
      (clear)
      (on-key! 'up (lambda () (set! event 'up)))
      (on-key! 'down (lambda () (set! event 'down))))
    
    ;Erases the last recorded event by resetting it to a dummy value
    (define (clear)
      (set! event 'no-event))
    
    ;Returns the last recorded event
    ;TODO: recording a single keystroke won't suffice 
    
    (define (last-recorded-event)
      event)
    
    (initialize)
    
    (define (dispatch message)
      (case message
        ((clear) clear)
        ((last-recorded-event) last-recorded-event)
        (else (error 'canvas-event-recorder "unknown message ~a" message))))
    dispatch))


; Game Loop 
; ---------

; Creates a game with the following parameters
; - game-objects: objects (e.g., balls) in the game
; - ui: the ui the game will be drawn on
; - event-recorded: the source of events for the game (e.g., keyboard input -> event)

;TODO: this loop clears and redraws the entire screen, even if nothing has changed
;TODO: this loop is unaware of how much time has passed between its iterations
(define (make-game-loop game-objects ui event-recorder)
  
  ;One iteration of the game loop
  (define (game-advancer)
    ;Clear (=erase) the user interface
    (send-message ui 'clear)
    ;Ask object in the game to ...
    (for-each (lambda (object)
                ;a) process (e.g., update its position) any recorded events
                (send-message object 'process-events event-recorder)
                ;b) draw itself on the screen
                (send-message object 'draw ui))
              game-objects)
    ;Clear the recorded events
    (send-message event-recorder 'clear))
  
  (define (start)
    (start-game-loop game-advancer))
  
  (define (dispatch message)
    (case message
      ((start) start)
      (else (error 'game-loop "unknown message ~a" message))))
  dispatch)


;Start a game with two balls
(send-message (make-game-loop (list (make-ball 20 10)
                                    (make-ball 50 100)) 
                              (make-canvas-ui) 
                              (make-canvas-event-recorder))
              'start)


