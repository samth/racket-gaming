#lang racket/base

(require "../gaming.rkt")

(provide (all-from-out "../gaming.rkt")
         
         console
         
         2d?
         2d
         rectangular
         polar
         x
         y
         
         dispatch
         send-message
         
         make-layer
         make-physics)

;; Meta definitions

(define (console msg . vars)
  (display (apply format (string-append "[DEBUG] " msg) vars))
  (newline))

;; Dispatcher objects

(define-syntax-rule (dispatch (object-name) local-var ...)
  (lambda (msg)
    (case msg
      ((local-var) local-var)
      ...
      (else (error 'object-name "message ~a not recognized" msg)))))

(define-syntax send-message
  (syntax-rules ()
    [(_ finish) finish]
    [(_ start (method args ...) expr ...)
     (send-message ((start `method) args ...) expr ...)]
    [(_ start field expr ...)
     (send-message (start `field) expr ...)]))

;; Two-dimensional vectors

(define*
  [2d? complex?]
  [rectangular make-rectangular]
  [polar make-polar]
  [2d rectangular]
  [x real-part]
  [y imag-part])

;; Drawing in layers

(define (make-layer . objects)
  (define (add! object)
    (set! objects (append objects (list object))))
  (define (remove! object)
    (set! objects (remove* (list object) objects)))
  (define (count)
    (length objects))
  (define (for-all proc)
    (for-each proc objects))
  (define (update-all time-delta)
    (for-all (lambda (object) (send-message object (update! time-delta)))))
  (define (draw-all canvas)
    (for-all (lambda (object) (send-message object (draw canvas)))))
  (dispatch (layer)
    for-all
    update-all
    draw-all
    add!
    remove!
    count))

;; A small physics engine

(define (make-physics position speed gravity rotation angular-velocity)
  
  (define (update! time-delta)
    (set! position (+ position (* speed time-delta)))
    (set! speed (- speed (* gravity time-delta)))
    (set! rotation (+ rotation (* time-delta angular-velocity))))
  
  (define (set-position! new-position)
    (set! position new-position))
  (define (set-speed! new-speed)
    (set! speed new-speed))
  (define (set-gravity! new-gravity)
    (set! gravity new-gravity))
  (define (set-rotation! new-rotation)
    (set! rotation new-rotation))
  (define (set-angular-velocity! new-angular-velocity)
    (set! angular-velocity new-angular-velocity))
  
  (define (halt!)
    (set! speed 0))
  (define (float!)
    (set! gravity 0))
  (define (straighten!)
    (set! rotation 0))
  (define (stabilize!)
    (set! angular-velocity 0))
  
  (dispatch (physics)
    update!
    position
    speed
    gravity
    rotation
    angular-velocity
    set-position!
    set-speed!
    set-gravity!
    set-rotation!
    set-angular-velocity!
    halt!
    float!
    stabilize!))