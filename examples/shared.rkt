#lang racket/base

(require "../gaming.rkt")

(provide console
         
         2d?
         2d
         rectangular
         polar
         x
         y
         
         dispatch
         send-message
         
         make-layer)

;; Meta definitions

(define (console msg . vars)
  (display (apply format (string-append "[DEBUG] " msg) vars))
  (newline))

;; Two dimensional vectors

(define*
  [2d? complex?]
  [rectangular make-rectangular]
  [polar make-polar]
  [2d rectangular]
  [x real-part]
  [y imag-part])

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