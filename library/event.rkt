#lang racket/gui

; Event handling and dispatching 
; ==============================
;
; This is the core for event-driven
; programming: handling events,
; manipulating them, and tracking/storing
; listeners inside.

(require "shared.rkt")

(provide event-handler%)

;; Generalization of an event handler

(define event-handler%
  (class object%
    
    ; should the event be enabled upon initialization?
    (init-field [enabled? #t])
    
    ; keep track of the listeners
    (define*
      [listeners '()]
      [no-listeners '()]
      [cache (if enabled? listeners no-listeners)])
    
    (super-new)
    
    ; updates the cache to refrect the listeners collection
    (define (update-cache)
      (set! cache (if enabled? listeners no-listeners)))
    
    ; trigger the event with arguments that should be passed to the listeners 
    (define/public (trigger . args)
      (for-each (lambda (listener) (apply listener args)) listeners))
    
    ; add a new listener to the current collection
    (define/public (add! thunk)
      (set! listeners (cons thunk listeners))
      (update-cache))
    
    ; remove all the occurences of a specific listener
    (define/public (remove! thunk)
      (set! listeners (remove thunk listeners))
      (update-cache))
    
    ; cleal the current listener collection
    (define/public (clear!)
      (set! listeners '())
      (update-cache))
    
    ; enable by setting the cache to the listeners collection
    (define/public (enable!)
      (set! enabled? #t)
      (update-cache))
    
    ; disable by setting the cache to an empty listener collection
    (define/public (disable!)
      (set! enabled? #f)
      (update-cache))))
