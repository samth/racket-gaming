#lang racket/gui

; Event handling and dispatching 
; ==============================
;
; This is the core for event-driven
; programming: handling events,
; manipulating them, and tracking/storing
; listeners inside.

(require "private/special-forms.rkt"
         "data/bag.rkt")

(provide event-handler%)

;; Performance optimization for collection

(define-generics bag% for-all)

;; Generalization of an event handler

(define event-handler%
  (class object%
    
    ; should the event be enabled upon initialization?
    (init-field [enabled? #t]
                [listeners (new bag%)])
    
    ; caching the listeners to save an if-test
    (define no-listeners (new bag%))
    (define cache (if enabled? listeners no-listeners))
    
    (super-new)
    
    ; updates the cache to reflect the listeners collection
    (define (update-cache)
      (set! cache (if enabled? listeners no-listeners)))
    
    ; trigger the event with arguments that should be passed to the listeners 
    (define/public (trigger . args)
      (send-generic cache for-all (lambda (listener) (apply listener args))))
    
    ; enable by setting the cache to the listeners collection
    (define/public (enable!)
      (set! enabled? #t)
      (update-cache))
    
    ; disable by setting the cache to an empty listener collection
    (define/public (disable!)
      (set! enabled? #f)
      (update-cache))))
