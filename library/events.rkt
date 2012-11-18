#lang racket/gui

; Event handling and dispatching 
; ==============================
;
; Store all event handlers here.

(require "meta.rkt"
         "../constants.rkt")

(provide event-handler%
         button-handler%
         keyboard-handler%
         mouse-handler%
         canvas-handler%)

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

;; Event handler for a single button

(define button-handler%
  (class event-handler%
    (field [press (new event-handler%)]
           [release (new event-handler%)])
    (super-new)))

;; Mouse event handling

(define wheel-handler%
  (class event-handler%
    (field [roll (new event-handler%)]
           [left (new event-handler%)]
           [right (new event-handler%)])
    (super-new)))

(define mouse-handler%
  (class button-handler%
    (init [wheel-sensitivity 1]) ; currently unused
    (field [move (new event-handler%)]
           [wheel (new wheel-handler%)]
           [left (new button-handler%)]
           [middle (new button-handler%)]
           [right (new button-handler%)])
    (super-new)))

;; Keyboard event handling

(define keyboard-handler%
  (class button-handler%
    
    ; character keys can be hashed
    (define char-buttons
      (build-vector
       (- UTF8-RANGE-END UTF8-RANGE-START)
       (lambda (n) (new button-handler%))))
    
    ; special keys will need to be looked up
    (define special-buttons
      (build-vector
       (vector-length SPECIAL-KEYS)
       (lambda (n) (new button-handler%))))
    
    (super-new)
    
    ; lookup a specific key button
    (define/public (get-key code)
      (cond
        ((char? code)
         (vector-ref char-buttons (- (char->integer code) UTF8-RANGE-START)))
        ((symbol? code)
         (let ((index (vector-member code SPECIAL-KEYS))) ; NOTE: use the BST-algorithm as a replacement
           (or (vector-ref special-buttons index)
               (error 'get-key "the key ~a could not be found (no such symbol)" code))))
        (else (error 'get-key "the key ~a could not be found (invalid type)" code))))))

;; Event handler for canvas events

(define canvas-handler%
  (class event-handler%
    (field [paint (new event-handler%)]
           [show (new event-handler%)]
           [hide (new event-handler%)]
           [resize (new event-handler%)])
    (super-new)))
