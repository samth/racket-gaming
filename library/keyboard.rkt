#lang racket/base

; Keyboard Interface
; ==================
;
; This is the general representation for
; any type of keyboard. Do note that events
; will never trigger. In order for them
; to work, the keyboard will need to be
; passed to a higher level device that
; can handle it.

(require "shared.rkt"
         "devices.rkt"
         "event.rkt")

(provide keyboard%)

(define key%
  (class button%
    (init-field code)
    (super-new)))

(define keyboard%
  (class object%
    
    ; character keys can be hashed
    (define char-keys
      (new static-sorted-list%
           [size (- UTF8-RANGE-END UTF8-RANGE-START)]
           [vector (build-vector
                    (- UTF8-RANGE-END UTF8-RANGE-START)
                    (lambda (n) (new key% (integer->char (+ n UTF8-RANGE-START)))))]))
    
    ; special keys will need to be looked up
    (define special-keys
      (new dictionary%
           [key-vector SPECIAL-KEYS]
           [value-builder
            (build-vector (vector-length SPECIAL-KEYS)
                          (lambda (n) (new key%)))]))

    (super-new)
    
    ; lookup a specific key button
    (define/public (get-key code)
      (send
       (cond
         ((char? code) char-keys)
         ((symbol? code) special-keys)
         (else (error 'get-key "the key ~a could not be found (invalid type)" code)))
       find code))))