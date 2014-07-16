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

(require racket/vector
         racket/file
         racket/runtime-path
         
         "shared.rkt"
         "push-button.rkt")

(provide keyboard%)

(define UTF8-RANGE-START 0)
(define UTF8-RANGE-END 255)
(define UTF8-RANGE-SIZE (- UTF8-RANGE-END UTF8-RANGE-START))

(define-runtime-path special-codes-file "special-keys")
(define special-codes (list->vector (map string->symbol (file->lines special-codes-file))))

(define key%
  (class push-button%
    (init-field code)
    (super-new)))

(define keyboard%
  (class object%
    
    ; character keys can be stored in a normal vector
    ; with a shift to the right for the char range
    (define char-keys 
      (build-vector
       (- UTF8-RANGE-END UTF8-RANGE-START)
       (lambda (n) (make-object key% (integer->char (+ n UTF8-RANGE-START))))))
    
    ; special keys will need to be looked up
    (define special-keys
      (vector-map (lambda (code)
                    (make-object key% code))
                  special-codes))
    
    (super-new)
    
    ; lookup a specific key button
    (define/public (get-key code)
      (cond
        ((char? code)
         (vector-ref char-keys (- (char->integer code) UTF8-RANGE-START)))
        ((symbol? code) 
         (let ((index (vector-member code special-codes)))
           (if index
               (vector-ref special-keys index)
               (error 'get-key "the key ~a could not be found (no such symbol)" code))))
        (else (error 'get-key "the key ~a could not be found (invalid type)" code))))))