#lang racket/base

;; A collection optimized for lookups

(require racket/class)

(define dictionary%
  (class* object% (storage<%>)
    
    (init [equality eq?]
          [lesser #f]
          [key-vector #()]
          [value-vector #()])
    
    (define*
      [keys key-vector]
      [values value-vector])
    
    (define storage (or vector (list->vector list) #()))
    
    (define/public (find key)
      (let ((index (vector-member key keys)))
        (and index (vector-ref values index))))))