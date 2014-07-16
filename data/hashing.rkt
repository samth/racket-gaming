#lang racket/base

;; A pre-determined collection of sorted items

(require racket/class)

(define hash-table%
  (class* object% (storage<%>)
    
    (init hasher size
          [vector #()])
    
    (define storage vector)
    
    (define (get-size)
      (vector-length storage))
    
    (define/public (find key)
      (let ((index (hash key)))
        (if (< index 0 (- (get-size) 1))
            (vector-ref storage index)
            #f)))))