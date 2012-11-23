#lang racket/base

; Bag ADT
; =======
;
; Bags, as the name suggests, store objects without any
; order. That is: when you would iterate over them, you
; can never be sure which one you will encounter first.
;
; A second property of bags is that they can store multiple
; data instances that are equal. When removing them, only the
; item that is matched first is wiped out.

(require "shared.rkt")

(provide bag%)

(define bag%
  (class object%
    
    (init-field [equality eq?]
                [from-list #f]
                [from-vector #f])
    
    (define* ; local variables
      [storage
       (cond
         ((from-list) from-list)
         ((from-vector) (vector->list from-vector))
         (else '()))]
      [==? equality])
    
    (define (make-compare target)
      (lambda (element) (==? element target)))
    
    (super-new)
    
    (define/public (add! . elements)
      (set! storage (append storage elements)))
    
    (define/public (delete! . elements)
      (let loop
        ((lst elements))
        (when (not (null? lst))
          (set! storage (remove (car lst) storage ==?))
          (loop (cdr lst)))))
    
    (define (delete-all! . elements)
      (set! storage (remove* elements storage ==?)))
    
    (define/public (empty!)
      (set! storage '()))
    
    (define/public (for-all proc)
      (for-each proc storage))
    
    (define/public (find element)
      (findf (make-compare element) storage))
    
    (define/public (get-size)
      (length storage))
    
    (define/public (to-list)
      storage)
    
    (define/public (to-vector)
      (list->vector storage))))
