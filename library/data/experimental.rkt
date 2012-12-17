#lang racket/gui

; Data Storage
; ============
;
; Experimental.
;

(require "special-forms.rkt"
         racket/mpair)

(provide (all-from-out "special-forms.rkt")
         
         bag%
         hash-table%
         dictionary%)

;; A collection of unsorted items
private
(define bag%
  (class* object% (storage<%>)
    
    ; no fields because they should never be changed
    (init-field [equality eq?]
                [elements '()])
    
    (private* ; local variables
      [storage (list->mlist elements)]
      [==? equality]
      [last-element (mlast first-element)])
    
    (define (add-sentinel! element)
      (set-mcdr! last-element (mlist element)))
    
    (define (remove-sentinel!)
      (set-mcdr! last-element '()))
    
    (define (sentinel? mlst)
      (eq? mlst (mcdr last-element)))
    
    (define/private (find-previous element)
      (add-sentinel! element)
      (let loop
        ((lst storage))
         (if (==? (car lst) target)
             (if (sentinel? lst)
                 (begin (remove-sentinel!) #f)
                 (begin (remove-sentinel!) lst))
             (loop (mcdr lst)))))
    
    (define/private (make-mutable lst last-element)
      (let loop
        ((rest lst))
        (if (null? lst)
            last-element
            (cons (car lst)
                  (loop (cdr lst))))))
    
    (super-new)
    
    (define/public (add! . elements)
      (set-mcdr! last-element
                 (let loop
                   ((lst elements))
                   (if (null? (cdr lst))
                       (begin (set! last-element lst) lst)
                       (mcons (loop (cdr lst)))))))
    
    (define/public (insert-before! target . elements)
      (let ((previous (find-previous target))
            (mlst (list->mlist elements)))
        (set-cdr! previous mlst))))
    
    (define/public (insert-after! target . elements)
      (add-sentinel! target)
      (let loop
        ((lst storage))
        (if (==? (car lst) target)
            (if (sentinel? lst)
                (error 'bag% "elements ~a not insertable after ~a (not found)" elements target))
           (append (cons (car lst) elements) (cdr lst)))
          (else (cons (car lst) (loop (cdr lst)))))))
    
    (define (delete! . elements)
      (set! storage (mremove* elements storage ==?)))
    
    (define/public (empty!)
      (set! storage  '()))
    
    (define/public (for-all proc)
      (for-each proc storage))
    
    (define/public (find element)
      (findf (make-compare element) storage))))

;; A pre-determined collection of sorted items

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

;; A collection optimized for lookups

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
      