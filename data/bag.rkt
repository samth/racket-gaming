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

(require racket/class
         racket/mpair
         
         "../private/special-forms.rkt")

(provide bag%)


(define (mlast lst)
  (do ((l (car lst) (cdr lst)))
    ((null? (cdr l)) l)))

(define (mfilter p l)
  (cond
    ((null? l) '())
    ((p (car l))
     (cons (car l)
           (mfilter p (cdr l))))
    (else (mfilter (cdr l)))))

(define (mfind pred lst)
  (cond
    ((null? lst) #f)
    ((pred (mcar lst)) lst)
    (else (mfind pred (mcdr lst)))))


(define bag%
  (class* object% ()
    
    ; no fields because they should never be changed
    (init-field [equality eq?]
                [elements '()])
    
    (define* ; local variables
      [storage (list->mlist elements)]
      [==? equality]
      [last-element (mlast elements)])
    
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
        (if (==? (car lst) element)
            (begin
              (remove-sentinel!)
              (if (sentinel? lst) #f lst))
            (loop (mcdr lst)))))
    
    (super-new)
    
    (define/public (add! . elements)
      (set-mcdr! last-element (list->mlist elements)))
    
    (define/public (insert-before! target . elements)
      (let ((previous (find-previous target))
            (mlst (list->mlist elements)))
        (and mlst (set-mcdr! previous mlst))))
    
    (define/public (insert-after! target . elements)
      (add-sentinel! target)
      (let loop
        ((lst storage))
        (if (==? (car lst) target)
            (if (sentinel? lst)
                (error 'bag% "elements ~a not insertable after ~a (not found)" elements target)
                (append (cons (car lst) elements) (cdr lst)))
            (cons (car lst) (loop (cdr lst))))))
    
    (define (delete! . elements)
      (set! storage
            (mfilter (lambda (el)
                       (not (mfind el elements)))
                     storage)))
    
    (define/public (empty!)
      (set! storage  '()))
    
    (define/public (for-all proc)
      (for-each proc storage))
    
    (define/public (find element)
      (findf (lambda (incoming)
               (==? element incoming))
             storage))))
