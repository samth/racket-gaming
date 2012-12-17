#lang racket/base

; Sorted List ADT's
; =================
;
; Sorted lists take two simple predicates to produce
; a list that has either a decreased lookup time or
; an increased mutation speed.
;
; This can comes in handy when you have a mostly
; static data which needs to be accessed really often,
; or when you need to change data more than you read
; from it.
;
; Sorted lists are not a very powerfull datatype.
; In time, they will most likely be replaced with
; binary search trees.

(require "shared.rkt")

(provide static-sorted-list%
         dynamic-sorted-list%)

(define static-sorted-list% ; immutable data with very fast lookup
  (class object%
    
    (init equality lesser
          [from-list #f]
          [from-vector #f])
    
    (define*
      [storage
       (cond
         ((from-list) (list->vector from-list))
         ((from-vector) from-vector))]
      [==? equality]
      [<<? lesser])
    
    (super-new)
    
    (define/public (find element)
      (let loop
        ((bottom 0)
         (top (- (vector-length storage) 1)))
        (let ((index (+ (/ (- top bottom) 2) bottom)))
          (cond
            ((= (- top bottom) 1)
             #f)
            ((==? element (vector-ref storage index))
             (vector-ref storage index))
            ((<<? element (vector-ref storage index))
             (loop bottom index))
            (else (loop index top))))))
    
    (define/public (has? element)
      (find element))))

(define dynamic-sorted-list% ; mutable data with average lookup
  (class object%
    
    (init equality lesser
          [sorted-list #f]
          [sorted-vector #f])
    
    (define*
      [storage
       (cond
         ((sorted-list) sorted-list)
         ((sorted-vector) (vector->list sorted-vector))
         (else '()))]
      [==? equality]
      [<<? lesser])
    
    (define/public (add! element)
      (let loop
        ((lst storage))
        (cond
          ((null? lst) element)
          ((<<? element (car lst))
           (cons element lst))
          (else (cons (car lst) (loop (cdr lst)))))))
    
    (define/public (delete! element)
      (let loop
        ((lst storage))
        (cond
          ((null? lst) '())
          ((==? element (car lst))
           (loop (cdr lst)))
          ((<<? element (car lst))
           lst)
          (else (cons (car lst) (loop (cdr lst)))))))
    
    (define/public (find element)
      (let loop
        ((lst storage))
        (cond
          ((null? lst) #f)
          ((==? (car lst) element)
           (car lst))
          ((<<? element (car lst))
           #f)
          (else (loop (cdr lst))))))))