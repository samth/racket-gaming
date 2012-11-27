#lang racket/base

(require "../utils.rkt"
         racket/class
         racket/vector
         racket/list
         racket/mpair)

(provide (all-from-out
          "../utils.rkt"
          racket/class
          racket/vector
          racket/list
          racket/mpair)
         
         binary-search)

(define (binary-search v n ==? <<?)
  (let search-loop
    ((left 0)
     (right (- (vector-length v) 1)))
    (and (<= left right)
         (let ((mid (quotient (+ right left 1) 2)))
           (cond
             ((==? (vector-ref v mid) n)
              mid)
             ((<<? (vector-ref v mid) n)
              (search-loop (+ mid 1) right))
             (else (search-loop left (- mid 1))))))))