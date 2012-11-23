#lang racket

(require rackunit "all.rkt")

;; Testing of bags

(define bag (new bag%))
(send bag add! 1 2 3 4 5 1 1 2)
(send bag delete! 1 2)
(check-= (send bag get-size) 6 0)

;; Testing of sorted lists

(define static-list
  (new static-sorted-list%
       [equality =]
       [lesser <]
       [from-list '(1 2 3 4 5 7 8 9 10)]))

(check-true (send static-list has? 1))
(check-true (send static-list has? 5))
(check-true (send static-list has? 10))
(check-false (send static-list has? 6))