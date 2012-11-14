#lang racket/base

(require "../graphics.rkt")

;
; Plaats in deze file alle tests die verband houden
; met de perfomantie van objecten, zoals lookups
; van velden, uitvoeren van methoden, ...
;

(define (repeat proc n)
  (when (> n 0)
    (proc)
    (repeat proc (- n 1))))

(define (make-test)
  (define (draw!)
    (list 1 2 3 4))
  (lambda (msg . arg)
    (case msg
      ((bla) 1)
      ((ble) 2)
      ((bli) 3)
      ((blo) 4)
      ((blu) 5)
      ((draw) (apply draw! arg))
      (else (error 'not-found)))))

(define test1 (make-test))

(define test%
  (class object%
    (super-new)
    (define/public (draw!)
      (list 1 2 3 4))))

(define test2 (new test%))