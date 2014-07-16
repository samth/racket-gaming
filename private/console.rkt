#lang racket/base

; Programming Utilities
; =====================

(provide console
         debug)

; displaying usefull information in the console
(define (console str . args)
  (display (apply format str args))
  (newline))

; pretty-printing varibles
(define (debug . vals)
  (display "[DEBUG] ")
  (for-each (lambda (val)
              (print val)
              (display " "))
            vals)
  (newline))