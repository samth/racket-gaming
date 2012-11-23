#lang racket/base

; Queue ADT
; =========
;
; Queues can be used to delay to define an order
; of operation in a set of data entries. The last
; one added will also be the last one to be served.

(require "shared.rkt")

(provide queue%)

(define queue%
  (class object%
    
    (define* ; local variables
      [storage '()]
      [last-element #f])
    
    (super-new)))
