#lang racket/base

(require "shared.rkt")

; Queue ADT
; =========
;
; Queues can be used to delay to define an order
; of operation in a set of data entries. The last
; one added will also be the last one to be served.

(provide queue%)

(define queue%
  (class object%
    
    (define* ; local variables
      [storage '()]
      [last-element #f])
    
    (super-new)))

; Priority Queue ADT
; ==================
;
; Priority queues use the same reasoning as a normal
; queue, with the addition that certain data entries
; an get priority over other data entries.
;
; This can be very usefull if you want to group
; entries that have the same importance.

(provide priority-queue%)

(define priority-queue%
  (class object%
    
    (define heap #())
    
    (super-new)))