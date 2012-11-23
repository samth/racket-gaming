#lang racket/base

; Priority Queue ADT
; ==================
;
; Priority queues use the same reasoning as a normal
; queue, with the addition that certain data entries
; an get priority over other data entries.
;
; This can be very usefull if you want to group
; entries that have the same importance.

(require "shared.rkt")

(provide priority-queue%)

(define priority-queue%
  (class object%
    
    (define heap #())
    
    (super-new)))