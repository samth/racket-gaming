#lang racket/gui

; Shared Device Objects
; =====================

(require "../shared.rkt"
         "../event.rkt")

(provide (all-from-out
         "../shared.rkt"
         "../event.rkt")
         
         push-button%)

;; Device interface for a single button

(define push-button%
  (class object%
    (field [press (new event-handler%)]
           [release (new event-handler%)])
    (super-new)))