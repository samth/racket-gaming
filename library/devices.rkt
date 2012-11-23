#lang racket/gui

; Input/Output Devices
; ====================
;
; A collection of small devices that
; do not need to be put into a separate file
; and mey be usefull by other "high level"
; devices.

(require "shared.rkt"
         "event.rkt")

(provide button%)

;; Event handler for a single button

(define button%
  (class object%
    (field [press (new event-handler%)]
           [release (new event-handler%)])
    (super-new)))
