#lang racket/base

; Mouse Interface
; ===============
;
; This is the general representation
; of a mouse. As with the keyboard,
; events are never triggered, only
; stored for use with appropriate
; higher-level mouse handlers.

(require "shared.rkt"
         "devices.rkt"
         "event.rkt")

(provide mouse%)

;; Mouse event handling

(define wheel%
  (class object%
    (field [roll (new event-handler%)]
           [left (new event-handler%)]
           [right (new event-handler%)])
    (super-new)))

(define mouse%
  (class object%
    (init [wheel-sensitivity 1]) ; currently unused
    (field [move (new event-handler%)]
           [wheel (new wheel%)]
           [left (new button%)]
           [middle (new button%)]
           [right (new button%)])
    (super-new)))