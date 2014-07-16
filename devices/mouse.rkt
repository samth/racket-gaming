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
         "push-button.rkt")

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
           [left (new push-button%)]
           [middle (new push-button%)]
           [right (new push-button%)])
    (super-new)))