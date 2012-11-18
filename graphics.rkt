#lang racket/base

; Simple Racket Gaming Library
; ============================
;
; This is the default library importation
; file. It includes the core library plus
; the procedural operators.
;
; **Warning:**
;
; It is recommend NOT to use the procedural
; operators. Instead, try to use racket's
; standard send and send-generic special
; forms or the custom *chain* special form
; for optimal performance wherever possible.

(require racket/gui
         "library/graphics.rkt"
         "library/meta.rkt"
         "library/operators.rkt")

(provide (all-from-out
          racket/gui
          "library/graphics.rkt"
          "library/meta.rkt"
          "library/operators.rkt"))