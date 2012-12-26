#lang racket/base

; Simple Racket Gaming Library
; ============================

(require racket/gui
         "constants.rkt"
         "library/utils.rkt"
         "library/data/all.rkt"
         "library/devices/all.rkt"
         "library/all.rkt")

(provide (all-from-out
          racket/gui
          "library/utils.rkt"
          "library/data/all.rkt"
          "library/devices/all.rkt"
          "library/all.rkt"))

(console "Racket game library v~a loaded" VERSION)