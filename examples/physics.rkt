#lang racket/base

(require "../library/full.rkt")

;; First we define the new space

(define the-gravity (2dv 0.0 9.81))

(define the-space
  (new space%
       [gravity the-gravity]))

(require racket/place)

(define (loop)
  (display "Looping ...")
  (newline)
  (loop))

(place a (loop))
(place b (loop))

;; 