#lang racket/base

(require "../../library/canvas.rkt")

(define WIDTH 800)
(define SPEED 10)
(define COLOR blue)

(define (make-square x y size)
  (define (dispatch msg . args)
    (apply
      (case msg
        ((x)       get-x)
        ((y)       get-y)
        ((width)   get-width)
        ((height)  get-height)
        ((x!)      set-x!)
        ((y!)      set-y!)
        (else
          (error msg "method missing ~a" dispatch)))
      args))

  (define (get-x)      x)
  (define (get-y)      y)
  (define (get-width)  size)
  (define (get-height) size)

  (define (set-x! new-x) (set! x new-x))
  (define (set-y! new-y) (set! y new-y))

  dispatch)


(define (render-rectangle rectangle)
  (let ((x      (rectangle 'x))
        (y      (rectangle 'y))
        (width  (rectangle 'width))
        (height (rectangle 'height)))
    (fill-rectangle! x y width height COLOR)))


(define (update-rectangle delta rectangle)
  (let ((x (rectangle 'x)))
    (if (> x WIDTH)
      (rectangle 'x! 0)
      (rectangle 'x! (+ (* SPEED (/ delta 100)) x)))))


(start-game-loop
  (let ((square  (make-square 0 200 100)))
    (lambda (delta)
      (render-rectangle square)
      (update-rectangle delta square)))
  #t)
