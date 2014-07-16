#lang racket/base
(require "../library/full.rkt")



(define blue (send the-color-database find-color "blue"))

(let ((brush (new brush% [color blue]))
      (loop   (new loop%
                   [interval (/ 1000 30)]))
      (canvas (new game-canvas%
                   [title  "Graphics Basics"]
                   [width  300]
                   [height 200])))
  (extract canvas [buffer buffer])
  (let ((graphics (new graphics%
                       [bitmap buffer])))
    (define-generics*
      [game-canvas% update]
      [graphics% set-brush draw-rectangle draw-ellipse clear])

    (set-field! callback loop (lambda (delta)
        (send-generic graphics set-brush brush)
        (send-generic graphics draw-rectangle 0 0 300 300)
        (send-generic canvas update)))
    (send loop start)))
