#lang racket

(require ramunk
         2htdp/image
         "syntax.rkt"
         "helpers.rkt"
         "point.rkt"
         "game-object.rkt"
         "timing.rkt")

(provide world%)

(define world%
  (class object%

    (init-field screen 
                [game-loop (new game-loop%)]
                [gravity (make-point 0 9.81)])
  
    (super-new)

    (define objects (mutable-set))

    (field [cp-space (cpSpaceNew)])
    (cpSpaceSetGravity cp-space (send gravity to-cpv))
    (cpSpaceSetIterations cp-space 10)
    (define cp-static-body (cpSpaceGetStaticBody cp-space))

    (define render-job (send game-loop add (lambda (delta) (render))))
    (define physics-job (send game-loop add (lambda (delta) (cpSpaceStep cp-space (exact->inexact (/ delta 1000))))))

    (define/public (render)
      (define dc (send screen get-dc))
      (send dc suspend-flush)
      (send dc clear)
      (for ([object objects])
        (send dc draw-bitmap 
              (image->bitmap (send object render))
              (send object get-x) 
              (send object get-y)))
      (send dc resume-flush))

    (define/public (get-cp-space)
      cp-space)

    (define/public (add-shapes . shapes)
      (for ([shape shapes])
        (define cp-shape (send shape get-cp-shape cp-static-body))
        (cpSpaceAddShape cp-space cp-shape)))

    (define/public (get-body)
      cp-static-body)

    (define/public (add-object obj)
      (cpSpaceAddBody cp-space (send obj get-cp-body))
      (set-add! objects obj))

    (define/public (remove-object obj)
      (set-add! objects obj))

    (define/public (play)
      (send game-loop start)
      (send render-job continue)
      (send render-job continue))

    (define/public (pause)
      (send render-job cancel)
      (send physics-job cancel))

    (define/public (terminate)
      (pause))

    ))
