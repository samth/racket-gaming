#lang racket/gui

(provide screen%)

(require (only-in rx event%)
         2htdp/image)

(define custom-frame%
  (class frame%
   
    (field [close-event (new event%)])

    (super-new)

    (define/augment (on-close)
      (send close-event trigger))

    ))

(define screen%
  (class object%

    (init [title ""])
    (init-field [width 800]
                [height 500])

    (define frame (new custom-frame% [label title] [width width] [height height]))
    (define canvas (new canvas% [parent frame]))
    (define dc (send canvas get-dc))

    (super-new)
    
    (define/public (get-width)
      (send frame get-width))
    (define/public (get-height)
      (send frame get-height))

    (define/public (get-dc)
      (send canvas get-dc))

    (send frame show #t)

    ))

