#lang racket/base

(require "../base/all.rkt"
         racket/place
         racket/class)

(define worker%
  (class object%
    
    (init-field [callbacks '()])
    
    (declare the-place)
    
    (super-new)
    
    (define/public (start)
      (set! the-place
            (place main
                   (place-channel-put main "Hello")))
      (place-channel-get the-place))
    
    (define/public (stop)
      (place-kill the-place))))

(define w (new worker%))
(send w start)