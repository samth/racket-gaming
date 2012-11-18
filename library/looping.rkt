#lang racket/gui

; Physics and animation looping
; =============================
;
; Manages game loops such as animations
; and physics callbacks.

(require "meta.rkt")

(provide game-loop%)

;; Looping with a single callback

(define game-loop%
  (class object%
    
    (init-field callback
                [interval #f]
                [framerate #f])
    
    (cond ; update the interval/framerate fields
      ((not (xor interval framerate))
      (error 'game-loop% "either interval or framerate must be set"))
      (interval (set-interval! interval))
      (framerate (set-framerate! framerate)))
    
    (declare loop) ; will contain the game loop 
    
    (super-new)
    
    ; the actual game-loop that calls handler
    (define (tick old-time)
      (let ((new-time (current-milliseconds)))
        (yield) ; give priority to native events
        (if (> (- new-time old-time) interval) ; check elapsed time
            (begin
              (callback (- new-time old-time))
              (queue-callback (thunk (loop new-time)) #t))
            (queue-callback (thunk (loop old-time)) #t))))
    
    ; start the game loop
    (define/public (start)
      (set! loop tick)
      (loop (current-milliseconds)))
    
    ; stop the game loop
    (define/public (stop)
      (set! loop void))
    
    ; reset the game loop to a new framerate
    (define/public (set-framerate! new-framerate)
      (set! framerate new-framerate)
      (set! interval (/ 1000 new-framerate)))
    
    ; reset the game loop to a new interval
    (define/public (set-interval! new-interval)
      (set! interval new-interval)
      (set! framerate (/ 1000 new-interval)))))
