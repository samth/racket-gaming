#lang racket/gui

; Physics and animation looping
; =============================
;
; Manages game loops such as animations
; and physics callbacks.

(require "shared.rkt")

(provide loop%)

;; Looping with a single callback

(define loop%
  (class object%
    
    (init-field callback interval)
    
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
    
    ; reset the game loop to a new interval
    (define/public (set-interval! new-interval)
      (set! interval new-interval))))


(define animation%
  (class loop%
    
    (inherit/super set-interval!)
    
    (init on-frame)
    (init-field [framerate 30])
    
    (super-new [interval (/ 1000 framerate)]
               [callback on-frame])
    
    (define/public (set-framerate! new-framerate)
      (set! framerate new-framerate)
      (set-interval! (/ 1000 new-framerate)))))