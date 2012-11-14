#lang racket/base

(require "../graphics.rkt"
         "shared.rkt")

;;;;;;;;;;;;;;;
;; Fireworks ;;
;;;;;;;;;;;;;;;
;
; Een herimplementatie van de oude versie uit 2011
; met de nieuwe library.
;

;; Instellingen

(define global-gravity (2d 0 170))
(define ground-level 370)

(define firework-scale 0.30)
(define min-lifetime 2000)
(define max-lifetime 3000)
(define min-sparkles 15)
(define max-sparkles 35)
(define min-lumen 10)
(define explode-chance 3) ; one in ...
  
(define min-fire-power 325)
(define max-fire-power 375)
(define rotate-sensitivity 1.5)
(define move-sensitivity 150)
(define cone-size (2d 5 25))
(define base-size 50)

;; Een paar hulpfuncties

(define (random-in-range min max)
  (+ min (random (- max min))))

(define (random-color)
  (make-object color%
    (random-in-range min-lumen 255)
    (random-in-range min-lumen 255)
    (random-in-range min-lumen 255)))

;; Simulatie van fysiche eigenschappen

(define (make-physics position speed gravity rotation angular-velocity)
  
  (define (update! time-delta)
    (set! position (+ position (* speed time-delta)))
    (set! speed (- speed (* gravity time-delta)))
    (set! rotation (+ rotation (* time-delta angular-velocity))))
  
  (define (set-position! new-position)
    (set! position new-position))
  (define (set-speed! new-speed)
    (set! speed new-speed))
  (define (set-gravity! new-gravity)
    (set! gravity new-gravity))
  (define (set-rotation! new-rotation)
    (set! rotation new-rotation))
  (define (set-angular-velocity! new-angular-velocity)
    (set! angular-velocity new-angular-velocity))
  
  (define (halt!)
    (set! speed 0))
  (define (float!)
    (set! gravity 0))
  (define (stabilize!)
    (set! angular-velocity 0))
  
  (dispatch (physics)
    update!
    position
    speed
    gravity
    rotation
    angular-velocity
    set-position!
    set-speed!
    set-gravity!
    set-rotation!
    set-angular-velocity!
    halt!
    float!
    stabilize!))

;; Het echte vuurwerk

(define (make-firework layer position speed strength color [new? #t])
  
  (define this (dispatch (firework) update! draw explode))
  
  (define physics (make-physics position speed global-gravity 0 0))
  (define explode-time (+ (current-milliseconds) (random-in-range min-lifetime max-lifetime)))
  
  (define (update! time-delta)
    (send-message physics (update! time-delta))
    (when (< (y (send-message physics position)) ground-level)
      (send-message layer (remove! this)))
    (when (> (current-milliseconds) explode-time)
      (explode)))
  
  (define (draw canvas)
    (use-brush canvas color)
    (draw-ellipse canvas
                  (x (send-message physics position))
                  (y (send-message physics position))
                  (* strength firework-scale) (* strength firework-scale)))
  
  (define (explode)
    (when (or new? (= (random (- explode-chance 1)) 1))
        (let loop
          ((n strength))
          (if (> n 0)
              (let ((sparkle
                     (make-firework
                       layer
                       (send-message physics position)
                       (* (magnitude (/ speed 2))
                          (rectangular (cos (/ strength n)) (sin (/ strength n))))
                       (/ strength 2)
                       color
                       #f)))
                (send-message layer (add! sparkle))
                (loop (- n 1)))
              (debug "Sploosh!"))))
    (send-message layer (remove! this)))
  
  this)

;; Een kanonnetje om vuurwerk af te schieten

(define (make-cannon target position color)
  
  (define physics (make-physics position (2d 0 0) (2d 0 0) 0 0))
  
  (define (update! time-delta)
    (send-message physics (update! time-delta)))
  
  (define (draw canvas)
    (let ((position (send-message physics position)))
      (use-brush canvas color)
      (use-rotation canvas
                    (thunk (draw-rectangle canvas (- (/ (x cone-size) 2)) 0 (x cone-size) (y cone-size)))
                    (send-message physics rotation)
                    (+ (x (send-message physics position)) (/ (x cone-size) 2))
                    (y (send-message physics position)))))
  
  (define (move! direction) ; + of - voor richting
    (send-message physics (set-speed! (2d (direction move-sensitivity) 0))))
  (define (rotate! direction) ; + or - voor richting
    (send-message physics (set-angular-velocity! (direction rotate-sensitivity))))
  (define (stop-move)
    (send-message physics (halt!)))
  (define (stop-rotate)
    (send-message physics (stabilize!)))
  
  (define (shoot power [custom-sparkles #f])
    (let ((firework
           (make-firework
             target
             (send-message physics position)
             (polar power (- (/ pi 2) (send-message physics rotation)))
             (or custom-sparkles (random-in-range min-sparkles max-sparkles))
             (new brush% [color (random-color)]))))
      
      (send-message target (add! firework))))

  (dispatch (cannon)
    update!
    draw
    shoot
    move!
    rotate!
    stop-move
    stop-rotate))

;; Alles bijeenbrengen in een apart object

(define (make-game canvas move-left-key move-right-key rotate-up-key rotate-down-key shoot-key big-key)
  
  (define fireworks (make-layer))
  
  (define cannon
    (make-cannon fireworks
                 (2d (/ (get-width my-canvas) 2) ground-level)
                 (new brush% [color "grey"])))
  
  (chain move-left-key press (add! (thunk (send-message cannon (move! -)))))
  (chain move-right-key press (add! (thunk (send-message cannon (move! +)))))
  (chain move-left-key release (add! (send-message cannon stop-move)))
  (chain move-right-key release (add! (send-message cannon stop-move)))
  
  (chain rotate-up-key press (add! (thunk (send-message cannon (rotate! -)))))
  (chain rotate-down-key press (add! (thunk (send-message cannon (rotate! +)))))
  (chain rotate-up-key release (add! (send-message cannon stop-rotate)))
  (chain rotate-down-key release (add! (send-message cannon stop-rotate)))
  
  (chain shoot-key press (add! (thunk (debug "POW! Here goes number ~a!" (+ (send-message fireworks (count)) 1)))))
  (chain shoot-key release (add! (thunk (send-message cannon (shoot (random-in-range min-fire-power max-fire-power))))))
  (chain big-key press (add! (thunk (debug "BOOM! Here comes the big one!"))))
  (chain big-key release (add! (thunk (send-message cannon (shoot max-fire-power (+ min-sparkles max-sparkles))))))
  
  (define background (make-object bitmap% "stars.jpg"))
  (define mask (make-object bitmap% "stars-mask.png" 'png/alpha))
  
  (define (frame time-delta)
    (use-document canvas (thunk (draw-bitmap canvas background 0 0)))
    (send-message fireworks (update-all time-delta))
    (send-message fireworks (draw-all canvas))
    (send-message cannon (update! time-delta))
    (send-message cannon (draw canvas))
    (use-document canvas (thunk (draw-bitmap canvas mask 0 0))))
  
  (chain canvas animations (add! frame))
  
  (dispatch (game) canvas fireworks cannon))

;; Een enkelvoudig spel starten

(define my-canvas (make-graphics 1000 800))
(define the-keyboard (chain my-canvas keyboard))

(define my-game
  (make-game my-canvas
             (chain the-keyboard (get-key 'left))
             (chain the-keyboard (get-key 'right))
             (chain the-keyboard (get-key 'up))
             (chain the-keyboard (get-key 'down))
             (chain the-keyboard (get-key #\space))
             (chain the-keyboard (get-key #\tab))))

(chain my-canvas animations (start))