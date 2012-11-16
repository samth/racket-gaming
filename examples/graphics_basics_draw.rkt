(require "../graphics.rkt")



(let ((canvas (make-graphics 250 175))
      (blue-brush (new brush% [color "blue"]))
      (red-brush  (new brush% [color "red"])))
  (define (draw delta)
    (use-brush canvas blue-brush)
    (draw-rectangle canvas 25 50 100 100)
    (use-brush canvas red-brush)
    (draw-ellipse canvas 125 50 100 100))

  (chain canvas animations (add! draw))
  (chain canvas animations (start)))
