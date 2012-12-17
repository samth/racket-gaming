#lang racket/gui

(require "shared.rkt")

(provide graphics%)

;; General variables

(define*
  [no-brush (new brush% [style 'transparent])] ; when we don't need a brush
  [no-pen (new pen% [style 'transparent])]) ; when we don't need a pen

;; Graphics object

(define graphics%
  (class bitmap-dc%
    
    ; methods of dc<%> we will be needing
    (inherit/super set-bitmap
                   set-brush
                   set-pen
                   get-size
                   draw-text
                   get-transformation
                   set-transformation
                   get-initial-matrix 
                   set-initial-matrix)
    
    ; make sure dc<%> draws to the buffer
    (super-new)
    
    ; zet de stijl waarop een bepaald element moet getekend worden
    (define/public (set-brush/pen brush pen)
      (set-brush brush)
      (set-pen pen))
    
    ; gebruik alleen een pen om te tekenen
    (define/public (only-pen pen)
      (set-brush/pen no-brush pen))
    
    ; gebruik alleen een penseel om te tekenen
    (define/public (only-brush brush)
      (set-brush/pen brush no-pen))
    
    ; gebruik een bepaalde transformatie voor een tekenfunctie  
    (define/public (use-transformation thunk new-transformation)
      (let ((old-transformation (get-transformation)))
        (set-transformation new-transformation)
        (thunk)
        (set-transformation old-transformation)))
    
    ; de oorsprong tjdelijk veranderen
    (define/public (use-origin thunk centre-x centre-y)
      (use-transformation thunk (vector (get-initial-matrix) centre-x centre-y 1 1 0)))
    
    ; de rotatie tijdelijk veranderen
    (define/public (use-rotation thunk rotation [centre-x 0] [centre-y 0])
      (use-transformation thunk (vector (get-initial-matrix) centre-x centre-y 1 1 rotation)))
    
    ; de schaal tijdelijk veranderen
    (define/public (use-scale thunk scale-x scale-y [centre-x 0] [centre-y 0])
      (use-transformation thunk (vector (get-initial-matrix) centre-x centre-y scale-x scale-y 0)))))