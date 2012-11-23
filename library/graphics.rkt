#lang racket/gui

(require "shared.rkt")

(provide graphics%)

;; General variables

(define*
  [no-brush (new brush% [style 'transparent])] ; when we don't need a brush
  [no-pen (new pen% [style 'transparent])]) ; when we don't need a pen

;; Using the screen display

(define (get-display-width)
  (let-values ([(width height) (get-display-size)]) width))

(define (get-display-height)
  (let-values ([(width height) (get-display-size)]) height))

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
                   set-initial-matrix
                   set-origin)
    
    (init-field [width (get-display-width)] ; the width  of this drawing object
                [height (get-display-height)] ; the height of this drawing object
                [mode 'cartesian] ; the type of grid that should be used
                [target (make-bitmap width height)]) ; take the entire screen as buffer
    
    ; make sure dc<%> draws to the buffer
    (super-new [bitmap target])
    
    ; zet de stijl waarop een bepaald element moet getekend worden
    (define/public (set-brush/pen brush pen)
      (set-brush brush)
      (set-pen pen))
    
    ; gebruik alleen een pen om te tekenen
    (define/public (use-pen pen)
      (set-brush/pen no-brush pen))
    
    ; gebruik alleen een penseel om te tekenen
    (define/public (use-brush brush)
      (set-brush/pen brush no-pen))
    
    ; zet het canvas naar het cartesisch assenstelsel
    (define/public (set-cartesian)
      (set-initial-matrix (vector 1 0 0 -1 0 height)))
    
    ; zet het canvas naar documentmodus (compatibiliteit)
    (define/public (set-document)
      (set-initial-matrix (vector 1 0 0 1 0 0)))
    
    ; gebruik een bepaalde transformatie voor een tekenfunctie  
    (define/public (use-transformation thunk new-transformation)
      (let ((old-transformation (get-transformation)))
        (set-transformation new-transformation)
        (thunk)
        (set-transformation old-transformation)))
    
    ; de rotatie tijdelijk veranderen
    (define/public (use-rotation thunk rotation [centre-x 0] [centre-y 0])
      (use-transformation thunk (vector (get-initial-matrix) centre-x centre-y 1 1 rotation)))
    
    ; de schaal tijdelijk veranderen
    (define/public (use-scale thunk scale-x scale-y [centre-x 0] [centre-y 0])
      (use-transformation thunk (vector (get-initial-matrix) centre-x centre-y scale-x scale-y 0)))
    
    ; tijdelijk het cartesisch assenstelsel gebruiken
    (define/public (use-cartesian thunk)
      (use-transformation thunk (vector (vector 1 0 0 -1 0 height) 0 0 1 1 0)))
    
    ; tijdelijk de documenten-layout gebruiken
    (define/public (use-document thunk)
      (use-transformation thunk (vector (vector 1 0 0 1 0 0) 0 0 1 1 0)))
    
    ; stel het juiste assenstelsel in
    (case mode
      ((cartesian) (set-cartesian))
      ((document) (set-document))
      (else (error 'graphics "the mode ~a is not supported by this library" mode)))))