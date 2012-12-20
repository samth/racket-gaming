#lang racket/base

; Procedural Library Wrapper
; ==========================
;
; This wrapper has been built for those who prefer
; procedural operators above the racket class
; system.
;
; The procedural wrapper will be slightly slower
; than the normal object system: tests show that
; the time of a method call itself increases with
; +/- 20% when using these procedures.

(require (for-syntax (only-in racket/provide-transform make-provide-pre-transformer))
         "objective.rkt"
         racket/class)

;; Syntax transfomers for generating class operators

(define-syntax class-out
  (make-provide-pre-transformer
    (lambda (stx modes)
      (syntax-case stx ()
        [(_ (class-name constructor-name ...)
            method-name ...)
         (and (identifier? #'class-name)
              (andmap identifier? (syntax-e #'(constructor-name ... method-name ...))))
         #`(rename-out #,@(map (lambda (constructor-id) ; create new constructors on-the-fly
                                 #`[#,(syntax-local-lift-expression
                                       #'(class-constructor class-name))
                                    #,constructor-id])
                               (syntax-e #'(constructor-name ...)))
                       #,@(values
                           (map (lambda (method-id) ; create new method accessors on-the-fly
                                  #`[#,(syntax-local-lift-expression
                                        #`(class-method-accessor class-name '#,method-id))
                                     #,method-id])
                                (syntax-e #'(method-name ...)))))]))))

;; Full list of classes and their operators

(provide (rename-out
          (class-out (graphics% make-graphics)
                     show
                     hide
                     update
                     get-width
                     get-height
                     set-cartesian
                     set-document
                     use-transformation
                     use-scale
                     use-rotation
                     use-cartesian
                     use-document
                     set-brush/pen
                     use-brush
                     use-pen)
          [show show-graphics]
          [hide hide-graphics]
          [update update-graphics])
         
         (rename-out
          (class-out (game-canvas% make-game-canvas)
                     get-width
                     get-height)
          [get-width get-canvas-width]
          [get-height get-canvas-height])
         
         (rename-out
          (class-out (pen%)
                     get-cap
                     get-color
                     get-join
                     get-stipple
                     get-style
                     is-immutable?
                     set-cap
                     set-color
                     set-join 
                     set-stipple
                     set-style
                     set-width)
          [get-cap get-pen-cap]
          [get-color get-pen-color]
          [get-join get-pen-join]
          [get-stipple get-pen-stipple]
          [get-style get-pen-width]
          [is-immutable? is-pen-immutable?]
          [set-cap set-pen-cap]
          [set-color set-pen-color]
          [set-join set-pen-join]
          [set-stipple set-pen-stipple]
          [set-style set-pen-style]
          [set-width set-pen-width])
         
         (rename-out
          (class-out (brush%)
                     get-color
                     get-gradient
                     get-stipple
                     get-style
                     get-transformation
                     is-immutable?
                     set-color
                     set-stipple
                     set-style)
          [get-color get-brush-color]
          [get-gradient get-brush-gradient]
          [get-stipple get-brush-stipple]
          [get-style get-brush-style]
          [get-transformation get-brush-transformation]
          [is-immutable? is-brush-immutable?]
          [set-color set-brush-color]
          [set-stipple set-brush-stipple]
          [set-style set-brush-style])
         
         (class-out (dc<%>)
                    cache-font-metrics-key
                    clear copy
                    draw-arc
                    draw-bitmap
                    draw-bitmap-section
                    draw-ellipse
                    draw-line
                    draw-lines
                    draw-path
                    draw-point
                    draw-polygon
                    draw-rectangle
                    draw-rounded-rectangle
                    draw-spline
                    draw-text
                    end-doc
                    end-page
                    erase
                    flush
                    get-alpha
                    get-background
                    get-brush
                    get-char-height
                    get-char-width
                    get-clipping-region
                    get-device-scale
                    get-font
                    get-gl-context
                    get-initial-matrix 
                    get-origin
                    get-pen
                    get-rotation
                    get-scale
                    get-size
                    get-smoothing
                    get-text-background
                    get-text-extent
                    get-text-foreground
                    get-text-mode
                    get-transformation
                    glyph-exists?
                    ok?
                    resume-flush
                    rotate
                    scale
                    set-alpha
                    set-background
                    set-brush
                    set-clipping-rect
                    set-clipping-region
                    set-font
                    set-initial-matrix
                    set-origin
                    set-pen
                    set-rotation
                    set-scale
                    set-smoothing
                    set-text-background
                    set-text-foreground
                    set-text-mode
                    set-transformation
                    start-doc
                    start-page
                    suspend-flush
                    transform
                    translate
                    try-color))