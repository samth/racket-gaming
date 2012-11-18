#lang racket/gui

; Procedural operations for the graphics library
; ===============================================
;
; This wrapper makes life easier for those who do
; not like to work with racket objects or prefer
; to use a purely procedural syntax.
;
; Do note that using this wrapper will be slightly
; slower than using the normal object system: tests
; show that the time of the method call itself
; increases with +/- 20% when using these procedures.

(require "meta.rkt"
         "graphics.rkt"
         "looping.rkt"
         "events.rkt")

;;;;;;;;;;;;;;;;;;;;;;
;; Meta Definitions ;;
;;;;;;;;;;;;;;;;;;;;;;

; to immediately provide a certain definition
(define-syntax-rule (define/provide var-name expr)
  (begin (define var-name expr) (provide var-name)))

; create a syntax that provides (optionally renamed) mapped arguments to a certain expression
(define-syntax-rule (define-provide-mapper (syntax-name base-expr ... arg-name) generator-expr)
  (define-recursive-syntax (syntax-name base-expr ...)
    [([arg-name var-name]) (define/provide var-name generator-expr)]
    [(arg-name) (define/provide arg-name generator-expr)]))

(define-provide-mapper (provide-constructors class/interface)
  (class-constructor class/interface))

(define-provide-mapper (provide-method-accessors class/interface method-name)
  (class-method-accessor class/interface 'method-name))

(define-provide-mapper (provide-field-accessors class/interface field-name)
  (class-field-accessor class/interface 'field-name))

(define-provide-mapper (provide-field-mutators class/interface field-name)
  (class-field-mutators class/interface 'field-name))

;;;;;;;;;;;;;;;;;;
;; Constructors ;;
;;;;;;;;;;;;;;;;;;

(provide-constructors [graphics% make-graphics]
                      [event-handler% make-event-handler])

;;;;;;;;;;;;;;;;;;;;;;
;; Method Operators ;;
;;;;;;;;;;;;;;;;;;;;;;

(provide-method-accessors graphics%
                   [show show-graphics]
                   [hide hide-graphics]
                   [update update-graphics]
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

(provide-method-accessors dc<%>
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
                   try-color)

(provide-method-accessors canvas%
                   [get-width get-canvas-width]
                   [get-height get-canvas-height])

(provide-method-accessors pen%
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

(provide-method-accessors brush%
                   [get-color get-brush-color]
                   [get-gradient get-brush-gradient]
                   [get-stipple get-brush-stipple]
                   [get-style get-brush-style]
                   [get-transformation get-brush-transformation]
                   [is-immutable? is-brush-immutable?]
                   [set-color set-brush-color]
                   [set-stipple set-brush-stipple]
                   [set-style set-brush-style])