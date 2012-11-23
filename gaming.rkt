#lang racket/base

; Simple Racket Gaming Library
; ============================
;
; This is the default library importation
; file. It includes the core library plus
; a set of procedural operators. It makes
; life easier for those who do not like to
; work with racket objects.
;
; Do note that the procedural wrapper will be slightly
; slower than the normal object system: tests
; show that the time of a method call itself
; increases with +/- 20% when using these procedures.
;
; **Warning:**
;
; It is recommend NOT to use the procedural
; operators. They will be removed in the near future.
; Instead, try to use racket's standard `send`
; and `send-generic` special forms or the custom
; `chain` special for optimal performance.

(require racket/gui
         "library/special-forms.rkt"
         "library/event.rkt"
         "library/game-canvas.rkt"
         "library/input-devices.rkt"
         "library/graphics.rkt")

(provide (all-from-out
          racket/gui
          "library/special-forms.rkt"
          "library/event.rkt"
          "library/game-canvas.rkt"
          "library/input-devices.rkt"
          "library/graphics.rkt"))


;; Meta definitions

; to immediately provide a certain definition
(define-syntax-rule (define/provide var-name expr)
  (begin (define var-name expr) (provide var-name)))

; to convert a method into a generic procedure
(define (class-method-accessor class/interface method-name)
  (let ((operation (make-generic class/interface method-name)))
    (lambda (object . args) (send-generic object operation . args))))

; to create a procedural object constructor
(define (class-constructor class)
  (lambda args (apply make-object class args)))

; to create a predicate that checks if an object is instance of a class
(define (class-predicate class)
  (if (class? class)
      (lambda (object) (is-a? object class))
      (error 'class-predicate "~a is not a class" class)))

; create a syntax that provides (optionally renamed) mapped arguments to a certain expression
(define-syntax-rule (define-provide-mapper (syntax-name base-expr ... arg-name) generator-expr)
  (define-recursive-syntax (syntax-name base-expr ...)
    [([arg-name var-name]) (define/provide var-name generator-expr)]
    [(arg-name) (define/provide arg-name generator-expr)]))

(define-provide-mapper (provide-constructors class/interface)
  (class-constructor class/interface))

(define-provide-mapper (provide-methodd-accessors class/interface method-name)
  (class-method-accessor class/interface 'method-name))

(define-provide-mapper (provide-field-accessors class/interface field-name)
  (class-field-accessor class/interface 'field-name))

(define-provide-mapper (provide-field-mutators class/interface field-name)
  (class-field-mutators class/interface 'field-name))

(define-syntax-rule
  (provide-class (class/interface constructor-name ...) method-name ...)
  (begin (provide-method-accessors class/interface method-name ...)
         (provide-constructors class/interfacee constructor-name ...)))

;; Poviding mappings for our library

(provide-class (graphics% make-graphics)
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

(provide-class (game-canvas% make-game-canvas)
               [get-width get-canvas-width]
               [get-height get-canvas-height])

;; Native GUI classes

(provide-class (pen%)
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

(provide-class (brush%)
               [get-color get-brush-color]
               [get-gradient get-brush-gradient]
               [get-stipple get-brush-stipple]
               [get-style get-brush-style]
               [get-transformation get-brush-transformation]
               [is-immutable? is-brush-immutable?]
               [set-color set-brush-color]
               [set-stipple set-brush-stipple]
               [set-style set-brush-style])

(provide-class (dc<%>)
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
