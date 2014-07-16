#lang racket/base

; Procedural Library Wrapper
; ==========================

(require "private/paradigm-transform.rkt"
         
         racket/gui
         "all.rkt")

(provide
 
 make-color
 
 (class-out (game-dc% make-graphics)
            use-transformation
            use-scale
            use-rotation
            set-brush/pen)
 
 (class-out (canvas<%>)
            [update update-canvas])
 
 (class-out (game-canvas% make-game-canvas)
            [get-width get-canvas-width]
            [get-height get-canvas-height])
 
 (class-out (pen%)
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
 
 (class-out (brush%)
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
