#lang racket/gui

(require syntax/srcloc
         2htdp/image)

(provide unknown-image
         image->bitmap)

(define (dirname path)
  (let-values ([(base name must-be-dir?) (split-path path)])
    base))

(define (unknown-image)
  (bitmap/file (build-path (dirname (source-location-source #'here)) ".." "assets" "unknown.png")))

(define (image->bitmap image)
  (let* ([width  (image-width  image)]
         [height (image-height image)]
         [bm     (make-bitmap width height)]
         [dc     (send bm make-dc)])
    (send dc clear)
    (send image draw dc 0 0 0 0 width height 0 0 #f)
    bm))


