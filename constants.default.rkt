#lang racket/base

(require racket/runtime-path racket/file)
(provide (all-defined-out))

;
; Standaardwaarden van het canvas-element.
;
; Deze waarden worden automatisch genomen als er bij de creatie
; van een nieuw canvas geen argument voor een bepaalde waarde
; was meegegeven.
;
(define CANVAS-TITLE "Jaarproject (2012-2013)")
(define CANVAS-WIDTH 800)
(define CANVAS-HEIGHT 600)
(define CANVAS-SHOWN #t)
(define CANVAS-FRAMERATE 30)

;
; De penseel die moet gebruikt worden in de backport van de graphics
; library. Dit kan alleen hier worden gewijzigd.
;
; Lees meer over de mogelijke waarden van de penselen op
; http://docs.racket-lang.org/draw/brush_.html
;
; WAARSCHUWING: stipple, gadient en tranformation worden momenteel genegeerd
;
(define BRUSH-STYLE 'solid)
(define BRUSH-STIPPLE #f)
(define BRUSH-GRADIENT #f)
(define BRUSH-TRANSFORMATION #f)

;
; De pen die gebruikt wordt in de backport van de graphics
; library. Dit kan alleen hier worden gewijzigd.
;
; Lees meer over de mogelijke waarden van de pennen op
; http://docs.racket-lang.org/draw/pen_.html
;
(define PEN-STYLE 'solid)
(define PEN-WIDTH 1)
(define PEN-CAP 'round)
(define PEN-JOIN 'round)
(define PEN-STIPPLE #f)

;
; Het font dat moet worden gebruikt in de backport van de graphics
; library. Dit kan alleen hier worden gewijzigd.
;
; Lees meer over de mogelijke waarden van de fonts op
; http://docs.racket-lang.org/draw/font_.html
;

(define ONT-COLOR "black")
(define FONT-SIZE 12)
(define FONT-FACE #f)
(define FONT-FAMILY 'roman)
(define FONT-STYLE 'normal)
(define FONT-WEIGHT 'normal)
(define FONT-UNDERLINED #f)
(define FONT-SMOOTHING 'unsmoothed)
(define FONT-SIZE-IN-PIXELS #f)
(define FONT-HINTING 'aligned)

;
; De range van toetsen in UTF-8 die kan gebruikt worden.
;
; Verander dit om exotische karakters als invoer te nemen,
; zoals de toetsen van Arabische of Chinese toetsenborden.
;
(define UTF8-RANGE-START 0)
(define UTF8-RANGE-END 255)
(define UTF8-RANGE-SIZE (- UTF8-RANGE-END UTF8-RANGE-START))

;
; De huidige versie van de library.
;
; Kan gebruikt worden voor de auto-updater.
;
(define-runtime-path version-file "version")

(define ROOT-FOLDER (current-directory))
(define UPDATE-URL "https://github.com/samvv/racket-gaming")
(define VERSION (file->string version-file))
(define AUTO-DOWNLOAD #t)
(define AUTO-EXTRACT #f)
(define CONFIRM-ALL #t)