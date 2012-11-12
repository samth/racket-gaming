#lang racket/base

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
; De pen die met gebruitkt worden in de bakcport van de graphics
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
(define FONT-SIZE 12)
(define FONT-FACE #f)
(define FONT-FAMILY 'default)
(define FONT-STYLE 'normal)
(define FONT-WEIGHT 'normal)
(define FONT-UNDERLINED #f)
(define FONT-SMOOTHING 'default)
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
; Toetsen die niet naar een karakter kunnen worden omgezet.
; Deze zijn voorgedefinieerd in racket.
;
; Voor meer informatie:
; >> http://docs.racket-lang.org/gui/key-event_.html#(meth._(((lib._mred/main..rkt)._key-event~25)._get-key-code))
;
(define SPECIAL-KEYS
  #(start
    cancel
    clear
    shift
    rshift
    control
    rcontrol
    menu
    pause
    capital
    prior
    next
    end
    home
    left
    up
    right
    down
    escape
    select
    print
    execute
    snapshot
    insert
    help
    numpad0
    numpad1
    numpad2
    numpad3
    numpad4
    numpad5
    numpad6
    numpad7
    numpad8
    numpad9
    numpad-enter
    multiply
    add
    separator
    subtract
    decimal
    divide
    f1
    f2
    f3
    f4
    f5
    f6
    f7
    f8
    f9
    f10
    f11
    f12
    f13
    f14
    f15
    f16
    f17
    f18
    f19
    f20
    f21
    f22
    f23
    f24
    numlock
    scroll))