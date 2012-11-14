#lang r5rs 
(#%require "../devices/canvas-port.rkt")

(define my-red   (make-color 15 0 0))

; Geeft een mouse-click object terug wanneer de gebruiker geklikt heeft of #f wanneer de gebruiker niet geklikt heeft. 
; get-click           void -> ( mouse-click | false )

; Geeft de x-coördinaat van het mouse-click object terug
; mouse-click-x       mouse-click -> integer

; Geeft de y-coördinaat van het mouse-click object terug
; mouse-click-y       mouse-click -> integer


;Voorbeeld
(start-game-loop (lambda ()                  
                   (let ((mc (get-click)))
                     (display mc)
                     ;mc zal #f zijn als de gebruiker in de vorige loop niet geklikt heeft.
                     ; anders zal het een mous-click object zijn waar we de x en y coördinaat van kunnen opvragen
                     (if mc
                         (draw-text! (mouse-click-x mc) (mouse-click-y mc) "Click"  my-red)))))