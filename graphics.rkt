#lang racket/gui

(require racket/gui
         "constants.rkt")

(provide (all-from-out racket/gui)
         
         ; werken met objecten
         define*
         chain
         chain*
         declare
         extract
         make-constructor
         make-operator
         
         ; nieuwe klassen
         graphics%
         event-handler%
         
         ; alternatieve constructoren
         make-graphics
         search-brush
         search-color
         search-font
         search-pen
         
         ; operaties op graphics%
         show-graphics
         hide-graphics
         update-graphics
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
         use-pen
         
         ; operaties op brush%
         get-pen-cap
         get-pen-color
         get-pen-join
         get-pen-stipple
         get-pen-width
         is-pen-immutable?
         set-pen-cap
         set-pen-color
         set-pen-join
         set-pen-stipple
         set-pen-style
         set-pen-width

         ; operaties op pen%
         get-brush-color
         get-brush-gradient
         get-brush-stipple
         get-brush-style
         get-brush-transformation
         is-brush-immutable?
         set-brush-color
         set-brush-stipple
         set-brush-style
         
         ;  operaties op dc<%>
         cache-font-metrics-key
         clear
         copy
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

;;;;;;;;;;;;;;;;;;;;
;; METADEFINITIES ;;
;;;;;;;;;;;;;;;;;;;;

; syntax om makkelijker veel definities aan te maken
(define-syntax-rule
  (define* [name value] ...)
  (begin (define name value) ...))

; om een variabele te reserveren
(define-syntax-rule (declare var)
  (define var (void)))

; een objectconstructor van bepaald type aanmaken
(define (make-constructor class)
  (lambda args (apply make-object class args)))

; een methode van een klasse of interface omzetten naar een procedure
(define (make-operator type method-name)
  (let ((operation (make-generic type method-name)))
    (lambda (object . args) (send-generic object operation . args))))

; syntax om makkelijker veel klasse-operators aan te maken
(define-syntax define-operators
  (syntax-rules ()
    [(define-operators type [method-name var-name])
     (define var-name (make-operator type 'method-name))]
    [(define-operators type method-name)
     (define method-name (make-operator type 'method-name))]
    [(define-operators type [method-name var-name] exps ...)
     (begin
       (define var-name (make-operator type 'method-name))
       (define-operators type exps ...))]
    [(define-operators type method-name exps ...)
     (begin
       (define method-name (make-operator type 'method-name))
       (define-operators type exps ...))]))

; simpele objectnavigatie, naar het voorbeeld van andere objectgerichte talen
(define-syntax chain
  (syntax-rules ()
    [(chain expr) expr]
    [(chain object (method-expr args ...) exps ...)
     (chain (send object method-expr args ...) exps ...)]
    [(chain object field-expr exps ...)
     (chain (get-field field-expr object) exps ...)]))

; meerdere methoden tegelijk uitvoeren
(define-syntax-rule (chain* object (expr ...) ...)
  (begin (chain object expr ...) ...))

; meerdere objecteigenschappen naar buiten halen
(define-syntax-rule (extract object [name path ...] ...)
  (define* [name (chain object path ...)] ...))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; SELECTIEVERSNELLING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

; optimizaties voor het tekenen:
(define-operators dc<%> cache-font-metrics-key clear copy draw-arc draw-bitmap draw-bitmap-section draw-ellipse draw-line draw-lines draw-path draw-point draw-polygon draw-rectangle draw-rounded-rectangle draw-spline draw-text end-doc end-page erase flush get-alpha get-background get-brush get-char-height get-char-width get-clipping-region get-device-scale get-font get-gl-context get-initial-matrix get-origin get-pen get-rotation get-scale get-size get-smoothing get-text-background get-text-extent get-text-foreground get-text-mode get-transformation glyph-exists? ok? resume-flush rotate scale set-alpha set-background set-brush set-clipping-rect set-clipping-region set-font set-initial-matrix set-origin set-pen set-rotation set-scale set-smoothing set-text-background set-text-foreground set-text-mode set-transformation start-doc start-page suspend-flush transform translate try-color)

; voor het tekenen met een pen:
(define-operators pen%
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

; voor het tekenen met een borstel:
(define-operators brush%
  [get-color get-brush-color]
  [get-gradient get-brush-gradient]
  [get-stipple get-brush-stipple]
  [get-style get-brush-style]
  [get-transformation get-brush-transformation]
  [is-immutable? is-brush-immutable?]
  [set-color set-brush-color]
  [set-stipple set-brush-stipple]
  [set-style set-brush-style])

; nog een paar optimizaties voor muisinvoer:
(define-operators mouse-event% get-event-type get-x get-y)

; en hetzelfde voor de toetseninvoer:
(define-operators key-event% get-key-code get-key-release-code)

; het onthouden van specifieke stijlen:

(define (make-list-lookup list operator)
  (let ((find-or-create (make-generic (object-interface list) operator)))
    (lambda args (send-generic list  find-or-create . args))))

(define*
  [search-brush (make-list-lookup the-brush-list 'find-or-create-brush)]
  [search-color (make-list-lookup the-color-database 'find-color)]
  [search-font (make-list-lookup the-font-list 'find-or-create-font)]
  [search-pen  (make-list-lookup the-pen-list 'find-or-create-pen)])

;;;;;;;;;;;;;;;;;;;;
;; EVENT HANDLING ;;
;;;;;;;;;;;;;;;;;;;;

(define event-handler%
  (class object%
    (define listeners '())
    (declare cache)
    (super-new)
    (define/public (trigger . args)
      (for-each (lambda (listener) (apply listener args)) listeners))
    (define/public (add! thunk)
      (set! listeners (cons thunk listeners)))
    (define/public (remove! thunk)
      (set! listeners (remove thunk listeners)))))

(define button-handler%
  (class event-handler%
    (field [press (new event-handler%)]
           [release (new event-handler%)])
    (super-new)))

;; Muis

(define wheel%
  (class event-handler%
   (field [roll (new event-handler%)]
          [left (new event-handler%)]
          [right (new event-handler%)])
   (super-new)))

(define mouse%
  (class button-handler%
    (field [move (new event-handler%)]
           [wheel (new wheel%)]
           [left (new button-handler%)]
           [middle (new button-handler%)]
           [right (new button-handler%)])
    (super-new)))

;; Toetsenbord

(define keyboard%
  (class button-handler%
    
    ; we gebruiken een hashtabel om toetsen met events te associeren
    (define table (build-vector
                   (+ UTF8-RANGE-SIZE (vector-length SPECIAL-KEYS))
                   (lambda (i) (new button-handler%))))
    
    ; hash een key-code naar een index in de tabel
    (define (hash code)
      (if (symbol? code)
          (+ UTF8-RANGE-SIZE (vector-member code SPECIAL-KEYS))
          (char->integer code)))
    
    (super-new)
    
    ; om een bepaalde toets terug te geven
    (define/public (get-key code)
      (vector-ref table (hash code)))))

; we maken weer wat operators aan voor intern gebruik
(define-operators event-handler% trigger)
(define-operators keyboard% get-key)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; VERALGEMEEND CANVAS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define*
  [no-brush (new brush% [style 'transparent])] ; wanneer we geen brush nodig hebben
  [no-pen (new pen% [style 'transparent])]) ; wanneer we geen pen nodig hebben

(define graphics%
  (class bitmap-dc% ; we nemen de klasse die het meest gebruikt wordt als basis
    
    ; dc<%>-methoden die we nodig zullen hebben of moeten veranderen
    (inherit/super set-bitmap clear set-brush set-pen get-size)
    (inherit/super draw-text)
    (inherit/super get-transformation set-transformation get-initial-matrix set-initial-matrix set-origin)
    
    ; initialisatiewaarden voor het venster
    (init [width 800]
          [height 600]
          [title "Jaarproject (2012 - 2013)"]
          [shown? #t]
          [wheel-sensitivity 1])
    
    ; het type assenstelsel dat moet gebruikt worden
    (init-field [mode 'cartesian])
    
    ; abstraheer de input-events
    (field [mouse (new mouse%)]
           [keyboard (new keyboard%)])
    
    ; bespaar field lookups door objecten naar buiten te halen
    (define*
      [motion (chain mouse move)]
      [wheel-roll (chain mouse wheel roll)]
      [wheel-left (chain mouse wheel left)]
      [wheel-right (chain mouse wheel right)]
      [left-down (chain mouse left press)]
      [left-up (chain mouse left release)]
      [middle-down (chain mouse middle press)]
      [middle-up (chain mouse middle release)]
      [right-up (chain mouse right press)]
      [right-down (chain mouse right release)])
    
    ; andere klassen die nodig zijn voor de interface
    (field [window ; het venster-object
            (new (class frame% (super-new))
                 ; configuratie van het frame%-object
                 [label title]
                 [width width]
                 [height height]
                 [style '()])]
           [canvas ; het canvas-object
            (new (class canvas%
                   (super-new)
                   ; verwerk input die van het toetsenbord komt 
                   (define/override (on-char event)
                     (let ((code (send event get-key-code)))
                       (case code
                         ((wheel-up) (trigger wheel-roll wheel-sensitivity))
                         ((wheel-down) (trigger wheel-roll (- wheel-sensitivity)))
                         ((wheel-left) (trigger wheel-left))
                         ((wheel-right) (trigger wheel-right))
                         ((release) (trigger (chain (get-key keyboard (get-key-release-code event)) release)))
                         (else (trigger (chain (get-key keyboard code) press))))))
                   ; verwerk input die van de muis komt
                   (define/override (on-event event)
                     (let ((type (get-event-type event)))
                       (case type
                         ((motion) (trigger motion (get-x event) (get-y event)))
                         ((left-down) (trigger left-down))
                         ((left-up) (trigger left-up))
                         ((middle-down) (trigger middle-down))
                         ((middle-up) (trigger middle-up))
                         ((right-up) (trigger right-up))
                         ((right-down) (trigger right-down))))))
                 ; configuratie van het canvas%-object
                 [parent window]
                 [style '()])])
    
    ; toon of verberg het venster
    (send window show shown?)
    
    ; naar deze bitmap wordt getekend
    (define buffer (make-bitmap width height))
    
    ; construeer bitmap-dc%
    (super-new [bitmap buffer])
    
    ; de dc<%> die zichtbare inhoud tekent
    (define target (send canvas get-dc))
    
    ; push de huidige bitmap naar het scherm
    (define/public (update)
      (draw-bitmap target buffer 0 0))
    
    ; om het venster zichtbaar te maken
    (define/public (show)
      (send window show #t))
    
    ; om het venster te verbergen
    (define/public (hide)
      (send window show #f))
    
    ; de zichtbare breedte teruggeven
    (define/public (get-width)
      (let-values ([(width height) (get-size)])
        width))
    
    ; de zichtbare hoogte teruggeven
    (define/public (get-height)
      (let-values ([(width height) (get-size)])
        height))
    
    ; hou alle gameloops bij in een event-handler%
    (field [animations
            (new (class event-handler%
                   
                   (inherit/super trigger) ; om een nieuw frame te tekenen
                   (declare loop) ; houdt de gameloop zelf vast
                   
                   ; om de hoeveel tijd het frame moet worden ververst
                   (init [framerate 30])
                   
                   ; framerate omzgezet naar milliseconden
                   (define delta (/ 1000 framerate))
                   
                   ; deze tikfunctie roept de animaties om de zoveel milliseconden op
                   (define (tick old-time)
                     (let ((new-time (current-milliseconds)))
                       (yield)
                       (if (> (- new-time old-time) delta) ; wanneer de framerate overschreden is
                           (begin ; start met renderen van een nieuw frame
                             (trigger (/ (- new-time old-time) 1000)) ; voer alle animatiefuncties uit
                             (update) ; breng de buffer over naar het scherm
                             (clear) ; en leeg de buffer voor de volgende keer
                             (queue-callback (thunk (loop new-time)) #t))
                           (queue-callback (thunk (loop old-time)) #t)))) ; anders gewoon verder wachten
                   
                   (super-new)
                   
                   ; om de animaties te starten
                   (define/public (start)
                     (set! loop tick)
                     (loop (current-milliseconds)))
                   
                   ; om de animaties te stoppen
                   (define/public (stop)
                     (set! loop (thunk 'stopped)))))])
    
    ; zet de stijl waarop een bepaald element moet getekend worden
    (define/public (set-brush/pen brush pen)
      (set-brush brush)
      (set-pen pen))
    
    ; gebruik alleen een pen om te  tekenen
    (define/public (use-pen pen)
      (set-brush/pen no-brush pen))
    
    ; gebruik alleen een penseel om te tekenen
    (define/public (use-brush brush)
      (set-brush/pen brush no-pen))
    
    ; zet het canvas naar het cartesisch assenstelsel
    (define/public (set-cartesian)
      (set-initial-matrix (vector 1 0 0 -1 0 (get-height))))
    
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
      (use-transformation thunk (vector (vector 1 0 0 -1 0 (get-height)) 0 0 1 1 0)))
    
    ; tijdelijk de documenten-layout gebruiken
    (define/public (use-document thunk)
      (use-transformation thunk (vector (vector 1 0 0 1 0 0) 0 0 1 1 0)))
    
    ; stel het juiste assenstelsel in
    (case mode
      ((cartesian) (set-cartesian))
      ((document) (set-document))
      (else (error 'graphics "the mode ~a is not supported by this library" mode)))
    
    (yield))) ; een hack om ervoor te zorgen dat er meteen kan worden getekend

; simpele constructor
(define make-graphics (make-constructor graphics%))

; nog meer operators
(define-operators graphics%
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