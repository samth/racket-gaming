#lang racket/base

; Programming Utilities
; =====================
;
; Keep the library clean by storing definitions of
; special forms in this file wherever possible. That
; way they can easily be consulted and/or augmented.

(require racket/class)

(provide define*
         declare
         define-generics
         define-generics*
         chain
         chain*
         extract
         class-constructor
         class-method-accessor
         class-predicate)

;; Meta syntax forms

; to group expressions of the same type
(define-syntax-rule (define-expression-group group-tag grouper-name)
  (define-syntax-rule (grouper-name [expr (... ...)] (... ...))
    (begin (group-tag expr (... ...)) (... ...))))

;; General definitions

; to group variable definitions
(define-expression-group define define*)

; to reserve a mutable variable
(define-syntax-rule (declare var ...)
  (begin (define var (void)) ...))

; to define multiple generics of a class
(define-syntax-rule
  (define-generics class/interface method-name ...)
  (define* [method-name (generic class/interface method-name)] ...))

; to define multiple generics of multiple classes
(define-expression-group define-generics define-generics*)

; to navigate in (sub-)objects
(define-syntax chain
  (syntax-rules ()
    [(chain expr) expr] ; done
    [(chain object (method-expr args ...) exps ...) ; calling a method
     (chain (send object method-expr args ...) exps ...)]
    [(chain object field-expr exps ...) ; looking up a field
     (chain (get-field field-expr object) exps ...)]))

; to call multiple methods of (sub-)objects
(define-syntax-rule (chain* object (expr ...) ...)
  (begin (chain object expr ...) ...))

; to inject multiple properties of (sub-)object into the current scope
(define-syntax-rule (extract object [name path ...] ...)
  (define* [name (chain object path ...)] ...))

; create a procedural constructor for a class
(define (class-constructor class)
  (lambda args (apply make-object class args)))

; create a procedural method accessor for a class
(define (class-method-accessor class method)
  (let ((class-generic (make-generic class method)))
    (lambda (object . args) (send-generic object class-generic . args))))

; create an object predicate for a class
(define (class-predicate class)
  (if (class? class)
      (lambda (object) (is-a? object class))
      (error 'class-predicate "~a is not a class" class)))