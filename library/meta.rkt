#lang racket/gui

; Library meta definitions
; ========================
;
; Procedures, variables and special forms that are
; applicable to the entire library and might be
; usefull in other programs as well.
;
; Keep the library clean by storing definitions of
; special forms in this file wherever possible. That
; way they can easily be consulted and/or augmented.

(provide define-expression-group
         define-recursive-syntax
         
         define*
         declare
         define-generics
         
         class-constructor
         class-predicate
         class-method-accessor
         
         chain
         chain*
         extract)

;; General definitions

; to group expressions of the same type
(define-syntax-rule (define-expression-group group-tag grouper-name)
  (define-syntax-rule (grouper-name [expr (... ...)] (... ...))
    (begin (group-tag expr (... ...)) (... ...))))

; to group variable definitions
(define-expression-group define define*)

; to reserve a mutable variable
(define-syntax-rule (declare var ...)
  (begin (define var (void)) ...))

; applies a tranform-expr to all expressions after base-expr
(define-syntax-rule
  (define-recursive-syntax (syntax-name base-expr ...)
    [(case-expr ...) transform-expr] ...)
  (define-syntax syntax-name 
    (syntax-rules ()
      [(_ base-expr ...) (void)]
      [(_ base-expr ... case-expr ... exps (... ...))
       (begin transform-expr (syntax-name base-expr ... exps (... ...)))]
      ...)))

;; Classes and objects

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

; to define generics of a class or interface
(define-syntax define-generics
  (syntax-rules ()
    [(_ class/interface) (void)] ; done
    [(_ class/interface [variable-name method-name] expr ...)
     (begin
       (define variable-name (generic class/interface method-name))
       (define-generics class/interface expr ...))]
    [(_ class/interface method-name expr ...)
     (define-generics class/interface [method-name method-name] expr ...)]))

; to navigate in (sub-)objects
(define-syntax chain
  (syntax-rules ()
    [(chain expr) expr]
    [(chain object (method-expr args ...) exps ...)
     (chain (send object method-expr args ...) exps ...)]
    [(chain object field-expr exps ...)
     (chain (get-field field-expr object) exps ...)]))

; to call multiple methods of (sub-)objects
(define-syntax-rule (chain* object (expr ...) ...)
  (begin (chain object expr ...) ...))

; to inject multiple properties of (sub-)object into the current scope
(define-syntax-rule (extract object [name path ...] ...)
  (define* [name (chain object path ...)] ...))