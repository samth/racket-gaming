#lang racket/gui

; Syntax Special Forms
; ====================
;
; Keep the library clean by storing definitions of
; special forms in this file wherever possible. That
; way they can easily be consulted and/or augmented.

(provide define-expression-group
         define-recursive-syntax
         
         define*
         declare
         define-generics
         
         chain
         chain*
         extract)

;; Meta syntax forms

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

;; Classes and objects

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
