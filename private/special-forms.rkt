#lang racket/base

(require racket/class)

(provide (all-from-out racket/class)
         
         define/expression-group
         define
         define*
         
         define-generics
         define-generics*
         chain
         chain*
         extract
         class-constructor
         class-method-accessor
         class-predicate)

(define-syntax-rule (define/expression-group group-tag grouper-name)
  (define-syntax-rule (grouper-name [expr (... ...)] (... ...))
    (begin (group-tag expr (... ...)) (... ...))))

(define/expression-group define define*)

(define-syntax-rule (declare var ...)
  (begin (define var (void)) ...))

(define-syntax chain
  (syntax-rules ()
    [(chain expr) expr] ; done
    [(chain object (method-expr args ...) exps ...) ; calling a method
     (chain (send object method-expr args ...) exps ...)]
    [(chain object field-expr exps ...) ; looking up a field
     (chain (get-field field-expr object) exps ...)]))

(define-syntax-rule (chain* object (expr ...) ...)
  (begin (chain object expr ...) ...))

(define-syntax-rule (extract object [name path ...] ...)
  (define* [name (chain object path ...)] ...))

(define (class-constructor class)
  (lambda args (apply make-object class args)))

(define (class-method-accessor class method)
  (let ((class-generic (make-generic class method)))
    (lambda (object . args) (send-generic object class-generic . args))))

(define (class-predicate class)
  (if (class? class)
      (lambda (object) (is-a? object class))
      (error 'class-predicate "~a is not a class" class)))

(define-syntax-rule
  (define-generics class/interface method-name ...)
  (define* [method-name (generic class/interface method-name)] ...))

(define/expression-group define-generics define-generics*)