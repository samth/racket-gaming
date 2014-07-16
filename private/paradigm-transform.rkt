#lang racket/base

(require "special-forms.rkt"
         
         (for-syntax
          racket/base
          (only-in racket/provide-transform
                   make-provide-pre-transformer)))

(provide class-out)

; converting classes to procedures
(define-syntax class-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     (syntax-case stx ()
       [(_ (class-name constructor-name ...)
           method-name ...)
        (and (identifier? #'class-name)
             (andmap identifier? (syntax-e #'(constructor-name ...))))
        #`(rename-out #,@(map (lambda (constructor-id) ; create new constructors on-the-fly
                                #`[#,(syntax-local-lift-expression
                                      #'(class-constructor class-name))
                                   #,constructor-id])
                              (syntax-e #'(constructor-name ...)))
                      #,@(map (lambda (method-spec) ; create new method accessors on-the-fly
                                (syntax-case method-spec ()
                                  [(method-name procedure-name)
                                   (and (identifier? #'method-name)
                                        (identifier? #'procedure-name))
                                   #`(#,(syntax-local-lift-expression
                                         #'(class-method-accessor class-name 'method-name))
                                      procedure-name)]
                                  [method-name
                                   (identifier? #'method-name)
                                   #`(#,(syntax-local-lift-expression
                                         #'(class-method-accessor class-name 'method-name))
                                      method-name)]))
                              (syntax-e #'(method-name ...))))]))))