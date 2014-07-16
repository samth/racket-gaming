#lang racket/base

(provide game-object%)

(define game-object%
  (class object%
    
    (field [body #f])
    
    (super-new)))