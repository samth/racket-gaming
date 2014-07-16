#lang racket/base

(require "shared.rkt")

(provide push-button%)

(define push-button%
  (class object%
    (field [press (new event-handler%)]
           [release (new event-handler%)])
    (super-new)))