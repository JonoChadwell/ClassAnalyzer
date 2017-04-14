#lang typed/racket

(require "requirement.rkt" "utilities.rkt" "types.rkt" racket/set racket/struct)

(: remaining-class-count (-> student Integer))
(define (remaining-class-count student)
  0)
