#lang typed/racket

(require "types.rkt")

(: quarter->term (-> Quarter Term))
(define (quarter->term qtr)
  (cond
    [(= (modulo qtr 10) 2) 'WINTER]
    [(= (modulo qtr 10) 4) 'SPRING]
    [(= (modulo qtr 10) 6) 'SUMMER]
    [(= (modulo qtr 10) 8) 'FALL]
    [else (error "Invalid quarter")]))


(: quarter-difference (-> Quarter Quarter Number))
(define (quarter-difference from to)
  (if (<= to from)
      0
      (+ 1 (quarter-difference (quarter-after from) to))))


;; given a quarter returns the next academic quarter
(: quarter-after (-> Quarter Quarter))
(define (quarter-after qtr)
  (cond
        [(equal? (quarter->term qtr) 'WINTER) (+ qtr 2)]
        [(equal? (quarter->term qtr) 'SPRING) (+ qtr 4)]
        [(equal? (quarter->term qtr) 'SUMMER) (+ qtr 2)]
        [(equal? (quarter->term qtr) 'FALL) (+ qtr 4)]
        [else (error "This should be unreachable")]))


(define current-quarter 2174)
(define next-quarter (quarter-after current-quarter))


(module+ test
  (require typed/rackunit)

  (check-equal? (quarter->term 2172) 'WINTER)
  (check-equal? (quarter->term 2154) 'SPRING)
  (check-equal? (quarter->term 2168) 'FALL)
  (check-equal? (quarter-after 2168) 2172)
  (check-equal? (quarter-difference 2168 2168) 0)
  (check-equal? (quarter-difference 2168 2172) 1)
  (check-equal? (quarter-difference 2168 2178) 3)
  (check-equal? (quarter-difference 2142 2152) 3))

(provide
 quarter->term
 quarter-after
 quarter-difference
 current-quarter
 next-quarter)
 