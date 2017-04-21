#lang typed/racket

(require racket/set "types.rkt" "utilities.rkt" "course.rkt")

(: get-num-units (-> course-id Integer))
(define (get-num-units id)
  (course-units (course-id->course id)))

(provide get-num-units)

(module+ test
  (require typed/rackunit)

  (check-equal?
   (get-num-units (course-id "CSC" "101"))
   4)
  
  (check-equal?
   (get-num-units (course-id "CE" "406"))
   5)

  (check-equal?
   (get-num-units (course-id "CE" "440"))
   4))
