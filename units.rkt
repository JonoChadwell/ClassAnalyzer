#lang typed/racket

(require racket/set "types.rkt" "utilities.rkt" "course.rkt" "requirement.rkt")

(: get-num-units (-> course-id Integer))
(define (get-num-units crs)
  (course-units (course-id->course crs)))

(: get-min-core-units (-> course-set Requirement Integer))
(define (get-min-core-units courses req)
  (get-minimum-cost-to-satisfy
   get-num-units
   courses
   req))

(provide
 get-num-units
 get-min-core-units)

(module+ test
  (require typed/rackunit "test-data.rkt")

  (check-equal?
   (get-num-units FAKE_101)
   4)
  
  (check-equal?
   (get-num-units FAKE_102)
   4)

  (check-equal?
   (get-num-units FAKE_250)
   4)

  (check-equal?
   (get-min-core-units (set FAKE_101) FAKE_ENGINEERING_REQ)
   20))
