#lang racket

(require racket/set "types.rkt" "utilities.rkt")

(define unit-counts
  (hash
   (course-id "ARCE" "305") 2
   (course-id "ARCE" "372") 3
   (course-id "ARCE" "403") 3
   (course-id "BRAE" "345") 3
   (course-id "CHEM" "341") 3
   (course-id "CM" "334") 2
   (course-id "CM" "432") 3
   (course-id "CRP" "404") 3
   (course-id "CRP" "408") 3
   (course-id "IME" "314") 3
   (course-id "SS" "423") 5
   (course-id "ARCE" "372") 3
   (course-id "ARCE" "372") 3
   (course-id "ARCE" "372") 3
   (course-id "ARCE" "372") 3
   (course-id "ARCE" "372") 3
   (course-id "CE" "400") 2 ;; assuming max
   (course-id "CE" "406") 5
   (course-id "CE" "413") 2
   (course-id "CE" "470") 4 ;; assuming max
   (course-id "CE" "474") 2
   (course-id "CE" "493") 2
   (course-id "CE" "494") 6
   (course-id "CE" "495") 12
   (course-id "CE" "500") 3 ;; assuming max
   (course-id "CE" "555") 2
   (course-id "CE" "570") 4 ;; assuming max
   (course-id "CE" "571") 4 ;; assuming max
   (course-id "CE" "591") 1
   (course-id "CE" "592") 1
   (course-id "CE" "593") 2
   (course-id "CE" "594") 6
   (course-id "CE" "595") 12
   (course-id "CE" "596") 1
   (course-id "CE" "599") 12 ;; assuming max
   ))

(define (get-num-units crs)

  (hash-ref unit-counts crs (lambda () 4)))

(module+ test
  (require rackunit)
  
  (check-equal?
   (get-num-units (course-id "CE" "406"))
   5)

  (check-equal?
   (get-num-units (course-id "CE" "444"))
   4))

(provide unit-counts get-num-units)
