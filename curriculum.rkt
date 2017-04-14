#lang typed/racket

(require "requirement.rkt" "utilities.rkt" "types.rkt" racket/set racket/struct)

;; Returns true when a class would progress a student towards graduation
(: helps-student? (-> student course Boolean))
(define (helps-student? stdnt crs)
  (or
   (let* ([major (student-major stdnt)]
          [checker (curriculum-te-calculator major)]
          [te-units-needed (curriculum-te-needed major)]
          [courses (student-coursework stdnt)]
          [current-te-units (checker courses)])
     (and
      (<
       current-te-units
       te-units-needed)
      (<
       current-te-units
       (checker (set-add courses crs)))))
   (helps-student-core stdnt crs)))

(provide
 helps-student?)
