#lang typed/racket

(require "types.rkt")

(: get-first-alphabetically (-> course-id course-id course-id))
(define (get-first-alphabetically a b)
  (if (string-ci<? (course-id-dept a) (course-id-dept b))
      a
      b))

;; gets the cannonical id of a course
(: course-identifier (-> course course-id))
(define (course-identifier crs)
  (let ([lst (set->list (course-identifiers crs))])
    (if (empty? lst)
        (error "Course has no attached identifiers")
        (foldl get-first-alphabetically (first lst) (rest lst)))))

;; gets the cannonical department of a course
(: course-dept (-> course String))
(define (course-dept crs)
  (course-id-dept (course-identifier crs)))

;; gets the cannonical number of a course
(: course-number (-> course String))
(define (course-number crs)
  (course-id-number (course-identifier crs)))

;; Given a course identifier returns the cannonical identifier for the course
;; referenced by that identifier
(: cannonicalize-course (-> course-id course-id))
(define cannonicalize-course
  ;; TODO
  identity)

;; Gets the full course object from any of its identifiers
(: get-course (-> course-id course))
(define (get-course id)
  ;; TODO
  (error "Not yet implemented"))
 
(module+ test
  (require typed/rackunit)

  (define test-id-1 (course-id "CPE" "357"))
  (define test-id-2 (course-id "CSC" "357"))

  (define test-course-1
    (course
     (set test-id-1 test-id-2)
     4
     "Systems Programming"
     (all-of (list
              (exactly (course-id "CPE" "103"))
              (one-of (list
                       (exactly (course-id "CPE" "225"))
                       (exactly (course-id "CPE" "233"))))))))

  (check-equal?
   (get-first-alphabetically test-id-1 test-id-2)
   test-id-1)

  (check-equal?
   (course-number test-course-1)
   "357")

  (check-equal?
   (course-dept test-course-1)
   "CPE"))

(provide
 course-identifier
 course-dept
 course-number
 get-course
 cannonicalize-course)
