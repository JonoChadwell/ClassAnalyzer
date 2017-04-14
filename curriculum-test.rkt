#lang racket

(require
  racket/set
  rackunit
  "types.rkt"
  "curriculum.rkt"
  "utilities.rkt"
  "requirement.rkt"
  "units.rkt")

(define fake-curriculum
  (curriculum
   "Fake Degree"
   (all-of
    (list
     (group-all
      "FAKE" "101"
      "FAKE" "102"
      "FAKE" "103")
     (group-any
      "FAKE" "110"
      "FAKE" "111")
     (group-any
      "FAKE" "150"
      "FAKE" "151")))
   (lambda (courses)
     (sum-list
      (map
       get-num-units
       (filter
        (lambda (crs) (and
                       (> (string->number (course-number crs)) 200)
                       (eq? (course-dept crs) "FAKE")))
        (set->list courses)))))
   16))

(define test-student-fresh
  (student
   "00000001"
   fake-curriculum
   (set
    (course "FAKE" "101")
    (course "FAKE" "102"))))

(define test-student-halfway
  (student
   "00000002"
   fake-curriculum
   (set
    (course "FAKE" "101")
    (course "FAKE" "102")
    (course "FAKE" "103")
    (course "FAKE" "110")
    (course "FAKE" "215")
    (course "FAKE" "220"))))

(define test-student-missing-te
   (student
   "00000002"
   fake-curriculum
   (set
    (course "FAKE" "101")
    (course "FAKE" "102")
    (course "FAKE" "103")
    (course "FAKE" "110")
    (course "FAKE" "151")
    (course "FAKE" "215")
    (course "FAKE" "220")
    (course "FAKE" "230"))))


(define test-student-missing-core
   (student
   "00000003"
   fake-curriculum
   (set
    (course "FAKE" "101")
    (course "FAKE" "102")
    (course "FAKE" "103")
    (course "FAKE" "110")
    (course "FAKE" "215")
    (course "FAKE" "220")
    (course "FAKE" "230")
    (course "FAKE" "300"))))

(define test-student-done
   (student
   "00000004"
   fake-curriculum
   (set
    (course "FAKE" "101")
    (course "FAKE" "102")
    (course "FAKE" "103")
    (course "FAKE" "111")
    (course "FAKE" "150")
    (course "FAKE" "215")
    (course "FAKE" "220")
    (course "FAKE" "230")
    (course "FAKE" "300"))))

(check-true
 (helps-student? test-student-fresh (course "FAKE" "103")))

(check-false
 (helps-student? test-student-fresh (course "FAKE" "101")))

(check-true
 (helps-student? test-student-halfway (course "FAKE" "280")))

(check-true
 (helps-student? test-student-missing-te (course "FAKE" "280")))

(check-false
 (helps-student? test-student-missing-core (course "FAKE" "280")))
