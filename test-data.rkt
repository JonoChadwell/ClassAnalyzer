#lang typed/racket

(require "utilities.rkt" "types.rkt" "course.rkt" "ge-areas.rkt" "requirement.rkt")

(define FAKE_101 (course-id "FAKE" "101"))
(define FAKE_102 (course-id "FAKE" "102"))
(define FAKE_151 (course-id "FAKE" "151"))
(define FAKE_250 (course-id "FAKE" "250"))
  
(hash-set! course-id-table
           FAKE_101
           (course
            (set FAKE_101)
            (set 'FALL 'WINTER 'SPRING)
            4
            "Introduction to Faking it"
            empty-requirement))

(hash-set! course-id-table
           FAKE_102
           (course
            (set FAKE_102)
            (set 'FALL 'WINTER 'SPRING)
            4
            "Faking it better"
            (exactly FAKE_101)))

(hash-set! course-id-table
           FAKE_151
           (course
            (set FAKE_151)
            (set 'WINTER)
            4
            "Faking it in the cold"
            (exactly FAKE_101)))

(hash-set! course-id-table
           FAKE_250
           (course
            (set FAKE_250)
            (set 'WINTER)
            4
            "Meta deception"
            (group-any "FAKE" "102" "FAKE" "151")))

(define FAKE_ENGINEERING_REQ
  (all-of
   (list
    (group-all
     "FAKE" "101"
     "FAKE" "102"
     "FAKE" "151"
     "FAKE" "250")
    D1
    D2)))

(define FAKE_ENGINEERING
  (curriculum
   "Phony Engineering"
   "PHE"
   FAKE_ENGINEERING_REQ
   (lambda ([x : course-set])
     (sum-int-list
      (map
       (lambda ([x : course-id])
         (if (< (cast (string->number (course-id-number x)) Real) 300)
             0
             (course-units (course-id->course x))))
       (set->list x))))
   12))

(define FAKE_STUDENT_1
  (student
   "12344321"
   "fake_student@capoly.edu"
   FAKE_ENGINEERING
   (hash
    2002 (set FAKE_101)
    2004 (set FAKE_102)
    2008 (set FAKE_151))))

(provide FAKE_101
         FAKE_102
         FAKE_151
         FAKE_250
         FAKE_ENGINEERING_REQ
         FAKE_ENGINEERING
         FAKE_STUDENT_1)