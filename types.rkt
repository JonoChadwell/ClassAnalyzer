#lang typed/racket

;; represents a course
;; Number may include _###, which is used to represent a "variant" of a course.
;; this is done to explode requirements that use the same course twice into easier
;; to manage requirements with no double duplicated courses.
(struct course ([dept : String] [number : String]) #:transparent)

;; represents a group of courses
(define-type course-set (Setof course))

;; represents a course requirement, as e.g. a graduation requirement for a particular program
(define-type Requirement (U exactly one-of all-of))

;; represents a requirement that a student take a particular course
(struct exactly ([take : course]) #:transparent)

;; represents a requirement that a student satisfy one of a set of requirements
(struct one-of ([reqs : (Listof Requirement)]) #:transparent)

;; represents a requirement that a student satisfy all of a set of requirements
(struct all-of ([reqs : (Listof Requirement)]) #:transparent)

;; represents the full requirements for a degree
(struct curriculum
  ([name : String]
   [requirements : Requirement]
   [te-calculator : (-> course-set Integer)]
   [te-needed : Integer])
  #:transparent)

;; represents a single student
(struct student
  ([id : String]
   [major : curriculum]
   [coursework : course-set])
  #:transparent)

(provide
 exactly
 exactly?
 exactly-take
 one-of
 one-of?
 one-of-reqs
 all-of
 all-of?
 all-of-reqs
 Requirement
 course
 course-dept
 course-number
 course-set
 student
 student-id
 student-major
 student-coursework
 curriculum
 curriculum-name
 curriculum-requirements
 curriculum-te-calculator
 curriculum-te-needed)
