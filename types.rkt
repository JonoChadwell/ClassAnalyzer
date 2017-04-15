#lang typed/racket

;; Represents an identifier which a course goes by. Crosslisted courses may have
;; several such identifiers.
(struct course-id
  ([dept : String]
   [number : String])
  #:transparent)

;; When a single cannonical name is needed for a crosslisted class the name and
;; class number from the alphabetically first department is used. For courses
;; which offer a range of units, units is assumed to be the maximal value
;; (for now).
(struct course
  ([identifiers : (Setof course-id)]
   [units : Integer]
   [name : String]
   [prereqs : Requirement])
  #:transparent)

;; represents a group of courses
(define-type course-set (Setof course-id))

;; represents a course requirement, as e.g. a graduation requirement for a particular program
(define-type Requirement (U exactly one-of all-of))

;; represents a requirement that a student take a particular course
(struct exactly ([take : course-id]) #:transparent)

;; represents a requirement that a student satisfy one of a set of requirements
(struct one-of ([reqs : (Listof Requirement)]) #:transparent)

;; represents a requirement that a student satisfy all of a set of requirements
(struct all-of ([reqs : (Listof Requirement)]) #:transparent)

(define empty-requirement (all-of empty))

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
 empty-requirement
 course-id
 course-id-dept
 course-id-number
 course
 course-identifiers
 course-units
 course-name
 course-prereqs
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
