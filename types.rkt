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
   [typical-terms : (Setof Term)]
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
   [ticker : String]
   [requirements : Requirement]
   [te-calculator : (-> course-set Integer)]
   [te-needed : Integer])
  #:transparent)

;; An Integer in cal poly quarter id format:
;; 4 digits:
;;     |century - 1800|
;;     |year 10s|
;;     |year 1s|
;;     |quarter (2 for winter, 4 for spring, 6 for summer, 8 for fall)|
(define-type Quarter Integer)

(define-type Term (U 'WINTER 'SPRING 'SUMMER 'FALL))

;; represents a single student
(struct student
  ([id : String]
   [email : String]
   [major : curriculum]
   [coursework : (HashTable Quarter course-set)])
  #:transparent)

(provide (all-defined-out))
