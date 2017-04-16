#lang typed/racket

(require "types.rkt" "utilities.rkt")

(define current-quarter 2174)

(: accumulate (-> (HashTable Quarter (Setof course-id)) (Setof course-id)))
(define (accumulate table)
  (list->set
   (append-map
    (lambda ([x : course-set]) (set->list x))
    (map
     (lambda ([y : (Pairof Quarter course-set)]) (right y))
     (hash->list table)))))

;; gets all past and future courses
(: student-all-classes (-> student course-set))
(define (student-all-classes stdnt)
  (accumulate (student-coursework stdnt)))

;; gets the courses a student is enrolled in this quarter
(: student-enrolled-classes (-> student course-set))
(define (student-enrolled-classes stdnt)
  (accumulate
   (hash-retain-keys
    (lambda ([x : Quarter])
      (eq? x current-quarter))
    (student-coursework stdnt))))

;; gets the courses a student has completed and/or is completing
(: student-current-classes (-> student course-set))
(define (student-current-classes stdnt)
  (accumulate
   (hash-retain-keys
    (lambda ([x : Quarter])
      (<= x current-quarter))
    (student-coursework stdnt))))

;; gets the courses a student is planning on taking
(: student-planned-classes (-> student course-set))
(define (student-planned-classes stdnt)
  (accumulate
   (hash-retain-keys
    (lambda ([x : Quarter])
      (> x current-quarter))
    (student-coursework stdnt))))

;; gets the courses which (following their current plan) a student will
;; have completed before a given quarter
(: student-courses-before (-> student Quarter course-set))
(define (student-courses-before stdnt qtr)
  (accumulate
   (hash-retain-keys
    (lambda ([x : Quarter])
      (< x qtr))
    (student-coursework stdnt))))

(module+ test
  (require typed/rackunit)

  (define f101 (course-id "FAKE" "101"))
  (define f102 (course-id "FAKE" "102"))
  (define f103 (course-id "FAKE" "103"))
  (define f104 (course-id "FAKE" "104"))

  (define fake-curriculum
    (curriculum
     "Test"
     empty-requirement
     (lambda (x) 0)
     0))
  
  (define test-student-1
    (student
     "123"
     fake-curriculum
     (hash
      (- current-quarter 4) (set f101)
      (- current-quarter 2) (set f102)
      current-quarter (set f103)
      (+ current-quarter 2) (set f104))))

  (check-equal?
   (student-all-classes test-student-1)
   (set f101 f102 f103 f104))

  (check-equal?
   (student-enrolled-classes test-student-1)
   (set f103))

  (check-equal?
   (student-current-classes test-student-1)
   (set f101 f102 f103))

  (check-equal?
   (student-planned-classes test-student-1)
   (set f104))

  (check-equal?
   (student-courses-before test-student-1 (+ current-quarter 2))
   (student-current-classes test-student-1)))

(provide
 student-all-classes
 student-enrolled-classes
 student-current-classes
 student-planned-classes
 student-courses-before)
