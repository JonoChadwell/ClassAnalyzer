#lang racket

(require
  racket/set
  "requirement.rkt"
  "utilities.rkt"
  "civil-program.rkt"
  "csv-reader.rkt"
  "student.rkt"
  "types.rkt"
  "course.rkt"
  "units.rkt"
  "graduation-solver.rkt"
  plot)

(define quarter-to-analyze 2178)

(define student-list (get-students-from-file "data/no-names-ce-grade-data.csv" bs-civil-15-17))

(define (get-matching-students courses req)
  (filter (lambda (x) (meets x req)) courses))

(define (course->string crs)
  (string-append (course-id-dept crs) "_" (course-id-number crs)))

;; Current enrollment
(define current-enrollment-data
  (map
   (lambda (considered-course)
     (vector
      (course->string considered-course)
      (length
       (filter
        (lambda (stdnt)
          (set-member? (student-enrolled-classes stdnt) considered-course))
        student-list))))
   (hash-keys important-courses)))

;; Course completion
(define courses-completed-data
  (map
   (lambda (considered-course)
     (vector
      (course->string considered-course)
      (length
       (filter
        (lambda (stdnt)
          (set-member? (student-current-classes stdnt) considered-course))
        student-list))))
   (hash-keys important-courses)))

(define (student-courses stdnt)
  (hash-ref (student-coursework stdnt) quarter-to-analyze (lambda () (set))))

(define (student-units stdnt)
  (let ([classes (student-courses stdnt)])
        (sum-list (map get-num-units (set->list classes)))))

;; Student units taken
(define student-units-data
  (map
   (lambda (stdnt)
     (list
      (student-id stdnt)
      (student-units stdnt)
      (student-courses stdnt)))
   (filter
    (lambda (stdnt)
      (> (student-units stdnt) 16))
    student-list)))

(define (print-student-units-data)
  (for-each
   (lambda (lst)
     (display (string-append (first lst) ", " (number->string (second lst)) ", "))
     (for-each
      (lambda (crs)
        (display (string-append (course-id->string crs) ", ")))
      (set->list (third lst)))
     (displayln ""))
   student-units-data))

(define (get-course-counts-recursive courses)
  (if (empty? courses)
      (hash)
      (let* ([table (get-course-counts-recursive (rest courses))]
             [crs (first courses)])
        (hash-set
         table
         crs
         (+ (hash-ref table crs (lambda () 0)) 1)))))

;; returns the number of times a student is planning to take each course
(define (get-course-counts stdnt)
  (let* ([classes (student-coursework stdnt)]
         [quarterlist (hash->list classes)]
         [course-set-list (map right quarterlist)]
         [class-list (append-map set->list course-set-list)])
    (get-course-counts-recursive class-list)))

;; gets all courses a student's record shows in more than one quarter
(define (get-duplicated-courses stdnt)
  (hash-keys
   (hash-retain-values
    (lambda (x) (> x 1))
    (get-course-counts stdnt))))

;; student duplicated course planning
(define duplicated-course-data
  (filter
   (lambda (pr)
     (not (empty? (right pr))))
   (map
    (lambda (stdnt) (pair stdnt (get-duplicated-courses stdnt)))
    student-list)))

;; student past and future courses
(define planned-and-taken-data
  (filter
   (lambda (pr)
     (not (set-empty? (right pr))))
   (map
    (lambda (stdnt) (pair stdnt (set-intersect
                                 (student-current-classes stdnt)
                                 (student-planned-classes stdnt))))
    student-list)))

(define (print-planned-and-taken-data)
  (for-each
   (lambda (pr)
     (display (string-append (student-id (left pr)) ", "))
     (for-each
      (lambda (crs)
        (display (string-append (course-id->string crs) ", ")))
      (set->list (right pr)))
     (displayln ""))
   planned-and-taken-data))

(define plot-data courses-completed-data)

(define (do-plot)
  (plot (discrete-histogram
         plot-data)))

(plot-title "Civil Engineering Core Course Need")
(plot-x-label "Course")
(plot-y-label "Number of Students")
(plot-width 800)

;; (do-plot)

;; creates pairings between student id and earliest potential graduation date
(define (process-grad-info stdnt)
  (pair
   (student-id stdnt)
   (minimum-graduation-quarter (student-current-classes stdnt) (student-major stdnt) 2172)))
