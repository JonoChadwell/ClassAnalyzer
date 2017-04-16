#lang racket

(require racket/set "requirement.rkt" "utilities.rkt" "civil-program.rkt" "csv-reader.rkt" "student.rkt" "types.rkt" plot)

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

(define plot-data courses-completed-data)

(define (do-plot)
  (plot (discrete-histogram
         plot-data)))

(plot-title "Civil Engineering Core Course Need")
(plot-x-label "Course")
(plot-y-label "Number of Students")
(plot-width 800)

(do-plot)
