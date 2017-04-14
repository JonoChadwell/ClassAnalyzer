#lang racket

(require racket/set "start.rkt" "utilities.rkt" "civil-program.rkt" "csv-reader.rkt" plot)

(define student-hash (read-csv-file "data/no-names-ce-grade-data.csv"))

(define student-class-counts
  (map
   (lambda (courses)
     (get-remaining-count courses bs-civil-15-17))
   (hash-values student-hash)))

(define (get-matching-students courses req)
  (filter (lambda (x) (meets x req)) courses))

(define (course->string crs)
  (string-append (course-dept crs) "_" (course-number crs)))

(define plot-data
  (map
   (lambda (pr)
     (vector
      (course->string (left pr))
      (length
       (filter
        (lambda (student) (helps-student (right student) bs-civil-15-17 (left pr)))
        (filter
         (lambda (student)
           (and
            (meets (right student) (right pr))
            (not (set-member? (right student) (left pr)))))
         (hash->list student-hash))))))
   (hash->list important-courses)))

(define (do-plot)
  (plot (discrete-histogram
         plot-data)))

(plot-title "Civil Engineering Core Course Need")
(plot-x-label "Course")
(plot-y-label "Number of Students")
(plot-width 800)

(do-plot)
