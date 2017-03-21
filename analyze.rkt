#lang racket

(require racket/set "start.rkt" "utilities.rkt" "civil-program.rkt")

(define input_file (open-input-file "data.txt" #:mode'text))
(define output_file (open-output-file "out.txt" #:mode'text #:exists'replace))

(define (get-student-hash lst)
  (if (empty? lst)
      (hash)
      (let* ([student-id (first (first lst))]
             [crs (course (second (first lst)) (third (first lst)))]
             [remaining (get-student-hash (rest lst))]
             [student-set (hash-ref remaining student-id (lambda () (set)))])
        (hash-set remaining student-id (set-add student-set crs)))))

(define raw-input (read input_file))
(define student-hash (get-student-hash raw-input))

(define student-class-counts
  (map
   (lambda (courses)
     (get-remaining-count courses bs-civil-15-17))
   (hash-values student-hash)))

(for/list ([i student-class-counts])
  (writeln  i output_file))

(close-output-port output_file)
(close-input-port input_file)
