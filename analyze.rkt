#lang racket

(require
  racket/set
  "requirement.rkt"
  "utilities.rkt"
  "csv-reader.rkt"
  "csv-writer.rkt"
  "student.rkt"
  "types.rkt"
  "course.rkt"
  "units.rkt"
  "graduation-solver.rkt"
  "quarter.rkt"
  plot)

(define student-list (get-students-from-file "data/2174-computing-major-grade-data.csv"))

(define (get-matching-students courses req)
  (filter (lambda (x) (meets x req)) courses))

(define (course->string crs)
  (string-append (course-id-dept crs) "_" (course-id-number crs)))

#|
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
  (hash-ref (student-coursework stdnt) next-quarter (lambda () (set))))

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
|#
;; creates pairings between student id and earliest potential graduation date
(define (process-grad-info stdnt)
  (pair
   (student-id stdnt)
   (minimum-graduation-quarter (student-current-classes stdnt) (student-major stdnt) current-quarter)))

(define (get-schedule stdnt)
  (pair
   (student-id stdnt)
   (get-proposed-schedule (student-current-classes stdnt)
                          (student-major stdnt)
                          2172)))

(define (get-student emplid)
  (let ([matching-students (filter (lambda (stdnt) (equal? (student-id stdnt) emplid)) student-list)])
    (if (= (length matching-students) 1)
        (first matching-students)
        (error "Student not found"))))

(define (dotest stdnt)
  (build-course-trees 2172 (curriculum-requirements (student-major stdnt)) (student-current-classes stdnt)))

(define (check-classes stdnt)
  (pair
   (student-id stdnt)
   (let ([grad-quarter-before (minimum-graduation-quarter (student-courses-before stdnt current-quarter)
                                                          (student-major stdnt)
                                                          current-quarter)]
         [grad-quarter-after (minimum-graduation-quarter (student-courses-before stdnt next-quarter)
                                                         (student-major stdnt)
                                                         next-quarter)])
     (if (< grad-quarter-before grad-quarter-after)
         "Possible problem"
         "Okay"))))

; Turns a course into a list of strings representing info about the course
(define (stringify-course crs)
  (list
   (course-id-dept (course-identifier crs))
   (course-id-number (course-identifier crs))
   (course-name crs)
   (number->string (course-units crs))))

; Writes a bunch of courses to a file
(define (write-courses-to-file courses file-name)
  (write-csv
   file-name
   (map
    stringify-course
    courses)))

; Gets all courses within a department
(define (get-courses-from-department dept)
  (map
   right
   (filter
    (lambda (x) (equal? (course-id-dept (left x)) dept))
    (hash->list course-id-table))))

(define (write-dept-courses-to-file dept file-name)
  (write-courses-to-file (get-courses-from-department dept) file-name))

; Gets the number of students who have completed course
(define (num-completed crs)
  (length
   (filter
    (curryr set-member? (cannonicalize-course crs))
    (map (curryr student-courses-before current-quarter) student-list))))

; Gets a short description for a course
(define (get-short-description crs)
  (string-append
   (course-name crs)
   "("
   (course-id-dept (course-identifier crs))
   " "
   (course-id-number (course-identifier crs))
   ")"))

; Gets all students planning to take the given course next quarter
(define (get-all-expected-students crs)
  (filter
   (lambda (stdnt) (set-member?
                    (student-planned-classes stdnt)
                    (course-identifier crs)))
   student-list))

; Describes the format of a row in the by-course output file
(define student-rows-header
  (list
   "Student ID"
   "Student email"
   "Student major"
   "Has Prereqs"
   "Miss Cost (number of extra quarters a student will take if they don't get this course)"
   "Already Taken (Y means the student has already completed this class)"))

; Turns a student / course combo into a row in the by-course output file
(define (get-student-course-info stdnt crs)
  (list
   (student-id stdnt)
   (student-email stdnt)
   (curriculum-ticker (student-major stdnt))
   (boolean->string (meets
                     (student-current-classes stdnt)
                     (course-prereqs crs)))
   (let ([student-classes (set-union
                           (student-current-classes stdnt)
                           (student-planned-classes stdnt))])
     (number->string
      (quarter-difference
       (minimum-graduation-quarter
        student-classes (student-major stdnt) (quarter-after next-quarter))
       (minimum-graduation-quarter
        (set-remove student-classes (course-identifier crs)) (student-major stdnt) (quarter-after next-quarter)))))
   (boolean->string (set-member?
                     (student-current-classes stdnt)
                     (course-identifier crs)))))

; Gets a csv writable analysis of a course
(define (get-course-analysis crs)
  (cons
   (list "course:" (get-short-description crs))
   (cons
    student-rows-header
    (map
     (curryr get-student-course-info crs)
     (get-all-expected-students crs)))))

; Generates an analysis file for the given course
(define (generate-course-analysis-file crs)
  (let ([analysis (get-course-analysis crs)])
    (for-each
     (lambda (crs-id)
       (write-csv
        (string-append (course-id->string crs-id))
        analysis))
     (set->list (course-identifiers crs)))))

; Generates an analysis file for each course offered by a department
(define (generate-course-analysis-files dept)
  (for-each
   generate-course-analysis-file
   (get-courses-from-department dept)))

; Builds a list of lists of strings representing a schedule
; a schedule is a hash of quarter->course-set
(define (stringify-schedule schedule)
  (map
   (lambda (pr)
     (cons (number->string (left pr))
           (map
            course-id->string
            (set->list (right pr)))))
   (sort
    (hash->list schedule)
    (lambda (a b) (< (left a) (left b))))))

; Gets a csv writable analysis file for a student
(define (get-student-analysis stdnt)
  (let* ([planned-classes (set->list (student-planned-classes stdnt))]
         [coursework (student-current-classes stdnt)]
         [proposed-schedule (get-proposed-schedule
                             coursework
                             (student-major stdnt)
                             current-quarter)]
         [major (student-major stdnt)])
    (list*
     (list "Emplid:" (student-id stdnt))
     (list "Major:" (curriculum-name major))
     (list "Earliest Possible Graduation:"
           (number->string
            (max-list (cons current-quarter (hash-keys proposed-schedule)))))
     (list "Min core units needed:"
           (number->string
            (get-min-core-units coursework
                                (curriculum-requirements major))))
     (list "Tech Elective units required:"
           (number->string
            (curriculum-te-needed major)))
     (list "Tech Elective units completed:"
           (number->string
            ((curriculum-te-calculator major) coursework)))
     (cons "Completed Classes:" (map course-id->string (set->list (student-courses-before stdnt current-quarter))))
     (cons "Current Classes:" (map course-id->string (set->list (student-enrolled-classes stdnt))))
     (cons "Available Classes:" (map course-id->string
                                     (set->list
                                      (hash-ref
                                       proposed-schedule
                                       current-quarter
                                       (lambda () empty)))))
     (cons "Planned Classes:" (map course-id->string planned-classes))
     (cons "Meets Prereqs:" (map boolean->string
                                 (map (lambda (x)
                                        (let ([crs (course-id->course x)])
                                          (meets coursework (course-prereqs crs))))
                                      planned-classes)))
     (cons "Already Took:" (map boolean->string
                                (map (lambda (x)
                                       (set-member? coursework x))
                                     planned-classes)))
     (list "Proposed Schedule:")
     (stringify-schedule proposed-schedule))))

(define (generate-student-analysis-files)
  (for-each
   (lambda (stdnt)
     (write-csv 
      (student-id stdnt)
      (get-student-analysis stdnt)))
   student-list))
