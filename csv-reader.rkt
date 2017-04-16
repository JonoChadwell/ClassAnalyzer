#lang racket

(require csv-reading "types.rkt" "utilities.rkt" "course.rkt")

(define (id datum)
  (first datum))

(define (class datum)
  (cannonicalize-course
   (let* ([class (fourth datum)]
          [parts (string-split class)]
          [name (first parts)]
          [number (second parts)])
     (course-id name number))))

(define (quarter datum)
  (string->number (third datum)))

(define (read-student-file file-name)
  (csv-map identity (open-input-file file-name)))

(define (build-student-course-tables all-data)
  (if (empty? all-data)
      (hash)
      (let* ([datum (first all-data)]
             [student-id (id datum)]
             [crs (class datum)]
             [qtr (quarter datum)]
             [remaining (build-student-course-tables (rest all-data))]
             [student-table (hash-ref remaining student-id (lambda () (hash)))]
             [student-set (hash-ref student-table qtr (lambda () (set)))])
        (hash-set remaining student-id (hash-set student-table qtr (set-add student-set crs))))))

(define (build-students course-tables degree)
  (map
   (lambda (student-table-pair)
     (student
      (left student-table-pair)
      degree
      (right student-table-pair)))
   (hash->list course-tables)))

(define (get-students-from-file file-name degree)
  (build-students (read-student-file file-name) degree))

(module+ test
  (require rackunit)

  (define fake-curriculum
    (curriculum
     "Test"
     empty-requirement
     (lambda (x) 0)
     0))

  (define test-data
    (list
     '("001231123" "" "2168" "CSC 101" "A")
     '("001231123" "" "2172" "CSC 102" "A")
     '("001231123" "" "2174" "CSC 103" "A")
     '("001231123" "" "2174" "CSC 141" "A")
     '("001231123" "" "2178" "CSC 357" "A")
     '("004565432" "" "2162" "CSC 101" "A")
     '("004565432" "" "2164" "CSC 102" "A")
     '("004565432" "" "2168" "CSC 103" "A")))

  (check-equal?
   (map id test-data)
   (list "001231123" "001231123" "001231123" "001231123" "001231123"
         "004565432" "004565432" "004565432"))

  (check-equal?
   (map quarter test-data)
   (list 2168 2172 2174 2174 2178 2162 2164 2168))

  (check-equal?
   (map class test-data)
   (list (course-id "CSC" "101")
         (course-id "CSC" "102")
         (course-id "CSC" "103")
         (course-id "CSC" "141")
         (course-id "CSC" "357")
         (course-id "CSC" "101")
         (course-id "CSC" "102")
         (course-id "CSC" "103")))

  (check-equal?
   (build-student-course-tables test-data)
   (hash
    "001231123"
    (hash
     2168 (set (course-id "CSC" "101"))
     2172 (set (course-id "CSC" "102"))
     2174 (set (course-id "CSC" "103") (course-id "CSC" "141"))
     2178 (set (course-id "CSC" "357")))
    "004565432"
    (hash
     2162 (set (course-id "CSC" "101"))
     2164 (set (course-id "CSC" "102"))
     2168 (set (course-id "CSC" "103")))))

  (check-equal?
   (list->set (build-students (build-student-course-tables test-data) fake-curriculum))
   (set
    (student
     "001231123"
     fake-curriculum
     (hash
      2168 (set (course-id "CSC" "101"))
      2172 (set (course-id "CSC" "102"))
      2174 (set (course-id "CSC" "103") (course-id "CSC" "141"))
      2178 (set (course-id "CSC" "357"))))
    (student
     "004565432"
     fake-curriculum
     (hash
      2162 (set (course-id "CSC" "101"))
      2164 (set (course-id "CSC" "102"))
      2168 (set (course-id "CSC" "103")))))))

(provide get-students-from-file)
