#lang racket

(require csv-reading rackunit "start.rkt" "utilities.rkt")

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

(define (id datum)
  (first datum))

(check-equal?
 (map id test-data)
 (list "001231123" "001231123" "001231123" "001231123" "001231123"
       "004565432" "004565432" "004565432"))

(define (class datum)
  (let* ([class (fourth datum)]
         [parts (string-split class)]
         [name (first parts)]
         [number (second parts)])
    (course name number)))

(check-equal?
 (map class test-data)
 (list (course "CSC" "101")
       (course "CSC" "102")
       (course "CSC" "103")
       (course "CSC" "141")
       (course "CSC" "357")
       (course "CSC" "101")
       (course "CSC" "102")
       (course "CSC" "103")))

(define (read-student-file file-name)
  (csv-map identity (open-input-file file-name)))

(define (build-students all-data)
  (if (empty? all-data)
      (hash)
      (let* ([datum (first all-data)]
             [student-id (id datum)]
             [crs (class datum)]
             [remaining (build-students (rest all-data))]
             [student-set (hash-ref remaining student-id (lambda () (set)))])
        (hash-set remaining student-id (set-add student-set crs)))))

(check-equal?
 (build-students test-data)
 (hash
  "001231123"
  (set
   (course "CSC" "101")
   (course "CSC" "102")
   (course "CSC" "103")
   (course "CSC" "141")
   (course "CSC" "357"))
  "004565432"
  (set
   (course "CSC" "101")
   (course "CSC" "102")
   (course "CSC" "103"))))

(define (read-csv-file file-name)
  (build-students (read-student-file file-name)))

(provide read-csv-file)