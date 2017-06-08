#lang typed/racket

(require "types.rkt" "utilities.rkt" "course.rkt" )

(require/typed
 csv-reading
 [csv-map (All (A) (-> (-> (Listof String) A) Input-Port (Listof A)))])

(require/typed
  "majors.rkt"
 [majors (HashTable String curriculum)])

;; reading students from CSV files expects the following format:
;; MAJOR, EMPLID, EMAIL, CONCENTRATION, TERM, COURSE, ...
;; MAJOR - the 'ticker' for the students major (IE 'CSC' for computer science
;; EMPLID - the students EMPLID
;; EMAIL - the students email
;; CONCENTRATION - not currently parsed
;; TERM - the term a course was taken
;; COURSE - the course taken (expects ticker + number, IE 'CSC 101')
;; any additional fields will be ignored

;; Gets the student's major from a row
(: major (-> (Listof String) String))
(define (major datum)
  ;; TODO: figure out why unicode char 0xFEFF ends up in some major names
  (string-replace (first datum) "\uFEFF" ""))

;; Gets the student id from a row
(: id (-> (Listof String) String))
(define (id datum)
  (second datum))

;; Gets the student's email from a row
(: email (-> (Listof String) String))
(define (email datum)
  (third datum))

;; Gets the quarter the course was taken in from a row
(: term (-> (Listof String) Quarter))
(define (term datum)
  (string->integer (fifth datum)))

;; Gets the course-id of the course taken from a row
(: class (-> (Listof String) course-id))
(define (class datum)
  (cannonicalize-course
   (let* ([class (sixth datum)]
          [parts (string-split class)]
          [name (first parts)]
          [number (second parts)])
     (course-id name number))))

;; Gets a list of rows from a CSV file
(: read-student-file (-> String (Listof (Listof String))))
(define (read-student-file file-name)
  (csv-map (lambda ([x : (Listof String)]) x) (open-input-file file-name)))

;; Builds a map from emplid to student stucts from raw data.
(: build-students (-> (Listof (Listof String)) (HashTable String curriculum) (HashTable String student)))
(define (build-students raw-data major-map)
  (if (empty? raw-data)
      (hash)
      (let* ([datum (first raw-data)]
             [table (build-students (rest raw-data) major-map)]
             [stdnt-major (hash-ref major-map (major datum)
                                    (lambda ()
                                      (error (string-append "Unknown major: \"" (major datum) "\""))))]
             [stdnt-id (id datum)]
             [stdnt-email (email datum)]
             [qtr (term datum)]
             [crs-id (class datum)])
        (hash-set
         table
         stdnt-id
         (if (hash-has-key? table stdnt-id)
             (let ([old-student (hash-ref table stdnt-id (lambda () (error "This should never happen")))])
               (if (not (equal? (student-major old-student) stdnt-major))
                   (error (string-append "Mismatched majors on student " stdnt-id))
                   (if (not (equal? (student-email old-student) stdnt-email))
                       (error (string-append "Mismatched emails on student " stdnt-id))
                       (student
                        stdnt-id
                        stdnt-email
                        stdnt-major
                        (hash-set
                         (student-coursework old-student)
                         qtr
                         (set-add (hash-ref (student-coursework old-student)
                                            qtr
                                            (lambda () (cast (set) (Setof course-id))))
                                  crs-id))))))
             (student
              stdnt-id
              stdnt-email
              stdnt-major
              (hash qtr (set crs-id))))))))

;; Gets all the students from a CSV file
(: get-students-from-file (-> String (Listof student)))
(define (get-students-from-file file-name)
  (let ([raw-data (read-student-file file-name)])
    (hash-values (build-students raw-data majors))))

(module+ test
  (require typed/rackunit "test-data.rkt")

  (define test-data
    (list
     '("FAKE" "001231123" "dummy1@capoly.edu" "" "2168" "FAKE 101")
     '("FAKE" "001231123" "dummy1@capoly.edu" "" "2172" "FAKE 102")
     '("FAKE" "001231123" "dummy1@capoly.edu" "" "2174" "FAKE 151")
     '("FAKE" "001231123" "dummy1@capoly.edu" "" "2174" "FAKE 250")
     '("FAKE" "001231123" "dummy1@capoly.edu" "" "2178" "FAKE 400")
     '("FAKE" "004565432" "dummy2@capoly.edu" "" "2162" "FAKE 101")
     '("FAKE" "004565432" "dummy2@capoly.edu" "" "2164" "FAKE 102")
     '("FAKE" "004565432" "dummy2@capoly.edu" "" "2168" "FAKE 151")))

  (check-equal?
   (map id test-data)
   (list "001231123" "001231123" "001231123" "001231123" "001231123"
         "004565432" "004565432" "004565432"))

  (check-equal?
   (map term test-data)
   (list 2168 2172 2174 2174 2178 2162 2164 2168))

  (check-equal?
   (map class test-data)
   (list (course-id "FAKE" "101")
         (course-id "FAKE" "102")
         (course-id "FAKE" "151")
         (course-id "FAKE" "250")
         (course-id "FAKE" "400")
         (course-id "FAKE" "101")
         (course-id "FAKE" "102")
         (course-id "FAKE" "151")))

  ;; TODO: this returns a error citing "non-chaperone result" when using check-equal? instead
  ;; of check-true on equal? results. Investigate / file bug report if needed.
  (check-true
   (equal?
    (build-students test-data (hash "FAKE" FAKE_ENGINEERING))
    (hash
     "001231123"
     (student
      "001231123"
      "dummy1@capoly.edu"
      FAKE_ENGINEERING
      (hash
       2168 (set (course-id "FAKE" "101"))
       2172 (set (course-id "FAKE" "102"))
       2174 (set (course-id "FAKE" "151") (course-id "FAKE" "250"))
       2178 (set (course-id "FAKE" "400"))))
     "004565432"
     (student
      "004565432"
      "dummy2@capoly.edu"
      FAKE_ENGINEERING
      (hash
       2162 (set (course-id "FAKE" "101"))
       2164 (set (course-id "FAKE" "102"))
       2168 (set (course-id "FAKE" "151"))))))))

(provide get-students-from-file)
