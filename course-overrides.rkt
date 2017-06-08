#lang racket

(require "requirement.rkt" "utilities.rkt" "types.rkt")

(define prereq-override-map
  (hash
   ;; For CSC program
   (course-id "CSC" "357") (all-of (list (exactly (course-id "CSC" "203")) (group-any "CSC" "225" "CPE" "233")))
   ;; For Civil program
   (course-id "CSC" "357") (all-of (list (group-any "CSC" "225" "CPE" "233") (exactly (course-id "CSC" "203"))))
   (course-id "CPE" "431") (exactly (course-id "CSC" "430"))
   (course-id "CSC" "491") (group-any "CSC" "307" "CSC" "309")
   (course-id "ME" "211") (group-all "PHYS" "141" "MATH" "143")
   (course-id "CE" "111") (all-of empty)
   (course-id "CE" "112") (all-of empty) ;; leaving out calc 1 requirement because we don't have data for AP high school credit
   (course-id "CE" "113") (all-of empty)
   (course-id "CE" "204") (exactly (course-id "ME" "211"))
   (course-id "CE" "207") (exactly (course-id "CE" "204"))
   (course-id "CE" "251") (group-all "CE" "113" "CE" "204" "MATH" "244")
   (course-id "CE" "259") (exactly (course-id "CE" "204"))
   (course-id "CE" "321") (exactly (course-id "CE" "259"))
   (course-id "CE" "336") (group-any "ME" "341" "ENVE" "264")
   (course-id "CE" "352") (exactly (course-id "CE" "207"))
   (course-id "CE" "355") (all-of (list (exactly (course-id "CE" "259")) (group-any "CE" "351" "CE" "352")))
   (course-id "CE" "381") (all-of (list (exactly (course-id "CE" "207")) (group-any "ME" "341" "ENVE" "264")))))

;; If a course is not listed in the catalog, it will check here to see if there is an alternate name for
;; the course. (Some of these should probably be in the requirements for various majors instead of here).
(define course-aliases
  (hash
   (course-id "STAT" "321") (course-id "STAT" "312")
   (course-id "CPE" "101") (course-id "CSC" "101")
   (course-id "CPE" "102") (course-id "CSC" "202")
   (course-id "CPE" "103") (course-id "CSC" "203")
   (course-id "CSC" "103") (course-id "CSC" "203")
   (course-id "CSC" "141") (course-id "CSC" "348")
   (course-id "CPE" "225") (course-id "CSC" "225")
   (course-id "CPE" "300") (course-id "CSC" "300")
   (course-id "CPE" "307") (course-id "CSC" "307")
   (course-id "CSC" "315") (course-id "CPE" "315")
   (course-id "CPE" "349") (course-id "CSC" "349")
   (course-id "CPE" "430") (course-id "CSC" "430")))
  

(provide prereq-override-map course-aliases)
