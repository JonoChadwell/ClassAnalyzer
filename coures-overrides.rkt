#lang racket

(require "requirement.rkt" "utilities.rkt" "types.rkt")

(define prereq-override-map
  (hash
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

(provide prereq-override-map)
