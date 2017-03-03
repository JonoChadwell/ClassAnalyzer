#lang racket

(require "start.rkt" "ge-areas.rkt")

;; 2015-17 catalog.
;; Data from http://catalog.calpoly.edu/collegesandprograms/collegeofengineering/civilenvironmentalengineering/bscivilengineering/

;; Currently, technical elective requirements are not included

(define tech-elective
  

(define bs-civil-15-17
  (all-of
   (list
    ;; senior project
    (one-of (list
             (all-of (list
                      (exactly (course "CE" "466"))
                      (exactly (course "CE" "467"))))
             (all-of (list
                      (exactly (course "CE" "468"))
                      (exactly (course "CE" "469"))))))

    ;; core classes
    (group-all
     "CE" "111"
     "CE" "112"
     "CE" "113"
     "CE" "204"
     "CE" "207"
     "CE" "251"
     "CE" "259"
     "CE" "321"
     "CE" "322"
     "CE" "336"
     "CE" "337"
     "CE" "352"
     "CE" "355"
     "CE" "381"
     "CE" "382"
     "CE" "465")

    ;; engineering science elective
    (group-any
     "CSC" "231"
     "CSC" "234"
     "CSC" "341"
     "EE" "201"
     "IME" "314"
     "MATH" "304"
     "MATH" "344")

    ;; support classes
    (one-of (list
             (exactly (course "BMED" "213"))
             (exactly (course "BRAE" "213"))))
    (group-all
     "BIO" "213"
     "BRAE" "239"
     "CHEM" "124"
     "CHEM" "125"
     "ENGL" "149"
     "ENVE" "331"
     "GEOL" "201"
     "MATE" "210"
     "MATE" "215"
     "MATH" "141"
     "MATH" "142"
     "MATH" "143"
     "MATH" "241"
     "MATH" "244"
     "ME" "211"
     "ME" "212"
     "ME" "302"
     "ME" "341"
     "PHYS" "141"
     "PHYS" "132"
     "PHYS" "133"
     "STAT" "312")

    ;; general electives
    (all-of (list
             A1
             A2
             C1
             C2
             C3
             C4
             D1
             D2
             D3
             D4)))))
