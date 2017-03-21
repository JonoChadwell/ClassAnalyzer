#lang racket

(require racket/set "start.rkt" "ge-areas.rkt")

;; 2015-17 catalog.
;; Data from http://catalog.calpoly.edu/collegesandprograms/collegeofengineering/civilenvironmentalengineering/bscivilengineering/

;; Currently, technical elective requirements are not included

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

(define (valid-te crs)
  (or
   (member
    crs
    (list
     (course "CE" "356")
     (course "CE" "371")
     (course "CM" "371")
     (course "ENVE" "325")))
   (and (or (eq? (course-dept crs) "ENVE")
            (eq? (course-dept crs) "CE"))
        (and (> (string->number (course-number crs)) 400)
             (not
              (member
               crs
               (list
                (course "CE" "465")
                (course "CE" "466")
                (course "CE" "467")
                (course "CE" "468")
                (course "CE" "469")
                (course "CE" "404"))))))))

(define te-four-unit-max
  (list
   (course "ARCE" "305")
   (course "ARCE" "372")
   (course "ARCE" "403")
   (course "BIO" "421")
   (course "NR" "421")
   (course "SS" "421")
   (course "BMED" "404")
   (course "CE" "404")
   (course "ME" "404")
   (course "BRAE" "345")
   (course "BRAE" "447")
   (course "BRAE" "532")
   (course "CHEM" "341")
   (course "CM" "334")
   (course "CM" "432")
   (course "CRP" "420")
   (course "CRP" "435")
   (course "CRP" "404")
   (course "NR" "404")
   (course "CRP" "408")
   (course "NR" "408")
   (course "ERSC" "401")
   (course "GEOL" "401")
   (course "ERSC" "402")
   (course "GEOL" "402")
   (course "GEOL" "415")
   (course "IME" "314")
   (course "MATE" "425")
   (course "MATE" "450")
   (course "MATH" "344")
   (course "SS" "423")
   (course "SS" "442")))

(provide bs-civil-15-17)