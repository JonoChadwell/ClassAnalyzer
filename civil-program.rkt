#lang racket

(require racket/set "requirement.rkt" "types.rkt" "ge-areas.rkt" "utilities.rkt" "units.rkt")

;; 2015-17 catalog.
;; Data from http://catalog.calpoly.edu/collegesandprograms/collegeofengineering/civilenvironmentalengineering/bscivilengineering/

(define bs-civil-15-17-main-coursework
  (all-of
   (list
    ;; senior project
    (one-of (list
             (all-of (list
                      (exactly (course-id "CE" "466"))
                      (exactly (course-id "CE" "467"))))
             (all-of (list
                      (exactly (course-id "CE" "468"))
                      (exactly (course-id "CE" "469"))))))

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
             (exactly (course-id "BMED" "213"))
             (exactly (course-id "BRAE" "213"))))
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

;; checks whether a course is a valid core tech-elective
(define (valid-te crs)
  (or
   (list-contains
    crs
    (list
     (course-id "CE" "356")
     (course-id "CE" "371")
     (course-id "CM" "371")
     (course-id "ENVE" "325")))
   (and (or (eq? (course-id-dept crs) "ENVE")
            (eq? (course-id-dept crs) "CE"))
        (and (> (string->number (course-id-number crs)) 400)
             (not
              (list-contains
               crs
               (list
                (course-id "CE" "465")
                (course-id "CE" "466")
                (course-id "CE" "467")
                (course-id "CE" "468")
                (course-id "CE" "469")
                (course-id "CE" "404"))))))))

;; CE students can get credit for up to 4 units from these courses
(define te-four-unit-max
  (list
   (course-id "ARCE" "305")
   (course-id "ARCE" "372")
   (course-id "ARCE" "403")
   (course-id "BIO" "421")
   (course-id "NR" "421")
   (course-id "SS" "421")
   (course-id "BMED" "404")
   (course-id "CE" "404")
   (course-id "ME" "404")
   (course-id "BRAE" "345")
   (course-id "BRAE" "447")
   (course-id "BRAE" "532")
   (course-id "CHEM" "341")
   (course-id "CM" "334")
   (course-id "CM" "432")
   (course-id "CRP" "420")
   (course-id "CRP" "435")
   (course-id "CRP" "404")
   (course-id "NR" "404")
   (course-id "CRP" "408")
   (course-id "NR" "408")
   (course-id "ERSC" "401")
   (course-id "GEOL" "401")
   (course-id "ERSC" "402")
   (course-id "GEOL" "402")
   (course-id "GEOL" "415")
   (course-id "IME" "314")
   (course-id "MATE" "425")
   (course-id "MATE" "450")
   (course-id "MATH" "344")
   (course-id "SS" "423")
   (course-id "SS" "442")))

(define (count-te-limited-units courses)
  (min 4
       (sum-list
        (map get-num-units
             (filter
              (lambda (crs) (member crs te-four-unit-max))
              (set->list courses))))))

(define (count-te-non-limited-units courses)
  (sum-list
   (map get-num-units
        (filter
         valid-te
         (set->list courses)))))

(define (count-total-te-units courses)
  (+ (count-te-limited-units courses) (count-te-non-limited-units courses)))

(define needed-te-units 24)

(define bs-civil-15-17
  (curriculum
   "Civil Engineering BS. 2015-17 catalog."
   bs-civil-15-17-main-coursework
   count-total-te-units
   needed-te-units))

;; prerequisites leave out co-requisites (for now)
(define important-courses
  (hash
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

(provide
 bs-civil-15-17
 important-courses)
