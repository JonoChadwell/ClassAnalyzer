#lang racket

(require racket/set "start.rkt" "ge-areas.rkt" "utilities.rkt" "units.rkt")

;; 2015-17 catalog.
;; http://catalog.calpoly.edu/collegesandprograms/collegeofengineering/computersciencesoftwareengineering/bscomputerscience/

(define bs-cs-15-17-main-coursework
  (all-of
   (list
    ;; senior project
    (group-all
     "CSC" "491"
     "CSC" "492")

    ;; Discrete Math
    (group-one
     "CSC" "108"
     "CSC" "202")

    ;; Software Engineering
    (one-of (list
             (exactly (course "CSC" "307"))
             (all-of (list
                      (exactly (course "CSC" "308"))
                      (exactly (course "CSC" "309"))))))

    ;; core classes
    (group-all
     "CSC" "101"
     "CSC" "123"
     "CSC" "203"
     "CSC" "225"
     "CSC" "300"
     "CSC" "315"
     "CSC" "348"
     "CSC" "349"
     "CSC" "357"
     "CSC" "430"
     "CSC" "431"
     "CSC" "445"
     "CSC" "453")

    ;; support classes
    (one-of (list
             (exactly (course "MATH" "206"))
             (exactly (course "MATH" "244"))))
    (group-all
     "ENGL" "149"
     "MATH" "141"
     "MATH" "142"
     "MATH" "143"
     "STAT" "312")
    (one-of (list
             (all-of (list
                      (exactly (course "BIO" "213"))
                      (exactly (course "BMED" "213"))))
             (group-one
              "BIO" "111"
              "BIO" "161"
              "MCRO" "221"
              "MCRO" "224")))
    (group-one
     "MATH" "241"
     "MATH" "248"
     "MATH" "306"
     "MATH" "335"
     "MATH" "336"
     "MATH" "437"
     "MATH" "470"
     "STAT" "313"
     "STAT" "323"
     "STAT" "324"
     "STAT" "330"
     "STAT" "331"
     "STAT" "416"
     "STAT" "418"
     "STAT" "419")
    (one-of (list
             (group-all
              "CHEM" "124"
              "CHEM" "125"
              "CHEM" "126")
             (group-all
              "PHYS" "141"
              "PHYS" "132"
              "PHYS" "133")))
    (group-one
     "BIO" "111"
     "BIO" "161"
     "BOT" "121"
     "CHEM" "124"
     "MCRO" "221"
     "MCRO" "224"
     "PHYS" "141")

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

;; CE students can get credit for up to 4 units from these courses
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
  (+ (count-te-limited-units courses) (count-te-non-limited-units)))

(define needed-te-units 24)

(define (meets-te-requirement courses)
  (>= (count-total-te-units courses) needed-te-units))

(define bs-civil-15-17
  (degree-requirement
   "Civil Engineering BS. 2015-17 catalog."
   bs-civil-15-17-main-coursework
   meets-te-requirement))

;; prerequisites leave out co-requisites (for now)
(define important-courses
  (hash
   (course "CE" "111") (all-of empty)
   (course "CE" "112") (all-of empty) ;; leaving out calc 1 requirement because we don't have data for AP high school credit
   (course "CE" "113") (all-of empty)
   (course "CE" "204") (exactly (course "ME" "211"))
   (course "CE" "207") (exactly (course "CE" "204"))
   (course "CE" "251") (group-all "CE" "113" "CE" "204" "MATH" "244")
   (course "CE" "259") (exactly (course "CE" "204"))
   (course "CE" "321") (exactly (course "CE" "259"))
   (course "CE" "336") (group-any "ME" "341" "ENVE" "264")
   (course "CE" "352") (exactly (course "CE" "207"))
   (course "CE" "355") (all-of (list (exactly (course "CE" "259")) (group-any "CE" "351" "CE" "352")))
   (course "CE" "381") (all-of (list (exactly (course "CE" "207")) (group-any "ME" "341" "ENVE" "264")))))

(provide bs-civil-15-17 important-courses)