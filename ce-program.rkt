#lang racket

(require racket/set "requirement.rkt" "types.rkt" "ge-areas.rkt" "utilities.rkt" "units.rkt" "course.rkt")

;; 2015-17 catalog.
;; Data from http://catalog.calpoly.edu/collegesandprograms/collegeofengineering/civilenvironmentalengineering/bscivilengineering/

(define bs-ce-15-17-main-coursework
  (cannonicalize-req
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
      ;; These requirements are implied as prereqs for MATH 241 and just make noise for people with AP credit
      ; "MATH" "141"
      ; "MATH" "142"
      ; "MATH" "143"
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
              D4))))))

;; checks whether a course is a valid core tech-elective
(define (valid-te crs)
  (or
   (list-contains
    crs
    (map cannonicalize-course 
         (list
          (course-id "CE" "356")
          (course-id "CE" "371")
          (course-id "CM" "371")
          (course-id "ENVE" "325"))))
   (and (or (eq? (course-id-dept crs) "ENVE")
            (eq? (course-id-dept crs) "CE"))
        (and (> (string->number (course-id-number crs)) 400)
             (not
              (list-contains
               crs
               ((map cannonicalize-course 
                     (list
                      (course-id "CE" "465")
                      (course-id "CE" "466")
                      (course-id "CE" "467")
                      (course-id "CE" "468")
                      (course-id "CE" "469")
                      (course-id "CE" "404"))))))))))

;; CE students can get credit for up to 4 units from these courses
(define te-four-unit-max
  (map cannonicalize-course 
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
        (course-id "SS" "442"))))

(define (count-te-limited-units courses)
  (min 4
       (sum-int-list
        (map get-num-units
             (filter
              (lambda (crs) (member crs te-four-unit-max))
              (set->list courses))))))

(define (count-te-non-limited-units courses)
  (sum-int-list
   (map get-num-units
        (filter
         valid-te
         (set->list courses)))))

(define (count-total-te-units courses)
  (+ (count-te-limited-units courses) (count-te-non-limited-units courses)))

(define needed-te-units 24)

(define bs-ce-15-17
  (curriculum
   "Civil Engineering BS. 2015-17 catalog."
   "CE"
   bs-ce-15-17-main-coursework
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

(module+ test
  (require rackunit)

  (define completed-coursework
    (map cannonicalize-course 
         (list
          ;; core coursees
          (course-id "CE" "111")
          (course-id "CE" "112")
          (course-id "CE" "113")
          (course-id "CE" "204")
          (course-id "CE" "207")
          (course-id "CE" "251")
          (course-id "CE" "259")
          (course-id "CE" "321")
          (course-id "CE" "322")
          (course-id "CE" "336")
          (course-id "CE" "337")
          (course-id "CE" "352")
          (course-id "CE" "355")
          (course-id "CE" "381")
          (course-id "CE" "382")
          (course-id "CE" "465")
          (course-id "CSC" "231")
          ;; senior project
          (course-id "CE" "466")
          (course-id "CE" "467")
          ;; support courses
          (course-id "BMED" "213")
          (course-id "BIO" "213")
          (course-id "BRAE" "239")
          (course-id "CHEM" "124")
          (course-id "CHEM" "125")
          (course-id "ENGL" "149")
          (course-id "ENVE" "331")
          (course-id "GEOL" "201")
          (course-id "MATE" "210")
          (course-id "MATE" "215")
          (course-id "MATH" "141")
          (course-id "MATH" "142")
          (course-id "MATH" "143")
          (course-id "MATH" "241")
          (course-id "MATH" "244")
          (course-id "ME" "211")
          (course-id "ME" "212")
          (course-id "ME" "302")
          (course-id "ME" "341")
          (course-id "PHYS" "141")
          (course-id "PHYS" "132")
          (course-id "PHYS" "133")
          (course-id "STAT" "312")
          ;; GEs
          (course-id "ENGL" "134")
          (course-id "COMS" "101")
          (course-id "ENGL" "231")
          (course-id "PHIL" "230")
          (course-id "MU" "101")
          (course-id "ARCH" "320")
          (course-id "HIST" "206")
          (course-id "ECON" "201")
          (course-id "ANT" "201")
          (course-id "KINE" "250"))))

  (define missing-senior-project
    (map cannonicalize-course 
         (list
          ;; core coursees
          (course-id "CE" "111")
          (course-id "CE" "112")
          (course-id "CE" "113")
          (course-id "CE" "204")
          (course-id "CE" "207")
          (course-id "CE" "251")
          (course-id "CE" "259")
          (course-id "CE" "321")
          (course-id "CE" "322")
          (course-id "CE" "336")
          (course-id "CE" "337")
          (course-id "CE" "352")
          (course-id "CE" "355")
          (course-id "CE" "381")
          (course-id "CE" "382")
          (course-id "CE" "465")
          (course-id "CSC" "231")
          ;; senior project
          ;; support coursees
          (course-id "BMED" "213")
          (course-id "BIO" "213")
          (course-id "BRAE" "239")
          (course-id "CHEM" "124")
          (course-id "CHEM" "125")
          (course-id "ENGL" "149")
          (course-id "ENVE" "331")
          (course-id "GEOL" "201")
          (course-id "MATE" "210")
          (course-id "MATE" "215")
          (course-id "MATH" "141")
          (course-id "MATH" "142")
          (course-id "MATH" "143")
          (course-id "MATH" "241")
          (course-id "MATH" "244")
          (course-id "ME" "211")
          (course-id "ME" "212")
          (course-id "ME" "302")
          (course-id "ME" "341")
          (course-id "PHYS" "141")
          (course-id "PHYS" "132")
          (course-id "PHYS" "133")
          (course-id "STAT" "312")
          ;; GEs
          (course-id "ENGL" "134")
          (course-id "COMS" "101")
          (course-id "ENGL" "231")
          (course-id "PHIL" "230")
          (course-id "MU" "101")
          (course-id "ARCH" "320")
          (course-id "HIST" "206")
          (course-id "ECON" "201")
          (course-id "ANT" "201")
          (course-id "KINE" "250"))))

  (check-true (meets (list->set completed-coursework) (curriculum-requirements bs-ce-15-17)))
  (check-false (meets (list->set missing-senior-project) (curriculum-requirements bs-ce-15-17))))


(provide
 bs-ce-15-17
 important-courses)
