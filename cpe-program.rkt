#lang racket

(require racket/set "requirement.rkt" "ge-areas.rkt" "utilities.rkt" "units.rkt" "types.rkt" "course.rkt")

;; 2017-19 catalog.
;; http://catalog.calpoly.edu/collegesandprograms/collegeofengineering/computerengineering/bscomputerengineering/

(define bs-cpe-17-19-main-coursework
  (cannonicalize-req
   (all-of
    (list

     ;; core classes
     (group-all
      "CPE" "100"
      ;; This requirement is implied as a prereq for CSC 202 and just makes noise for people with AP credit
      ; "CSC" "101"
      "CSC" "123"
      "CPE" "133"
      "CSC" "202"
      "CSC" "203"
      "CPE" "233"
      "CPE" "315"
      "CSC" "357"
      "CPE" "350"
      "CPE" "450"
      "CPE" "453"
      "CPE" "461"
      "CPE" "462"
      "CPE" "464"
      "CSC" "348"
      "EE" "112"
      "EE" "211"
      "EE" "241"
      "EE" "212"
      "EE" "242"
      "EE" "228"
      "EE" "306"
      "EE" "346"
      "EE" "307"
      "EE" "347")

     ;; support classes
     (exactly (course-id "CHEM" "124"))
     (group-any
      "CHEM" "125"
      "CPE" "328"
      "CSC" "349"
      "MATE" "210"
      "MATE" "215"
      "ME" "211")
     (exactly (course-id "ENGL" "149"))
     (group-any
      "IME" "156"
      "IME" "157"
      "IME" "458")
     (group-all
      ;; This requirement is implied as a prereq for MATH 143 and just makes noise for people with AP credit
      ; "MATH" "141"
      ;; This requirement is implied as a prereq for MATH 143 and just makes noise for people with AP credit
      ; "MATH" "142"
      "MATH" "143"
      "MATH" "241"
      "MATH" "244"
      "PHYS" "141"
      "PHYS" "132"
      "PHYS" "133"
      "PHYS" "211"
      "STAT" "350")

     ;; general electives
     (all-of (list
              A1
              A2
              B2
              C1
              C2
              C3
              C4
              D1
              D2
              D3
              D4))))))

;; CPE tech electives are any classes 300-500 level CPE, CSC, or EE courses except for the
;; the following (which are required classes and therefore not valid electives).
(define te-invalid
  (map cannonicalize-course
       (list
        (course-id "CPE" "315")
        (course-id "CSC" "357")
        (course-id "CPE" "350")
        (course-id "CPE" "450")
        (course-id "CPE" "453")
        (course-id "CPE" "461")
        (course-id "CPE" "462")
        (course-id "CPE" "464")
        (course-id "CSC" "348")
        (course-id "EE" "306")
        (course-id "EE" "346")
        (course-id "EE" "307")
        (course-id "EE" "347")
        (course-id "CPE" "328")
        (course-id "CSC" "349"))))


;; checks whether a course is a valid core tech-elective
(define (valid-te? crs-id)
  (if (set-member? (set "CSC" "CPE" "EE") (course-id-dept crs-id))
      (if (not (list-contains crs-id te-invalid))
          (>= (string->integer (regexp-replace "[^0-9]" (course-id-number crs-id) "")) 300)
          #f)
      #f))

;; CPE students can get credit for up to 4 units from these courses
(define te-four-unit-max
  (map cannonicalize-course 
       (list
        (course-id "AERO" "450")
        (course-id "ART" "384")
        (course-id "BUS" "310")
        (course-id "CHEM" "216")
        (course-id "CHEM" "217")
        (course-id "CHEM" "218")
        (course-id "CHEM" "312")
        (course-id "ECON" "339")
        (course-id "EE" "201")
        (course-id "EE" "251")
        (course-id "EE" "314")
        (course-id "EE" "336")
        (course-id "CPE" "336")
        (course-id "EE" "424")
        (course-id "ENVE" "542")
        (course-id "IME" "301")
        (course-id "IME" "314")
        (course-id "IME" "356")
        (course-id "MATH" "241")
        (course-id "MATH" "242")
        (course-id "MATH" "248")
        (course-id "MATH" "304")
        (course-id "MATH" "341")
        (course-id "MATH" "350")
        (course-id "MATH" "412")
        (course-id "ME" "211")
        (course-id "ME" "212")
        (course-id "ME" "405")
        (course-id "PHIL" "412")
        (course-id "PHIL" "422")
        (course-id "PSY" "329")
        (course-id "PSY" "333")
        (course-id "PSY" "351")
        (course-id "PSY" "457")
        (course-id "STAT" "313")
        (course-id "STAT" "323")
        (course-id "STAT" "324")
        (course-id "STAT" "330")
        (course-id "STAT" "331")
        (course-id "STAT" "416")
        (course-id "STAT" "418")
        (course-id "STAT" "419"))))

; Counts the TE units from courses which can only supply up to 4 units
(define (count-te-limited-units courses)
  (min 4
       (sum-list
        (map get-num-units
             (filter
              (lambda (crs) (member crs te-four-unit-max))
              (set->list courses))))))

; Counts the TE units from sources with no contribution limits
(define (count-te-non-limited-units courses)
  (sum-list
   (map get-num-units
        (filter
         valid-te?
         (set->list courses)))))

(define (count-total-te-units courses)
  (+ (count-te-limited-units courses) (count-te-non-limited-units courses)))

(define needed-te-units 24)

(define bs-cpe-17-19
  (curriculum
   "Computer Engineering BS. 2017-19 catalog."
   "CPE"
   bs-cpe-17-19-main-coursework
   count-total-te-units
   needed-te-units))

(module+ test
  (require rackunit)

  (check-true (valid-te? (cannonicalize-course (course-id "CSC" "480"))))
  (check-false (valid-te? (cannonicalize-course (course-id "CSC" "357"))))
  (check-false (valid-te? (cannonicalize-course (course-id "CSC" "225"))))
  (check-false (valid-te? (cannonicalize-course (course-id "ENGL" "480")))))

(provide bs-cpe-17-19)