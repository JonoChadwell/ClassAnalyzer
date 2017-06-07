#lang racket

(require racket/set "requirement.rkt" "ge-areas.rkt" "utilities.rkt" "units.rkt" "types.rkt" "course.rkt")

;; 2015-17 catalog.
;; http://catalog.calpoly.edu/collegesandprograms/collegeofengineering/computersciencesoftwareengineering/bscomputerscience/

(define bs-cs-15-17-main-coursework
  (cannonicalize-req
   (all-of
    (list
     ;; senior project
     (group-all
      "CSC" "491"
      "CSC" "492")

     ;; Discrete Math
     (group-any
      "CSC" "108"
      "CSC" "202")

     ;; Software Engineering
     (one-of (list
              (exactly (course-id "CSC" "307"))
              (all-of (list
                       (exactly (course-id "CSC" "308"))
                       (exactly (course-id "CSC" "309"))))))

     ;; core classes
     (group-any
      "CSC" "225"
      "CPE" "233")
     (group-all
      ;; This requirement is implied as a prereq for CSC 108/202 and just makes noise for people with AP credit
      ; "CSC" "101"
      "CSC" "123"
      "CSC" "203"
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
              (exactly (course-id "MATH" "206"))
              (exactly (course-id "MATH" "244"))))
     (group-all
      "ENGL" "149"
      ;; This requirement is implied as a prereq for MATH 143 and just makes noise for people with AP credit
      ; "MATH" "141"
      ;; This requirement is implied as a prereq for MATH 143 and just makes noise for people with AP credit
      ; "MATH" "142"
      "MATH" "143"
      "STAT" "312")
     (one-of (list
              (all-of (list
                       (exactly (course-id "BIO" "213"))
                       (exactly (course-id "BMED" "213"))))
              (group-any
               "BIO" "111"
               "BIO" "161"
               "MCRO" "221"
               "MCRO" "224")))
     (group-any
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
     (group-any
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
              D4))))))

(define te-valid
  (map cannonicalize-course 
       (list
        (course-id "CSC" "301")
        (course-id "CSC" "305")
        (course-id "CSC" "309")
        (course-id "CSC" "321")
        (course-id "CSC" "323")
        (course-id "CSC" "325")
        (course-id "CSC" "344")
        (course-id "CSC" "365")
        (course-id "CSC" "366")
        (course-id "CSC" "369")
        (course-id "CSC" "371")
        (course-id "CSC" "378")
        (course-id "CSC" "400")
        (course-id "CSC" "402")
        (course-id "CSC" "405")
        (course-id "CSC" "406")
        (course-id "CSC" "409")
        (course-id "CSC" "410")
        (course-id "CSC" "422")
        (course-id "CSC" "424")
        (course-id "CSC" "429")
        (course-id "CSC" "435")
        (course-id "CSC" "436")
        (course-id "CSC" "437")
        (course-id "CSC" "448")
        (course-id "CSC" "454")
        (course-id "CPE" "454")
        (course-id "CSC" "458")
        (course-id "CPE" "458")
        (course-id "CSC" "466")
        (course-id "CSC" "468")
        (course-id "CSC" "471")
        (course-id "CPE" "471")
        (course-id "CSC" "473")
        (course-id "CSC" "474")
        (course-id "CSC" "476")
        (course-id "CPE" "476")
        (course-id "CSC" "477")
        (course-id "CSC" "478")
        (course-id "CSC" "480")
        (course-id "CSC" "481")
        (course-id "CSC" "483")
        (course-id "CSC" "484")
        (course-id "CSC" "486")
        (course-id "CSC" "489")
        (course-id "CSC" "490")
        (course-id "CSC" "496")
        (course-id "CSC" "508")
        (course-id "CSC" "509")
        (course-id "CSC" "515")
        (course-id "CPE" "515")
        (course-id "CSC" "521")
        (course-id "CSC" "530")
        (course-id "CSC" "540")
        (course-id "CSC" "550")
        (course-id "CSC" "560")
        (course-id "CSC" "564")
        (course-id "CPE" "564")
        (course-id "CSC" "566")
        (course-id "CSC" "569")
        (course-id "CPE" "569")
        (course-id "CSC" "570")
        (course-id "CSC" "572")
        (course-id "CSC" "580")
        (course-id "CSC" "581")
        (course-id "CSC" "582")
        (course-id "CPE" "400")
        (course-id "CPE" "416")
        (course-id "CPE" "419")
        (course-id "CPE" "428")
        (course-id "CPE" "464")
        (course-id "CPE" "465")
        (course-id "CPE" "482")
        (course-id "CPE" "485")
        (course-id "CPE" "488")
        (course-id "DATA" "301"))))

(define te-must-have
  (map cannonicalize-course 
       (list
        (course-id "CSC" "325")
        (course-id "CSC" "366")
        (course-id "CSC" "402")
        (course-id "CSC" "405")
        (course-id "CSC" "406")
        (course-id "CSC" "409")
        (course-id "CSC" "410")
        (course-id "CSC" "422")
        (course-id "CSC" "424")
        (course-id "CSC" "429")
        (course-id "CSC" "435")
        (course-id "CSC" "437")
        (course-id "CSC" "454")
        (course-id "CPE" "454")
        (course-id "CSC" "466")
        (course-id "CSC" "468")
        (course-id "CSC" "473")
        (course-id "CSC" "474")
        (course-id "CSC" "476")
        (course-id "CPE" "476")
        (course-id "CSC" "477")
        (course-id "CSC" "478")
        (course-id "CSC" "481")
        (course-id "CSC" "483")
        (course-id "CSC" "484")
        (course-id "CSC" "486")
        (course-id "CSC" "489")
        (course-id "CSC" "508")
        (course-id "CSC" "509")
        (course-id "CSC" "515")
        (course-id "CPE" "515")
        (course-id "CSC" "521")
        (course-id "CSC" "530")
        (course-id "CSC" "540")
        (course-id "CSC" "550")
        (course-id "CSC" "560")
        (course-id "CSC" "564")
        (course-id "CPE" "564")
        (course-id "CSC" "566")
        (course-id "CSC" "572")
        (course-id "CSC" "580")
        (course-id "CSC" "581")
        (course-id "CSC" "582")
        (course-id "CPE" "416")
        (course-id "CPE" "465"))))

;; Checks whether a course is a valid core tech-elective
(define (valid-te? crs)
  (let ([crs-cannonical (cannonicalize-course crs)])
    (or (set-member? te-valid crs-cannonical)
        (set-member? te-must-have crs-cannonical))))

;; CSC students can get credit for up to 4 units from these courses
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
  (let ([units (sum-list
                (map get-num-units
                     (filter
                      valid-te?
                      (set->list courses))))])
    ; Limit TE units to one less than enough if no tech elective with prereq
    ; has been taken
    (if (set-empty? (set-intersect courses (list->set te-must-have)))
        (min units (- needed-te-units 1))
        units)))

(define (count-total-te-units courses)
  (+ (count-te-limited-units courses) (count-te-non-limited-units courses)))

(define needed-te-units 24)

(define bs-cs-15-17
  (curriculum
   "Computer Science BS. 2015-17 catalog."
   bs-cs-15-17-main-coursework
   count-total-te-units
   needed-te-units))

(provide bs-cs-15-17)