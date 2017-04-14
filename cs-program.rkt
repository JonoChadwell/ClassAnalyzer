#lang racket

(require racket/set "requirement.rkt" "ge-areas.rkt" "utilities.rkt" "units.rkt" "types.rkt")

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
    (group-any
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
             D4)))))

;; checks whether a course is a valid core tech-elective
(define (valid-te crs)
  ;; todo
  #f)

(define te-valid
  (list
   (course "CSC" "301")
   (course "CSC" "305")
   (course "CSC" "309")
   (course "CSC" "321")
   (course "CSC" "323")
   (course "CSC" "325")
   (course "CSC" "344")
   (course "CSC" "365")
   (course "CSC" "366")
   (course "CSC" "369")
   (course "CSC" "371")
   (course "CSC" "378")
   (course "CSC" "400")
   (course "CSC" "402")
   (course "CSC" "405")
   (course "CSC" "406")
   (course "CSC" "409")
   (course "CSC" "410")
   (course "CSC" "422")
   (course "CSC" "424")
   (course "CSC" "429")
   (course "CSC" "435")
   (course "CSC" "436")
   (course "CSC" "437")
   (course "CSC" "448")
   (course "CSC" "454")
   (course "CPE" "454")
   (course "CSC" "458")
   (course "CPE" "458")
   (course "CSC" "466")
   (course "CSC" "468")
   (course "CSC" "471")
   (course "CPE" "471")
   (course "CSC" "473")
   (course "CSC" "474")
   (course "CSC" "476")
   (course "CPE" "476")
   (course "CSC" "477")
   (course "CSC" "478")
   (course "CSC" "480")
   (course "CSC" "481")
   (course "CSC" "483")
   (course "CSC" "484")
   (course "CSC" "486")
   (course "CSC" "489")
   (course "CSC" "490")
   (course "CSC" "496")
   (course "CSC" "508")
   (course "CSC" "509")
   (course "CSC" "515")
   (course "CPE" "515")
   (course "CSC" "521")
   (course "CSC" "530")
   (course "CSC" "540")
   (course "CSC" "550")
   (course "CSC" "560")
   (course "CSC" "564")
   (course "CPE" "564")
   (course "CSC" "566")
   (course "CSC" "569")
   (course "CPE" "569")
   (course "CSC" "570")
   (course "CSC" "572")
   (course "CSC" "580")
   (course "CSC" "581")
   (course "CSC" "582")
   (course "CPE" "400")
   (course "CPE" "416")
   (course "CPE" "419")
   (course "CPE" "428")
   (course "CPE" "464")
   (course "CPE" "465")
   (course "CPE" "482")
   (course "CPE" "485")
   (course "CPE" "488")
   (course "DATA" "301")))

(define te-must-have
  (list
   (course "CSC" "325")
   (course "CSC" "366")
   (course "CSC" "402")
   (course "CSC" "405")
   (course "CSC" "406")
   (course "CSC" "409")
   (course "CSC" "410")
   (course "CSC" "422")
   (course "CSC" "424")
   (course "CSC" "429")
   (course "CSC" "435")
   (course "CSC" "437")
   (course "CSC" "454")
   (course "CPE" "454")
   (course "CSC" "466")
   (course "CSC" "468")
   (course "CSC" "473")
   (course "CSC" "474")
   (course "CSC" "476")
   (course "CPE" "476")
   (course "CSC" "477")
   (course "CSC" "478")
   (course "CSC" "481")
   (course "CSC" "483")
   (course "CSC" "484")
   (course "CSC" "486")
   (course "CSC" "489")
   (course "CSC" "508")
   (course "CSC" "509")
   (course "CSC" "515")
   (course "CPE" "515")
   (course "CSC" "521")
   (course "CSC" "530")
   (course "CSC" "540")
   (course "CSC" "550")
   (course "CSC" "560")
   (course "CSC" "564")
   (course "CPE" "564")
   (course "CSC" "566")
   (course "CSC" "572")
   (course "CSC" "580")
   (course "CSC" "581")
   (course "CSC" "582")
   (course "CPE" "416")
   (course "CPE" "465")))

;; CSC students can get credit for up to 4 units from these courses
(define te-four-unit-max
  (list
   (course "AERO" "450")
   (course "ART" "384")
   (course "BUS" "310")
   (course "CHEM" "216")
   (course "CHEM" "217")
   (course "CHEM" "218")
   (course "CHEM" "312")
   (course "ECON" "339")
   (course "EE" "201")
   (course "EE" "251")
   (course "EE" "314")
   (course "EE" "336")
   (course "CPE" "336")
   (course "EE" "424")
   (course "ENVE" "542")
   (course "IME" "301")
   (course "IME" "314")
   (course "IME" "356")
   (course "MATH" "241")
   (course "MATH" "242")
   (course "MATH" "248")
   (course "MATH" "304")
   (course "MATH" "341")
   (course "MATH" "350")
   (course "MATH" "412")
   (course "ME" "211")
   (course "ME" "212")
   (course "ME" "405")
   (course "PHIL" "412")
   (course "PHIL" "422")
   (course "PSY" "329")
   (course "PSY" "333")
   (course "PSY" "351")
   (course "PSY" "457")
   (course "STAT" "313")
   (course "STAT" "323")
   (course "STAT" "324")
   (course "STAT" "330")
   (course "STAT" "331")
   (course "STAT" "416")
   (course "STAT" "418")
   (course "STAT" "419")))

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
  (curriculum
   "Civil Engineering BS. 2015-17 catalog."
   bs-civil-15-17-main-coursework
   count-total-te-units))

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