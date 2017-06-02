#lang typed/racket

(require "types.rkt" "quarter.rkt" "utilities.rkt" "course-overrides.rkt")

(require/typed  "catalog-page-parser.rkt"
                [read-courses-from-file (-> String (Listof course))])

(require/typed "course-overrides.rkt"
               [course-aliases (HashTable course-id course-id)])

(: get-first-alphabetically (-> course-id course-id course-id))
(define (get-first-alphabetically a b)
  (if (string-ci<? (course-id-dept a) (course-id-dept b))
      a
      b))

;; gets the cannonical id of a course
(: course-identifier (-> course course-id))
(define (course-identifier crs)
  (let ([lst (set->list (course-identifiers crs))])
    (if (empty? lst)
        (error "Course has no attached identifiers")
        (foldl get-first-alphabetically (first lst) (rest lst)))))

;; gets the cannonical department of a course
(: course-dept (-> course String))
(define (course-dept crs)
  (course-id-dept (course-identifier crs)))

;; gets the cannonical number of a course
(: course-number (-> course String))
(define (course-number crs)
  (course-id-number (course-identifier crs)))

;; Given a course identifier returns the cannonical identifier for the course
;; referenced by that identifier
(: cannonicalize-course (-> course-id course-id))
(define (cannonicalize-course id)
  (course-identifier (get-course id)))

;; cannonicalizes all parts of a requirement
(: cannonicalize-req (-> Requirement Requirement))
(define (cannonicalize-req req)
  (cond
    [(exactly? req) (exactly (cannonicalize-course (exactly-take req)))]
    [(all-of? req) (all-of (map cannonicalize-req (all-of-reqs req)))]
    [(one-of? req) (one-of (map cannonicalize-req (one-of-reqs req)))]))

;; Gets the full course object from any of its identifiers
(: get-course (-> course-id course))
(define (get-course id)
  (hash-ref course-id-table
            (hash-ref
             course-aliases
             id
             (lambda () id))
            (lambda ()
              (course
               (set id)
               (set)
               0
               "Unlisted Course"
               empty-requirement))))

(define department-list
  '("aero" "agb" "aeps" "agc" "aged" "ag" "asci" "ant" "arce" "arch" "art"
           "astr" "bio" "bmed" "brae" "bot" "bus" "chem" "cd" "chin" "crp"
           "ce" "coms" "cpe" "csc" "cm" "dsci" "danc" "data" "ese" "esm"
           "ersc" "econ" "educ" "ee" "engr" "engl" "edes" "enve" "es" "fpe"
           "fsn" "fr" "geog" "geol" "ger" "gs" "gsa" "gsb" "gse" "gsp" "grc"
           "hist" "hnrc" "hnrs" "ime" "itp" "isla" "ital" "jpns" "jour"
           "kine" "la" "laes" "ls" "msci" "mate" "math" "me" "mcro" "msl"
           "mu" "nr" "phil" "pem" "pew" "psc" "phys" "pols" "psy" "rpta"
           "rels" "scm" "socs" "soc" "ss" "span" "stat" "sie" "th" "univ"
           "wvit" "wgs" "wlc"))


(define all-courses
  (map
   (lambda ([x : String]) (read-courses-from-file (string-append "data/" x ".html")))
   department-list))

(: course-id-table (HashTable course-id course))
(define course-id-table (make-hash))

(for-each
 (lambda ([lst : (Listof course)])
   (for-each
    (lambda ([crs : course])
      (for-each
       (lambda ([crsid : course-id])
         (cond
           [(not (hash-has-key? course-id-table crsid))
            (hash-set! course-id-table crsid crs)]))
       (set->list (course-identifiers crs))))
    lst))
 (reverse all-courses))

;; Cannonicalize all prerequisistes
(for-each
 (lambda ([pr : (Pairof course-id course)])
   (hash-set! course-id-table
              (left pr)
              (course
               (course-identifiers (right pr))
               (course-typical-terms (right pr))
               (course-units (right pr))
               (course-name (right pr))
               (cannonicalize-req (course-prereqs (right pr))))))
 (hash->list course-id-table))

(: course-id->course (-> course-id course))
(define (course-id->course id)
  (hash-ref course-id-table id
            (lambda ()
              (course
               (set id)
               (set)
               0
               "Unlisted Course"
               empty-requirement))))

;; Gets a string representation of a course
(: course-id->string (-> course-id String))
(define (course-id->string id)
  (string-append (course-id-dept id) "_" (course-id-number id)))

;; Given a course id and the earliest possible quarter, returns the next
;; quarter that a course will be offered.
(: course-next-offered (-> course-id Quarter Quarter))
(define (course-next-offered id qtr)
  (let ([terms-offered (course-typical-terms (course-id->course id))]
        [next-qtr (quarter-after qtr)])
    (if (set-empty? terms-offered)
        ;; assume TBD courses will be offered within a year
        (+ qtr 10)
        (if (set-member? terms-offered (quarter->term qtr))
            qtr
            (course-next-offered id next-qtr)))))

(module+ test
  (require typed/rackunit)

  (define test-id-1 (course-id "CPE" "357"))
  (define test-id-2 (course-id "CSC" "357"))

  (define test-course-1
    (course
     (set test-id-1 test-id-2)
     (set)
     4
     "Systems Programming"
     (all-of (list
              (exactly (course-id "CPE" "103"))
              (one-of (list
                       (exactly (course-id "CPE" "225"))
                       (exactly (course-id "CPE" "233"))))))))

  (check-equal?
   (get-first-alphabetically test-id-1 test-id-2)
   test-id-1)

  (check-equal?
   (course-number test-course-1)
   "357")

  (check-equal?
   (course-id->string (course-id "CPE" "101"))
   "CPE_101")

  (check-equal?
   (course-dept test-course-1)
   "CPE"))

(provide
 course-id-table
 course-identifier
 course-dept
 course-number
 get-course
 cannonicalize-course
 course-next-offered
 course-id->course
 course-id->string
 cannonicalize-req)
