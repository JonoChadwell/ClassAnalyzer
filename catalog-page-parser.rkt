#lang racket

(require html-parsing
         sxml/sxpath
         "types.rkt"
         "course-overrides.rkt")

;; this file mostly lifted from Dr. Clement's web scraper

;; TODO!!!!: Change prerequisite parsing to determing prereq type (one of / all of).
;; for now, it is assumed you need ALL mentioned prerequisites

;; convert a file to sxml
(define (file->sxml file)
  (call-with-input-file file html->xexp))

;; the list of courses as sxml elements
(define (sxml->courses page-sxml)
  ((sxpath courseblock-sxpath) page-sxml))

;; the xpath that extracts the courses from the page:
(define courseblock-sxpath
  "//div[@class='courses']/div[@class='courseblock']")

;; Gets the information from the header of the xml representation of a course
;; returns the list: department, number, name, units
(define (parse-course-header sxml)
  (match ((sxpath coursetitle-sxpath) sxml)
    [(list (list 'strong
                 (regexp #px"^([A-Z]+)\\W*([0-9]+)\\. (.*)\n" (list _ dept numb name))
                 (list
                  'span
                  _
                  ;; Picks the larger side of the unit range
                  (regexp #px"^[0-9]+-([0-9]+)" (list _ units)))
                 _ ...)
           _ ...)
     (list dept numb name (string->number units))]
    [(list (list 'strong
                 (regexp #px"^([A-Z]+)\\W*([0-9]+)\\. (.*)\n" (list _ dept numb name))
                 (list
                  'span
                  _
                  (regexp #px"^([0-9]+)" (list _ units)))
                 _ ...)
           _ ...)
     (list dept numb name (string->number units))] ; (list dept num descr)]
    ['()
     (raise-argument-error
      'course-title
      "Could not find course information"
      0 sxml)]))

(define coursetitle-sxpath
  "//p[@class='courseblocktitle']/strong")

;; given a list of 2 element lists of string, builds a list
;; of course-ids for the strings
(define (map-to-course-ids lst)
  (map (lambda (x)
         (course-id (first x) (second x)))
       lst))

;; given sxml for a course, return the course's prerequisites
(define (course-prereqs sxml)
  (define all-paras ((sxpath "//p") sxml))
  (define prereq-paras
    (filter
     (λ (p) (match p
              [(list 'p (regexp #px"^(Prerequisite|Corequisite|Concurrent|Recommended)" (list _ _)) _ ...) #t]
              [other #f]))
     all-paras))
  (match prereq-paras
    [(list p)
     (map-to-course-ids (second (extract-prereq-from-para p)))]
    [(list) empty]))

;; the prereq paragraph can contain any of three separate
;; sections: prerequisites, corequisites, and recommended.
;; fortunately, they always seem to occur in this order.
;; basically, we should ignore anything that occurs after
;; either "Corequisite" or "Recommended". Wait... or "Concurrent" ?
;; returns a list of two elements: the prerequisite text, and a list
;; of the prerequisite courses.
(define (extract-prereq-from-para sxml)
  (define para-elts
    (match sxml
      [(list 'p elts ...) elts]))
  (define prereq-elts
    (let loop ([elts para-elts])
      (cond [(empty? elts) '()]
            [else (match (prereq-ending-check (first elts))
                    [#f (cons (first elts)
                              (loop (rest elts)))]
                    [(list pre) (list pre)])])))
  
  (define links ((sxpath "//a[@class='bubblelink code']")
                 (cons 'p prereq-elts)))
  (list
   (flatten-sxml-text (cons 'p prereq-elts))
   (map prereq-link->course links)))

;; given an sxml element, if it is a string containing one
;; of the prereq-ending words (such as "Corequisite"), return
;; a list containing the part of the string before the word.
;; otherwise return false.
(define (prereq-ending-check elt)
  (match elt
    [(regexp #px"^(.*)(Concurrent|Corequisite|Recommended)"
             (list _ prefix _))
     (list prefix)]
    [else #f]))

;; given a prereq link, return the course it points to:
(define (prereq-link->course sxml)
  (match sxml
    [(list 'a (list '@ _ ... (list 'title
                                   (regexp #px"^(.*)&(#[^;]*|nbsp);(.*)$"
                                           (list _ dept _ num)))
                    _ ...) _ ...)
     (list dept num)]))

;; get the terms a course is offered from the description
(define (course-terms-offered sxml)
  (append-map (lambda (element)
                (match element
                  [(list 'p _ (regexp #px"^Term Typically Offered:(.*)" (list _ offerings)))
                   (parse-terms-offered offerings)]
                  [else
                   empty]))
              ((sxpath "//p") sxml)))

;; given terms offered text, produce a list of Terms
(define (parse-terms-offered str)
  (let ([trimmed (string-trim str)])
    (if (equal? "TBD" trimmed)
        empty
        (let ([parts (map string-trim (string-split trimmed ","))])
          (map (lambda (part)
                 (cond
                   [(equal? part "F") 'FALL]
                   [(equal? part "W") 'WINTER]
                   [(equal? part "SP") 'SPRING]
                   [(equal? part "SU") 'SUMMER]
                   [else (error (string-append "Course offered in unknown term: '" part "'"))]))
               parts)))))

;; given an sxml element, flatten out the text.
(define (flatten-sxml-text sxml)
  (match sxml
    [(? string? s) s]
    [(list (or 'a 'p 'div) (list '@ _ ...) contents ...)
     (apply string-append (map flatten-sxml-text contents))]
    [(list (or 'a 'p 'div) contents ...)
     (apply string-append (map flatten-sxml-text contents))]
    [(list '& 'nbsp) " "]
    [other
     (error 'flatten-sxml-text "unexpected sxml text: ~e"
            sxml)]))

;; find the cross-listing information associated with a course
(define (cross-listing-info sxml)
  (match ((sxpath "//div[@class='courseblockdesc']") sxml)
    [(list p)
     (match (flatten-sxml-text p)
       [(regexp #px"Crosslisted\\s+as\\s+([^.]+)" (list _ course))
        (crosslisted-text->course-list course)]
       [other #f])]
    [(list)
     (error 'cross-listing-info
            "no course description block found in ~e" 
            sxml)]))

;; given the text appearing after "Crosslisted as", return a list
;; of the associated courses
(define (crosslisted-text->course-list text)
  (match (string-trim text)
    [(regexp #px"^([A-Z]+)/([A-Z]+)\\W*([0-9]+)$" (list _ pre1 pre2 num))
     `((,pre1 ,num) (,pre2 ,num))]
    [(regexp #px"^([A-Z]+)/([A-Z]+)/([A-Z]+)\\W*([0-9]+)$" (list _ pre1 pre2 pre3 num))
     `((,pre1 ,num) (,pre2 ,num) (,pre3 ,num))]
    [(regexp #px"^([A-Z]+)/([A-Z]+)/([A-Z]+)/([A-Z]+)/([A-Z]+)/([A-Z]+)/([A-Z]+)\\W*([0-9]+)$"
             (list _ pre1 pre2 pre3 pre4 pre5 pre6 pre7 num))
     `((,pre1 ,num) (,pre2 ,num) (,pre3 ,num) (,pre4 ,num) (,pre5 ,num) (,pre6 ,num) (,pre7 ,num))]
    [(regexp #px"^([A-Z]+)\\W*([0-9]+)/([A-Z]+)\\W*([0-9]+)$"
             (list _ pre1 num1 pre2 num2))
     `((,pre1 ,num1) (,pre2 ,num2))]
    [(regexp #px"^([A-Z]+)\\W*([0-9]+)/([A-Z]+)\\W*([0-9]+)/([A-Z]+)\\W*([0-9]+)$"
             (list _ pre1 num1 pre2 num2 pre3 num3))
     `((,pre1 ,num1) (,pre2 ,num2) (,pre3 ,num3))]
    [other (error 'cross-listing-info
                  "unexpected text: ~e"
                  other)]))

;; gets the set of all identifiers for a sxml representation of a course
(define (get-all-names sxml primary-id)
  (let ([crosslistings (cross-listing-info sxml)])
    (if crosslistings
        (foldl (lambda (a b) (set-add b a)) (set primary-id) (map-to-course-ids crosslistings))
        (set primary-id))))

;; gets the override prerequisites for a course
(define (get-override-prereqs id-list)
  (if (empty? id-list)
      #f
      (let ([override (hash-ref prereq-override-map (first id-list) (lambda () #f))])
        (if override
            override
            (get-override-prereqs (rest id-list))))))

;; Converts from an xml representation of a course to a course struct
(define (parse-course sxml)
  (let* ([header (parse-course-header sxml)]
         [dept (first header)]
         [numb (second header)]
         [name (third header)]
         [units (fourth header)]
         [primary-id (course-id dept numb)]
         [all-ids (get-all-names sxml primary-id)]
         [terms (list->set (course-terms-offered sxml))]
         [override-prereqs (get-override-prereqs (set->list all-ids))]
         [parsed-prereqs (all-of (map exactly (course-prereqs sxml)))]
         [prereqs (if override-prereqs override-prereqs parsed-prereqs)])
    (course all-ids terms units name prereqs)))

(define (read-courses-from-file filename)
  (map parse-course (sxml->courses (file->sxml filename))))

(provide read-courses-from-file)

(module+ test
  (require rackunit)

  (check-equal? (crosslisted-text->course-list "CPE/CSC 443")
                '(("CPE" "443") ("CSC" "443")))
  (check-equal? (crosslisted-text->course-list "CSC 310/HNRS 311")
                '(("CSC" "310") ("HNRS" "311")))
  (check-equal? (crosslisted-text->course-list "CPE 441/EE 431")
                '(("CPE" "441") ("EE" "431")))
  (check-equal? (crosslisted-text->course-list "CPE 488/IME 458/MATE 458")
                '(("CPE" "488") ("IME" "458") ("MATE" "458")))

  (check-equal? (parse-terms-offered " F, W, SU") '(FALL WINTER SUMMER))

  (check-equal? (parse-terms-offered " TBD  ") '())

  (check-equal?
   (map-to-course-ids '(("CSC" "101") ("CSC" "102")))
   (list (course-id "CSC" "101") (course-id "CSC" "102"))))
