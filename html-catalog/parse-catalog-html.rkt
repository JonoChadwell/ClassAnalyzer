#lang racket

;; sxml functions for parsing catalog html pages.

(require html-parsing
         sxml/sxpath)

(provide file->sxml
         sxml->courses
         course-title
         course-prereqs
         cross-listing-info)

;; convert a file to sxml
(define (file->sxml file)
  (call-with-input-file file html->xexp))

;; the list of courses as sxml elements
(define (sxml->courses page-sxml)
  ((sxpath courseblock-sxpath) page-sxml))

;; the xpath that extracts the courses from the page:
(define courseblock-sxpath
  "//div[@class='courses']/div[@class='courseblock']")

;; given a course sxml block, return the course title as e.g.
;; '("CSC" "344" "Music Programming")
(define (course-title sxml)
    (match ((sxpath coursetitle-sxpath) sxml)
      [(list (list 'strong
                   dept
                   (list '& 'nbsp)
                   (regexp #px"^([0-9]+)\\. (.*)\\.\n"  (list _
                                                             num
                                                             descr))
                   _ ...))
       (list dept num descr)]
      ['()
       (raise-argument-error
        'course-title
        "sxml for a course containing courseblocktitle paragraphs"
        0 sxml)]))

(define coursetitle-sxpath
  "//p[@class='courseblocktitle']/strong")

;; given a crosslistings table and
;; sxml for a course, return the course's prerequisites
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
     (extract-prereq-from-para p)]
    [(list) #f]))


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
                                   (regexp #px"^(.*)&nbsp;(.*)$"
                                           (list _ dept num)))
                    _ ...) _ ...)
     (list dept num)]))



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
    [(regexp #px"^([A-Z]+)/([A-Z]+) ([0-9]+)$" (list _ pre1 pre2 num))
     `((,pre1 ,num) (,pre2 ,num))]
    [(regexp #px"^([A-Z]+) ([0-9]+)/([A-Z]+) ([0-9]+)$"
             (list _ pre1 num1 pre2 num2))
     `((,pre1 ,num1) (,pre2 ,num2))]
    [(regexp #px"^([A-Z]+) ([0-9]+)/([A-Z]+) ([0-9]+)/([A-Z]+) ([0-9]+)$"
             (list _ pre1 num1 pre2 num2 pre3 num3))
     `((,pre1 ,num1) (,pre2 ,num2) (,pre3 ,num3))]
    [other (error 'cross-listing-info
                  "unexpected text: ~e"
                  other)]))

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
  
  
  (check-equal?
   (extract-prereq-from-para
   '(p
     "Prerequisite: "
     (a
      (@
       (href
        "http://catalog.calpoly.edu/search/?P=MATH%20118")
       (title "MATH&nbsp;118")
       (class "bubblelink code")
       (onclick "return showCourse(this, 'MATH 118');"))
      "MATH"
      (& nbsp)
      "118")
     "\n"
     " (or equivalent) with a grade of C- or better, significant experience in\n"
     " computer programming, and consent of instructor. Corequisite: CSC 141 \n"
     "or "
     (a
      (@
       (href "http://catalog.calpoly.edu/search/?P=CSC%20348")
       (title "CSC&nbsp;348")
       (class "bubblelink code")
       (onclick "return showCourse(this, 'CSC 348');"))
      "CSC"
      (& nbsp)
      "348")
     "."))
   (list "Prerequisite: MATH 118\n (or equivalent) with a grade of C- \
or better, significant experience in\n computer programming, and \
consent of instructor. "
         (list (list "MATH" "118"))))

  (check-equal?
   (flatten-sxml-text
    '(p
      "Prerequisite: CSC/"
      (a
       (@
        (href "http://catalog.calpoly.edu/search/?P=CPE%20103")
        (title "CPE&nbsp;103")
        (class "bubblelink code")
        (onclick "return showCourse(this, 'CPE 103');"))
       "CPE"
       (& nbsp)
       "103")
      ", with a grade of C- or better, or equivalent."))
   "Prerequisite: CSC/CPE 103, with a grade of C- or better, or equivalent."))