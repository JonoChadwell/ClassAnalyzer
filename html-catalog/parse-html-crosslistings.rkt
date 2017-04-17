#lang racket

;; parse the crosslistings from a catalog html file

(require racket/runtime-path
         "parse-catalog-html.rkt")

(define-runtime-path HERE ".")

;; this data is used to canonicalize names of cross-listed courses
(define fad-courses
  (file->value (build-path HERE
                           "2158-2164-fad-course-names.rktd")))

;; a hash table mapping all observed course names to
;; the "preferred" names according to the FAD
(define (courses->crosslistings courses)
  (define as-list
    (filter (λ (x) x)
            (map cross-listing-info courses)))
  (for/fold ([ht (hash)])
            ([names (in-list as-list)])
    (define preferred-name (choose-preferred-name names))
    (define other-names (remove preferred-name names))
    (for/fold ([ht ht])
              ([name (in-list other-names)])
      (when (and (hash-has-key? ht name)
                 (not equal? (hash-ref ht name) preferred-name))
        (error 'crosslistings
               "duplicated crosslisting for ~e: ~e and ~e"
               name
               (hash-ref ht name)
               preferred-name))
      (hash-set ht name preferred-name))))

;; given a list of names that are crosslisted, pick a "best" one,
;; using fad name if possible, otherwise defaulting
;; to prefer CPE to others and then to use the alphabetically earlier one.
(define (choose-preferred-name lon)
  (define fad-matches
    (for/list ([name (in-list lon)])
      (match-define (list prefix coursenum) name)
      (cond [(member name fad-courses) name]
            [(member (list prefix (~a "0" coursenum)) fad-courses) name]
            [else #f])))
  (define courses-in-fad-list
    (filter (λ (x) x) fad-matches))  
  (match courses-in-fad-list
    ['() (match (filter (λ (n) (equal? (first n) "CPE")) lon)
           ['()
            (define alpha-first
              (first (sort lon string<?
                           #:key (λ (n) (apply string-append n)))))
            (fprintf
             (current-error-port)
             "warning: using string sort to choose crosslisted name ~e from ~e\n"
             alpha-first lon)
            alpha-first]
           [(list cpe-course)
            (fprintf
             (current-error-port)
             "warning: using CPE preference to choose crosslisted name ~e from ~e\n"
             cpe-course lon)
            cpe-course]
           [(list _ ...) (raise-argument-error
                          'choose-preferred-name
                          "list containing at most one CPE course"
                          0 lon)])]
    [(list in-fad) in-fad]
    [(list _ ...)
     (raise-argument-error
      'choose-preferred-name
      "list containing at most one course in FAD list"
      0 lon)]))


(module+ test
  (require rackunit)
  
  (check-equal? (choose-preferred-name '(("CPE" "123") ("CSC" "123")))
                '("CPE" "123"))
  (check-equal? (choose-preferred-name '(("CSC" "400") ("EE" "997")))
                '("CSC" "400")))

(module+ main
  ;; read the html as sxml
  (define page-sxml
    (file->sxml
     "/Users/clements/clements/datasets/scheduling/calpoly-2015-2017-catalog-csc-courses.html"))

  (define courses
    (sxml->courses page-sxml))

  (define crosslistings
    (courses->crosslistings courses))

  (with-output-to-file "/tmp/crosslistings.rktd"
    (λ ()
      (pretty-write crosslistings))
    #:exists 'truncate))
