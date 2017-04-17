#lang racket

;; given the HTML from the course listing for a subject in the
;; Cal Poly online course catalog, extract information on
;; courses, prerequisites, and cross-listing

(require explorer
         graph
         racket/runtime-path
         "crosslistings.rkt"
         "parse-catalog-html.rkt")

(define-runtime-path HERE ".")


;; given a course, return the title block and the prerequisites
(define (process-course sxml)
  (match-define (list subj num descr)
    (course-title sxml))
  (list (append (canonicalize-name (list subj num)) (list descr))
        (canonicalize-prereq
         (course-prereqs sxml))))

(define (canonicalize-prereq p)
  (match p
    [#f #f]
    [else
     (match-define (list english course-decls) p)
     (list english
           (map canonicalize-name course-decls))]))

;; map, e.g., '("CSC" "400") to "CSC-400"
(define (course-str c)
  (string-append (first c) (second c)))

(define (canonicalize-name n)
  (hash-ref crosslisting-hash n n))


(define cool-color '(95/360 65/100 91/100))
(define hot-color '(21/360 96/100 99/100))
;; given a number between 0 and 1, produce an h,s,v list
;; so that 0 returns the cool color and 1.0 the hot
(define (color-map frac)
  (for/list ([limit1 (in-list cool-color)]
             [limit2 (in-list hot-color)])
    (+ limit1 (* (- limit2 limit1) frac))))

;; given a list of three numbers, return a string
;; in the format used by 'dot' for an H,S,V color
(define (hsv->str l)
  (apply format "\"~a,~a,~a\""
         (map exact->inexact l)))

(module+ test
  (require rackunit)
  (check-equal? (color-map 0) cool-color)
  (check-equal? (color-map 1) hot-color))


#|
#;(module+ main
  ;; read the html as sxml
  (define page-sxml
    (file->sxml
     (build-path "/Users/clements/clements/datasets/scheduling/"
                 "calpoly-2015-2017-catalog-csc-courses.html")))

  (define courses
    (sxml->courses page-sxml))

  (define processed
    (map process-course courses))

  processed

  (define nodes (map (λ (e) (take (first e) 2))
                     processed))

  (define conn
    (postgresql-connect #:port 13432
                        #:user "fad"
                        #:password "aoeuidht"
                        #:database "fad"))

  (define weight-query-result 
    (map (λ (v) (list (list (vector-ref v 0)
                            (match (vector-ref v 1)
                              [(regexp #px"^0(.*)" (list _ n)) n]))
                      (vector-ref v 2)))
         (query-rows
          conn
          (~a "SELECT subject,num,SUM(enrollment) FROM offerings "
              " WHERE qtr >= 2108"
              " GROUP BY subject,num;"))))
  

  (define weight-hash
    (for/hash ([n (in-list processed)])
      (values (take (first n) 2)
              (log
               (max 0.5
                    (match (dict-ref weight-query-result
                                     (take (first n) 2)
                                     (list 0))
                      ['() 0]
                      [(list n) n]
                      [other (error 'oh-dear)]))))))
  

  (define max-weight
    (apply max
           (for/list ([n (in-list nodes)])
             (hash-ref weight-hash n))))

  (printf "maximum weight: ~v\n" max-weight)
  
  (define edges
    (apply
      append
      (for/list ([course (in-list processed)])
        (match course
          [`((,dept ,num ,_1)
             (,_2 ,prereqs))
           (for/list ([prereq (in-list prereqs)])
             (list (course-str (list dept num))
                   (course-str prereq)))]
          [`((,dept ,num ,_1)
             #f)
           '()]))))

  (call-with-output-file "/tmp/foo2.graphviz"
    (λ (port)
      (fprintf port "digraph G {\n")
      (fprintf port "  rankdir=RL;\n")
      (fprintf port "  node [style=filled];\n")
      (for ([n (in-list nodes)])
        (fprintf port "  ~a [fillcolor = ~a];\n"
                 (course-str n)
                 (hsv->str (color-map
                            (/ (hash-ref weight-hash n)
                               max-weight)))))
      (fprintf port "  subgraph D {\n")
      (for ([e (in-list edges)])
        (fprintf port "    ~a -> ~a;\n"
                 (first e) (second e)))
      (fprintf port "  }\n")
      (fprintf port "}\n"))
    #:exists 'truncate)
  
  (display-to-file
   (graphviz
    (unweighted-graph/directed
     (apply
      append
      (for/list ([course (in-list processed)])
        (match course
          [`((,dept ,num ,_1)
             (,_2 ,prereqs))
           (for/list ([prereq (in-list prereqs)])
             (list (course-str (list dept num))
                   (course-str prereq)))]
          [`((,dept ,num ,_1)
             #f)
           '()]))))
    )
   "/tmp/foo.graphviz"
   #:exists 'truncate))
|#

;;div with class courses
#;(module+ main
    ;; code used once to canonicalize decls after annotation.

  (define (canonicalize-decl decl)
    (match decl
      [(list 'or sub-decls ...)
       (cons 'or (map canonicalize-decl sub-decls))]
      [(list 'and sub-decls ...)
       (cons 'and (map canonicalize-decl sub-decls))]
      ['other 'other]
      [(list (? string? s1) (? string? s2))
       (hash-ref crosslisting-hash decl decl)]))

  (for/list ([d (in-list csc-dependencies)])
    (match d
      [(list (list dept num descr)
             (list engl-prereq-text
                   deps-decl))
       (define canonical-name
         (hash-ref crosslisting-hash (list dept num) (list dept num)))
       (list (append canonical-name (list descr))
             (list engl-prereq-text
                   (map canonicalize-decl deps-decl)))]
      [(list (list dept num descr)
             #f)
       (define canonical-name
         (hash-ref crosslisting-hash (list dept num) (list dept num)))
       (list (append canonical-name (list descr))
             #f)])))