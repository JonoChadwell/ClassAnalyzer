#lang typed/racket

;; represents a course
(struct course ([dept : String] [number : String]))

;; represents a course requirement, as e.g. a graduation requirement for a particular program
(define-type Requirement (U exactly one-of all-of))

;; represents a requirement that a student take a particular course
(struct exactly ([take : course]))

;; represents a requirement that a student satisfy one of a set of requirements
(struct one-of ([list : (Listof Requirement)]))

;; represents a requirement that a student satisfy all of a set of requirements
(struct all-of ([list : (Listof Requirement)]))

(define-type course-set (Listof course))

;; (: satisfies (-> requirement (Listof course)))
;; (define (satisfies list

(define courses (list (course "CE" "100") (course "CE" "101") (course "CE" "102")))

;; get all subsets of courses that fully satisfy req
;; a result of empty list implies the provided courses cannot satisfy the requirement
(: get-satisfying-courses (-> (Listof course) Requirement (Listof course-set)))
(define (get-satisfying-courses courses req)
  (cond
    [(exactly? req)
     (if (member (exactly-take req) courses)
         (list (list (exactly-take req)))
         empty)]
    [(one-of? req)
     (let ([reqs (one-of-list req)])
       (append-map (lambda ([req : Requirement]) (get-satisfying-courses courses req)) reqs))]
    [(all-of? req) ;; empty]))

     (let ([reqs (all-of-list req)])
       (if (empty? reqs)
           ;; (all-of empty) requires no courses to satisfy
           (list empty)
           (let* ([this-requirement (first reqs)]
                  [satisfying-this (get-satisfying-courses courses this-requirement)]
                  [other-requirements (all-of (rest reqs))]
                  [satisfying-other (get-satisfying-courses courses other-requirements)])
             
             (map (lambda ([val : (Pairof course-set course-set)])
                    (append (car val) (cdr val)))
                  (filter
                   (lambda ([val : (Pairof course-set course-set)])
                     (discrete? (car val) (cdr val)))
                   (cross satisfying-this satisfying-other))))))]))

(: powerset (All (A) (-> (Listof A) (Listof (Listof A)))))
(define (powerset lst)
  (powerset-recursive empty lst))

(: powerset-recursive (All (A) (-> (Listof A) (Listof A) (Listof (Listof A)))))
(define (powerset-recursive elements lst)
  (if (empty? lst)
      (list elements)
      (append
       (powerset-recursive elements (rest lst))
       (powerset-recursive (cons (first lst) elements) (rest lst)))))

;; Returns all elements in from and not in values
(: list-subtract (All (A) (-> (Listof A) (Listof A) (Listof A))))
(define (list-subtract from values)
  (if (empty? from)
      empty
      (let ([this (first from)]
            [remaining (list-subtract (rest from) values)])
        (if (member this values)
            remaining
            (cons this remaining)))))

;; returns true iff a and b share no elements
(: discrete? (All (A) (-> (Listof A) (Listof A) Boolean)))
(define (discrete? a b)
  (if (empty? a)
      #t
      (if (member (first a) b)
          #f
          (discrete? (rest a) b))))

;; returns the 'cross product' of two lists
(: cross (All (A B) (-> (Listof A) (Listof B) (Listof (Pair A B)))))
(define (cross a b)
  (if (empty? a)
      empty
      (append (map (lambda ([value : B]) (cons (first a) value)) b) (cross (rest a) b))))

(provide get-satisfying-courses course exactly one-of all-of powerset list-subtract discrete? cross)