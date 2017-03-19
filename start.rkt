#lang typed/racket

(require racket/set racket/struct)

;; represents a course
(struct course ([dept : String] [number : String]) #:transparent)

;; represents a course requirement, as e.g. a graduation requirement for a particular program
(define-type Requirement (U exactly one-of all-of))

;; represents a requirement that a student take a particular course
(struct exactly ([take : course]) #:transparent)

;; represents a requirement that a student satisfy one of a set of requirements
(struct one-of ([reqs : (Listof Requirement)]) #:transparent)

;; represents a requirement that a student satisfy all of a set of requirements
(struct all-of ([reqs : (Listof Requirement)]) #:transparent)

;; represents a group of courses
(define-type course-set (Setof course))

;; checks whether a course-set meets a requirement
(: meets (-> course-set Requirement Boolean))
(define (meets courses req)
  (not (empty? (get-satisfying-courses courses req))))

;; get all subsets of courses that fully satisfy req
;; a result of empty set implies the provided courses cannot satisfy the requirement
(: get-satisfying-courses (-> course-set Requirement (Listof course-set)))
(define (get-satisfying-courses courses req)
  (cond
    [(exactly? req)
     (if (set-member? courses (exactly-take req))
         (list (set (exactly-take req)))
         empty)]
    [(one-of? req)
     (let ([reqs (one-of-reqs req)])
       (append-map (lambda ([req : Requirement]) (get-satisfying-courses courses req)) reqs))]
    [(all-of? req)
     (let ([reqs (all-of-reqs req)])
       (if (empty? reqs)
           ;; (all-of empty) requires no courses to satisfy
           (list (set))
           (let* ([this-requirement (first reqs)]
                  [satisfying-this (get-satisfying-courses courses this-requirement)]
                  [other-requirements (all-of (rest reqs))]
                  [satisfying-other (get-satisfying-courses courses other-requirements)])
             (map (lambda ([val : (Pairof course-set course-set)])
                    (set-union (car val) (cdr val)))
                  (filter
                   (lambda ([val : (Pairof course-set course-set)])
                     (set-discrete? (car val) (cdr val)))
                   (cross satisfying-this satisfying-other))))))]))

(: combine-two-class-counts (-> (HashTable course Integer) (HashTable course Integer) (HashTable course Integer)))
(define (combine-two-class-counts a b)
  (foldl
   (lambda ([new-value : (Pairof course Integer)] [current : (HashTable course Integer)])
     (hash-set
      current
      (car new-value)
      (+ (hash-ref current (car new-value) (lambda () 0)) (cdr new-value))))
   a
   (hash->list b)))

;; coerce the type checker
(: hashCI (-> (HashTable course Integer)))
(define (hashCI) (hash))

(: combine-many-class-counts (-> (Listof (HashTable course Integer)) (HashTable course Integer)))
(define (combine-many-class-counts lst)
  (foldl combine-two-class-counts (hashCI) lst))

;; counts the occurances of each course
(: get-class-counts (-> Requirement (HashTable course Integer)))
(define (get-class-counts req)
  (cond
    [(exactly? req)
     (hash (exactly-take req) 1)]
    [(one-of? req)
     (combine-many-class-counts (map get-class-counts (one-of-reqs req)))]
    [(all-of? req)
     (combine-many-class-counts (map get-class-counts (all-of-reqs req)))]))


;; gets the minimum number of classes a student has yet to take to satisfy a requirement
(: get-remaining-count (-> course-set Requirement Integer))
(define (get-remaining-count courses req)
  0)

;; returns every possible way to satisfy a requirement
(: get-all-options (-> Requirement (Listof course-set)))
(define (get-all-options req)
  (cond
    [(exactly? req)
     (list (set (exactly-take req)))]
    [(one-of? req)
     (let ([reqs (one-of-reqs req)])
       (append-map (lambda ([req : Requirement]) (get-all-options req)) reqs))]
    [(all-of? req)
     (let ([reqs (all-of-reqs req)])
       (if (empty? reqs)
           ;; (all-of empty) requires no courses to satisfy
           (list (set))
           (let* ([this-requirement (first reqs)]
                  [all-this (get-all-options this-requirement)]
                  [other-requirements (all-of (rest reqs))]
                  [all-other (get-all-options other-requirements)])
             (map (lambda ([val : (Pairof course-set course-set)])
                    (set-union (car val) (cdr val)))
                  ;; dont allow the same class twice
                  (filter
                   (lambda ([val : (Pairof course-set course-set)])
                     (set-discrete? (car val) (cdr val)))
                   (cross all-this all-other))))))]))

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

;; returns true iff a and b share no elements
(: set-discrete? (All (A) (-> (Setof A) (Setof A) Boolean)))
(define (set-discrete? a b)
  (set-empty? (set-intersect a b)))

;; returns the 'cross product' of two lists
(: cross (All (A B) (-> (Listof A) (Listof B) (Listof (Pair A B)))))
(define (cross a b)
  (if (empty? a)
      empty
      (append (map (lambda ([value : B]) (cons (first a) value)) b) (cross (rest a) b))))

;; Shorthand for creating a one of many classes requirement
(: group-any (-> String * one-of))
(define (group-any . values)
  (one-of (group-list values)))

(: group-all (-> String * all-of))
(define (group-all . values)
  (all-of (group-list values)))

(: group-list (-> (Listof String) (Listof exactly)))
(define (group-list vals)
  (if (empty? vals)
      empty
      (let ([dept (first vals)]
            [number (second vals)]
            [remaining (rest (rest vals))])
        (cons (exactly (course dept number)) (group-list remaining)))))

(provide
 get-satisfying-courses
 get-all-options course
 exactly
 one-of
 all-of
 powerset
 list-subtract
 discrete?
 cross
 group-all
 group-any
 combine-two-class-counts
 combine-many-class-counts
 get-class-counts)
