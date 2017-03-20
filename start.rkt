#lang typed/racket

(require "utilities.rkt" racket/set racket/struct)

;; represents a course
;; Number may include _###, which is used to represent a "variant" of a course.
;; this is done to explode requirements that use the same course twice into easier
;; to manage requirements with no double duplicated courses.
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

(: strip-singletons (-> (HashTable course Integer) (HashTable course Integer)))
(define (strip-singletons table)
  (strip-singletons-recursive (hash->list table)))

(: strip-singletons-recursive (-> (Listof (Pairof course Integer)) (HashTable course Integer)))
(define (strip-singletons-recursive lst)
  (if (empty? lst)
      (hashCI)
      (if (eq? 1 (right (first lst)))
          (strip-singletons-recursive (rest lst))
          (hash-set (strip-singletons-recursive (rest lst)) (left (first lst)) (right (first lst))))))

;; adds _#val to the course number of a course. Makes no change if val is -1.
(: rename-course (-> course Integer course))
(define (rename-course crs val)
  (if (eq? val -1)
      crs
      (let ([postfix(string-append "_#" (number->string val))])
        (course (course-dept crs) (string-append (course-number crs) postfix)))))

;; Produces a requirement with all courses in req and table renamed to be unique
(: rename-courses (-> Requirement (HashTable course Integer) Requirement))
(define (rename-courses req table)
  (let* ([result ((rename-courses-monadic req) table)]
         [result-table (left result)]
         [result-nonzero
          (filter (lambda ([x : (Pairof course Integer)]) (not (eq? 0 (right x))))
                  (hash->list result-table))])
    (if (empty? result-nonzero)
        (right ((rename-courses-monadic req) table))
        (error "too few courses found"))))

(: rename-courses-monadic (-> Requirement (-> (HashTable course Integer) (Pairof (HashTable course Integer) Requirement))))
(define (rename-courses-monadic req)
  (lambda ([table : (HashTable course Integer)])
    (cond
      [(exactly? req)
       (let* ([crs (exactly-take req)]
              [val (hash-ref table crs (lambda () -1))]
              [new-table (if (eq? -1 val) table (hash-set table crs (- val 1)))]
              [new-req (exactly (rename-course crs val))])
         (if (eq? val 0)
             (error "too many courses found")
             (pair new-table new-req)))]
      [(one-of? req)
       (let* ([reqs (one-of-reqs req)]
              [fold-result (foldr
                            (lambda ([req : Requirement] [val : (Pairof (HashTable course Integer) (Listof Requirement))])
                              (let ([result ((rename-courses-monadic req) (left val))])
                                (pair (left result) (cons (right result) (right val)))))
                            (pair table empty)
                            reqs)])
         (pair (left fold-result) (one-of (right fold-result))))]
      [(all-of? req)
       (let* ([reqs (all-of-reqs req)]
              [fold-result (foldr
                            (lambda ([req : Requirement] [val : (Pairof (HashTable course Integer) (Listof Requirement))])
                              (let ([result ((rename-courses-monadic req) (left val))])
                                (pair (left result) (cons (right result) (right val)))))
                            (pair table empty)
                            reqs)])
         (pair (left fold-result) (all-of (right fold-result))))])))

;; Create all possible variants of a course set using the number of variants in table
(: explode-courses (-> course-set (Listof (Pairof course Integer)) (Listof course-set)))
(define (explode-courses courses lst)
  (if (empty? lst)
      (list courses)
      (let* ([replacement-course (left (first lst))]
             [replacement-count (right (first lst))]
             [others (explode-courses (set-remove courses replacement-course) (rest lst))]
             [new-courses (map
                           (lambda ([x : Integer]) (rename-course replacement-course (+ x 1)))
                           (range replacement-count))])
        (map (lambda ([p : (Pairof course course-set)])
               (set-add (right p) (left p)))
             (cross new-courses others)))))

;; "dedupes" a requirement by exploding all like classes into different instances of the same class
(: deduplicate-courses (-> course-set Requirement (Pairof (Listof course-set) Requirement)))
(define (deduplicate-courses courses req)
  (let* ([counts
          (make-immutable-hash
           (filter
            (lambda ([x : (Pairof course Integer)]) (set-member? courses (left x)))
            (hash->list (strip-singletons (get-class-counts req)))))]
         [new-req (rename-courses req counts)]
         [new-classes (explode-courses courses (hash->list counts))])
    (pair new-classes new-req)))

;; gets the minimum number of classes a student has yet to take to satisfy a requirement
;; req must contain no duplicated courses
(: get-remaining-count-deduped (-> course-set Requirement Integer))
(define (get-remaining-count-deduped courses req)
  (cond
    [(exactly? req)
     (if (set-member? courses (exactly-take req))
         0
         1)]
    [(one-of? req)
     (let ([reqs (one-of-reqs req)])
       (min-list
        (map
         (lambda ([r : Requirement]) (get-remaining-count-deduped courses r))
         reqs)))]
    [(all-of? req)
     (let ([reqs (all-of-reqs req)])
       (sum-list
        (map
         (lambda ([r : Requirement]) (get-remaining-count-deduped courses r))
         reqs)))]))

;; gets the minimum number of classes a student has yet to take to satisfy a requirement
(: get-remaining-count (-> course-set Requirement Integer))
(define (get-remaining-count courses req)
  (let* ([result (deduplicate-courses courses req)]
         [possible-courses (left result)]
         [new-req (right result)])
    (min-list (map
               (lambda ([courses : course-set])
                 (get-remaining-count-deduped courses new-req))
               possible-courses))))

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

;; Returns true when a class would progress a student towards graduation
(: helps-student (-> course-set Requirement course Boolean))
(define (helps-student courses req crs)
  (< (get-remaining-count (set-add courses crs) req) (get-remaining-count courses req)))

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
 strip-singletons
 rename-courses
 explode-courses
 deduplicate-courses
 get-remaining-count
 get-class-counts
 helps-student)
