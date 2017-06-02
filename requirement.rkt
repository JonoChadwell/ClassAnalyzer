#lang typed/racket

(require "utilities.rkt" "types.rkt" racket/set racket/struct)

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

(: combine-two-class-counts (-> (HashTable course-id Integer) (HashTable course-id Integer) (HashTable course-id Integer)))
(define (combine-two-class-counts a b)
  (foldl
   (lambda ([new-value : (Pairof course-id Integer)] [current : (HashTable course-id Integer)])
     (hash-set
      current
      (car new-value)
      (+ (hash-ref current (car new-value) (lambda () 0)) (cdr new-value))))
   a
   (hash->list b)))

;; coerce the type checker
(: hashCI (-> (HashTable course-id Integer)))
(define (hashCI) (hash))

(: combine-many-class-counts (-> (Listof (HashTable course-id Integer)) (HashTable course-id Integer)))
(define (combine-many-class-counts lst)
  (foldl combine-two-class-counts (hashCI) lst))

;; counts the occurances of each course
(: get-class-counts (-> Requirement (HashTable course-id Integer)))
(define (get-class-counts req)
  (cond
    [(exactly? req)
     (hash (exactly-take req) 1)]
    [(one-of? req)
     (combine-many-class-counts (map get-class-counts (one-of-reqs req)))]
    [(all-of? req)
     (combine-many-class-counts (map get-class-counts (all-of-reqs req)))]))

(: strip-singletons (-> (HashTable course-id Integer) (HashTable course-id Integer)))
(define (strip-singletons table)
  (strip-singletons-recursive (hash->list table)))

(: strip-singletons-recursive (-> (Listof (Pairof course-id Integer)) (HashTable course-id Integer)))
(define (strip-singletons-recursive lst)
  (if (empty? lst)
      (hashCI)
      (if (eq? 1 (right (first lst)))
          (strip-singletons-recursive (rest lst))
          (hash-set (strip-singletons-recursive (rest lst)) (left (first lst)) (right (first lst))))))

;; adds _#val to the course number of a course. Makes no change if val is -1.
(: rename-course (-> course-id Integer course-id))
(define (rename-course crs val)
  (if (eq? val -1)
      crs
      (let ([postfix(string-append "_#" (number->string val))])
        (course-id (course-id-dept crs) (string-append (course-id-number crs) postfix)))))

;; Produces a requirement with all courses in req and table renamed to be unique
(: rename-courses (-> Requirement (HashTable course-id Integer) Requirement))
(define (rename-courses req table)
  (let* ([result ((rename-courses-monadic req) table)]
         [result-table (left result)]
         [result-nonzero
          (filter (lambda ([x : (Pairof course-id Integer)]) (not (eq? 0 (right x))))
                  (hash->list result-table))])
    (if (empty? result-nonzero)
        (right ((rename-courses-monadic req) table))
        (error "too few courses found"))))

(: rename-courses-monadic (-> Requirement (-> (HashTable course-id Integer) (Pairof (HashTable course-id Integer) Requirement))))
(define (rename-courses-monadic req)
  (lambda ([table : (HashTable course-id Integer)])
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
                            (lambda ([req : Requirement] [val : (Pairof (HashTable course-id Integer) (Listof Requirement))])
                              (let ([result ((rename-courses-monadic req) (left val))])
                                (pair (left result) (cons (right result) (right val)))))
                            (pair table empty)
                            reqs)])
         (pair (left fold-result) (one-of (right fold-result))))]
      [(all-of? req)
       (let* ([reqs (all-of-reqs req)]
              [fold-result (foldr
                            (lambda ([req : Requirement] [val : (Pairof (HashTable course-id Integer) (Listof Requirement))])
                              (let ([result ((rename-courses-monadic req) (left val))])
                                (pair (left result) (cons (right result) (right val)))))
                            (pair table empty)
                            reqs)])
         (pair (left fold-result) (all-of (right fold-result))))])))

;; Create all possible variants of a course set using the number of variants in table
(: explode-courses (-> course-set (Listof (Pairof course-id Integer)) (Listof course-set)))
(define (explode-courses courses lst)
  (if (empty? lst)
      (list courses)
      (let* ([replacement-course (left (first lst))]
             [replacement-count (right (first lst))]
             [others (explode-courses (set-remove courses replacement-course) (rest lst))]
             [new-courses (map
                           (lambda ([x : Integer]) (rename-course replacement-course (+ x 1)))
                           (range replacement-count))])
        (map (lambda ([p : (Pairof course-id course-set)])
               (set-add (right p) (left p)))
             (cross new-courses others)))))

;; "dedupes" a requirement by exploding all like classes into different instances of the same class
(: deduplicate-courses (-> course-set Requirement (Pairof (Listof course-set) Requirement)))
(define (deduplicate-courses courses req)
  (let* ([counts
          (make-immutable-hash
           (filter
            (lambda ([x : (Pairof course-id Integer)]) (set-member? courses (left x)))
            (hash->list (strip-singletons (get-class-counts req)))))]
         [new-req (rename-courses req counts)]
         [new-classes (explode-courses courses (hash->list counts))])
    (pair new-classes new-req)))

;; gets the minimum number of classes a student has yet to take to satisfy a requirement
;; req must contain no duplicated courses
(: get-minimum-cost-to-satisfy-deduped (-> (-> course-id Integer) course-set Requirement Integer))
(define (get-minimum-cost-to-satisfy-deduped cost courses req)
  (cond
    [(exactly? req)
     (if (set-member? courses (exactly-take req))
         0
         (cost (exactly-take req)))]
    [(one-of? req)
     (let ([reqs (one-of-reqs req)])
       (min-list
        (map
         (lambda ([r : Requirement]) (get-minimum-cost-to-satisfy-deduped cost courses r))
         reqs)))]
    [(all-of? req)
     (let ([reqs (all-of-reqs req)])
       (sum-list
        (map
         (lambda ([r : Requirement]) (get-minimum-cost-to-satisfy-deduped cost courses r))
         reqs)))]))

;; gets the minimum number of classes a student has yet to take to satisfy a requirement
(: get-minimum-cost-to-satisfy (-> (-> course-id Integer) course-set Requirement Integer))
(define (get-minimum-cost-to-satisfy cost courses req)
  (let* ([result (deduplicate-courses courses req)]
         [possible-courses (left result)]
         [new-req (right result)])
    (min-list (map
               (lambda ([courses : course-set])
                 (get-minimum-cost-to-satisfy-deduped cost courses new-req))
               possible-courses))))

;; gets the minimum number of classes a student has yet to take to satisfy a requirement
(: get-remaining-count (-> course-set Requirement Integer))
(define (get-remaining-count courses req)
  (get-minimum-cost-to-satisfy (lambda ([x : course-id]) 1) courses req))

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

;; Shorthand for creating a one-of classes requirement
(: group-any (-> String * one-of))
(define (group-any . values)
  (one-of (group-list values)))

;; Shorthand for creating an all-of classes requirement
(: group-all (-> String * all-of))
(define (group-all . values)
  (all-of (group-list values)))

;; Shorthand for creating a list of exactly requirements
(: group-list (-> (Listof String) (Listof exactly)))
(define (group-list vals)
  (if (empty? vals)
      empty
      (let ([dept (first vals)]
            [number (second vals)]
            [remaining (rest (rest vals))])
        (cons (exactly (course-id dept number)) (group-list remaining)))))

(module+ test
  (require typed/rackunit)
  
  (define CPE101 (course-id "CPE" "101"))
  (define CPE102 (course-id "CPE" "102"))
  (define CPE103 (course-id "CPE" "103"))
  (define CPE225 (course-id "CPE" "225"))
  (define CPE357 (course-id "CPE" "357"))

  ;; meets tests
  (check-true
   (meets
    (set CPE101)
    (exactly CPE101)))
  
  (check-false
   (meets
    (set CPE101 CPE102 CPE225)
    (all-of (list
             (one-of (list
                      (exactly CPE101)
                      (exactly CPE102)))
             (exactly CPE357)))))
  
  (check-true
   (meets
    (set CPE101 CPE102 CPE225 CPE357)
    (all-of (list
             (one-of (list
                      (exactly CPE101)
                      (exactly CPE102)))
             (exactly CPE357)))))

  ;; get-satisfying-courses tests
  (check-equal?
   (get-satisfying-courses
    (set)
    (exactly CPE101))
   empty)

  (check-equal?
   (get-satisfying-courses
    (set CPE101)
    (exactly CPE101))
   (list (set CPE101)))

  (check-equal?
   (get-satisfying-courses
    (set CPE101)
    (one-of (list (exactly CPE101) (exactly CPE102))))
   (list (set CPE101)))

  (check-equal?
   (get-satisfying-courses
    (set CPE101 CPE102)
    (one-of (list (exactly CPE101) (exactly CPE102))))
   (list (set CPE101) (set CPE102)))

  (check-equal?
   (get-satisfying-courses
    (set CPE101 CPE102)
    (one-of (list (exactly CPE102))))
   (list (set CPE102)))

  (check-equal?
   (get-satisfying-courses
    (set CPE101 CPE102)
    (all-of (list (exactly CPE101) (exactly CPE102))))
   (list (set CPE101 CPE102)))

  (check-equal?
   (get-satisfying-courses
    (set CPE101 CPE102)
    (all-of (list (exactly CPE101) (exactly CPE102) (exactly CPE103))))
   empty)

  (check-equal?
   (get-satisfying-courses
    (set CPE101 CPE102)
    (all-of (list (exactly CPE101) (exactly CPE102) (exactly CPE103))))
   empty)

  (check-equal?
   (get-satisfying-courses
    (set CPE101 CPE102 CPE103 CPE357)
    (all-of (list (exactly CPE101) (exactly CPE102) (exactly CPE103))))
   (list (set CPE101 CPE102 CPE103)))

  ;; get-all-options tests
  (check-equal?
   (get-all-options
    (exactly CPE101))
   (list (set CPE101)))

  (check-equal?
   (get-all-options
    (one-of (list (exactly CPE101) (exactly CPE102))))
   (list (set CPE101) (set CPE102)))

  (check-equal?
   (get-all-options
    (one-of (list (exactly CPE102))))
   (list (set CPE102)))

  (check-equal?
   (get-all-options
    (all-of (list (exactly CPE101) (exactly CPE102))))
   (list (set CPE101 CPE102)))

  (check-equal?
   (get-all-options
    (all-of (list (exactly CPE101) (exactly CPE102) (exactly CPE103))))
   (list (set CPE101 CPE102 CPE103)))

  (check-equal?
   (get-all-options
    (one-of (list
             (all-of (list (exactly CPE101) (exactly CPE357)))
             (all-of (list (exactly CPE102) (exactly CPE103))))))
   (list (set CPE101 CPE357) (set CPE102 CPE103)))

  ;; powerset result lists are backwards because I am too lazy to find a better way
  (check-not-false (member empty (powerset (list 1 2 3))))
  (check-not-false (member (list 1) (powerset (list 1 2 3))))
  (check-not-false (member (list 2) (powerset (list 1 2 3))))
  (check-not-false (member (list 3) (powerset (list 1 2 3))))
  (check-not-false (member (list 2 1) (powerset (list 1 2 3))))
  (check-not-false (member (list 3 1) (powerset (list 1 2 3))))
  (check-not-false (member (list 3 2) (powerset (list 1 2 3))))
  (check-not-false (member (list 3 2 1) (powerset (list 1 2 3))))

  (check-true (discrete? (list 1 2 3) (list 4 5 6)))
  (check-true (discrete? (list 1 2 3) (list 5 6 8)))
  (check-false (discrete? (list 1 2 3) (list 5 4 3)))
  (check-false (discrete? (list 1 2 3) (list 3 4 5)))
  (check-false (discrete? (list 1 2 3) (list 3 3 3)))
  (check-false (discrete? (list 1 2 3) (list 5 4 1)))
  (check-false (discrete? (list 1 2 3) (list 1 4 5)))

  (check-equal? (list-subtract (list 1 2 3 4 5) (list 2 4 5)) (list 1 3))

  (check-equal? (cross (list 1 2 3) empty) empty)
  (check-equal? (cross empty (list 1 2 3)) empty)
  (check-equal? (cross (list 1 2) (list 3 4)) (list (cons 1 3) (cons 1 4) (cons 2 3) (cons 2 4)))

  (check-equal?
   (group-any "ENGL" "133" "ENGL" "134")
   (one-of (list
            (exactly (course-id "ENGL" "133"))
            (exactly (course-id "ENGL" "134")))))
  (check-equal?
   (group-all "ENGL" "133" "ENGL" "134")
   (all-of (list
            (exactly (course-id "ENGL" "133"))
            (exactly (course-id "ENGL" "134")))))

  ;; combine class counts tests
  (check-equal?
   (combine-two-class-counts (hash CPE101 1) (hash CPE101 2))
   (hash CPE101 3))

  (check-equal?
   (combine-two-class-counts (hash CPE101 3) (hash CPE102 2))
   (hash CPE101 3 CPE102 2))

  (check-equal?
   (combine-two-class-counts (hash CPE101 1 CPE102 3) (hash CPE101 1 CPE103 4))
   (hash CPE101 2 CPE102 3 CPE103 4))

  (check-equal?
   (combine-many-class-counts (list (hash CPE101 1 CPE102 3) (hash CPE101 1 CPE103 4) (hash CPE101 1 CPE357 5)))
   (hash CPE101 3 CPE102 3 CPE103 4 CPE357 5))

  ;; get class counts tests
  (check-equal?
   (get-class-counts (all-of (list
                              (one-of (list (exactly CPE101) (exactly CPE102)))
                              (one-of (list (exactly CPE102) (exactly CPE103))))))
   (hash CPE101 1 CPE102 2 CPE103 1))

  ;; strip singletons tests
  (check-equal?
   (strip-singletons (hash CPE101 1 CPE102 2 CPE103 1))
   (hash CPE102 2))

  ;; rename courses tests
  (check-equal?
   (rename-courses (exactly CPE101) (hash CPE101 1))
   (exactly (course-id "CPE" "101_#1")))

  (check-equal?
   (rename-courses (exactly CPE101) (hash CPE102 0))
   (exactly (course-id "CPE" "101")))

  (check-exn
   exn:fail?
   (lambda () (rename-courses (exactly CPE101) (hash CPE102 5)))
   "too few courses found")

  (check-equal?
   (rename-courses (all-of (list (exactly CPE101) (exactly CPE102))) (hash))
   (all-of (list (exactly CPE101) (exactly CPE102))))

  (check-equal?
   (rename-courses (all-of (list (exactly CPE101) (exactly CPE101))) (hash CPE101 2))
   (all-of (list (exactly (course-id "CPE" "101_#1")) (exactly (course-id "CPE" "101_#2")))))

  (check-exn
   exn:fail?
   (lambda () (rename-courses (all-of (list (exactly CPE101) (exactly CPE101))) (hash CPE101 1)))
   "too many courses found")

  (check-equal?
   (rename-courses (all-of (list
                            (one-of (list (exactly CPE101) (exactly CPE101)))
                            (one-of (list (exactly CPE101) (exactly CPE101)))))
                   (hash CPE101 4))
   (all-of (list
            (one-of (list (exactly (course-id "CPE" "101_#1")) (exactly (course-id "CPE" "101_#2"))))
            (one-of (list (exactly (course-id "CPE" "101_#3")) (exactly (course-id "CPE" "101_#4")))))))

  ;; explode courses tests
  (check-equal?
   (explode-courses (set CPE101 CPE102 CPE103) (hash->list (hash CPE101 3)))
   (list (set (course-id "CPE" "101_#1") CPE102 CPE103)
         (set (course-id "CPE" "101_#2") CPE102 CPE103)
         (set (course-id "CPE" "101_#3") CPE102 CPE103)))

  (check-equal?
   (list->set (explode-courses (set CPE101 CPE102 CPE103) (hash->list (hash CPE101 3 CPE102 2))))
   (set (set (course-id "CPE" "101_#1") (course-id "CPE" "102_#1") CPE103)
        (set (course-id "CPE" "101_#1") (course-id "CPE" "102_#2") CPE103)
        (set (course-id "CPE" "101_#2") (course-id "CPE" "102_#1") CPE103)
        (set (course-id "CPE" "101_#2") (course-id "CPE" "102_#2") CPE103)
        (set (course-id "CPE" "101_#3") (course-id "CPE" "102_#1") CPE103)
        (set (course-id "CPE" "101_#3") (course-id "CPE" "102_#2") CPE103)))

  ;; deduplicate-courses tests
  (check-equal?
   (deduplicate-courses
    (set CPE101 CPE102)
    (all-of (list
             (one-of (list (exactly CPE101) (exactly CPE102)))
             (one-of (list (exactly CPE101) (exactly CPE103))))))
   (pair
    (list (set (course-id "CPE" "101_#1") CPE102)
          (set (course-id "CPE" "101_#2") CPE102))
    (all-of (list
             (one-of (list (exactly (course-id "CPE" "101_#1")) (exactly CPE102)))
             (one-of (list (exactly (course-id "CPE" "101_#2")) (exactly CPE103)))))))

  ;; get-remaining-count tests
  (check-equal?
   (get-remaining-count
    (set CPE101 CPE102)
    (all-of (list
             (one-of (list (exactly CPE101) (exactly CPE102)))
             (one-of (list (exactly CPE101) (exactly CPE103))))))
   0)

  (check-equal?
   (get-remaining-count
    (set CPE101)
    (all-of (list
             (one-of (list (exactly CPE101) (exactly CPE102)))
             (one-of (list (exactly CPE101) (exactly CPE103))))))
   1)

  (check-equal?
   (get-remaining-count
    (set)
    (all-of (list
             (one-of (list (exactly CPE101) (exactly CPE102)))
             (one-of (list (exactly CPE101) (exactly CPE103))))))
   2)

  (check-equal?
   (get-remaining-count
    (set CPE103)
    (all-of (list
             (one-of (list (exactly CPE101) (exactly CPE102)))
             (all-of (list (exactly CPE103) (exactly CPE357))))))
   2))

(provide
 meets
 get-satisfying-courses
 get-all-options course
 powerset
 list-subtract
 discrete?
 cross
 group-all
 group-any
 strip-singletons
 rename-courses
 explode-courses
 deduplicate-courses
 get-remaining-count
 get-minimum-cost-to-satisfy
 get-class-counts)
