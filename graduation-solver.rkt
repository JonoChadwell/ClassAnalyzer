#lang typed/racket

(require "types.rkt" "utilities.rkt" "course.rkt" "quarter.rkt")

;; TODO properly handle concurrent class requirements
;; TODO properly handle per quarter timing constraints
;; TODO make schedules that have no more than 16 units per quarter

;; A course tree represents a course and the minimal prereqs a student can get to take each node
;; for example, (node (crs "CSC" "102") (list (node (crs "CSC" "101") empty))) would be the course tree
;; of csc 102 for a student who has not taken either 101 or 102.
(struct course-tree
  ([crs : course-id]
   [children : (Listof course-tree)])
  #:transparent)

(: course-tree-leaf (-> course-id course-tree))
(define (course-tree-leaf crs)
  (course-tree crs empty))

(: minimum-graduation-quarter (-> course-set curriculum Quarter Quarter))
(define (minimum-graduation-quarter courses major qtr)
  (tree-complete-quarter
   qtr
   (slowest-tree qtr (build-course-trees qtr
                                         (curriculum-requirements major)
                                         courses))))

;; Returns a set of course trees a student could use to graduate, favoring subtrees
;; which can be completed sooner.
(: build-course-trees (-> Quarter Requirement course-set (Listof course-tree)))
(define (build-course-trees qtr req courses)
  (cond
    [(exactly? req)
     (let* ([crs (exactly-take req)])
       (if (set-member? courses crs)
           empty
           (list (course-tree
                  crs
                  (build-course-trees
                   qtr
                   (course-prereqs (course-id->course crs))
                   courses)))))]
    [(one-of? req)
     (let* ([options (one-of-reqs req)]
            [option-trees (map
                           (lambda ([x : Requirement])
                             (build-course-trees qtr x courses))
                           options)])
       (quickest-trees qtr option-trees))]
    [(all-of? req)
     (append-map
      (lambda ([x : Requirement])
        (build-course-trees qtr x courses))
      (all-of-reqs req))]))
            


;; Returns the earliest possible quarter in which the top class in the tree can be taken
(: tree-complete-quarter (-> Quarter course-tree Quarter))
(define (tree-complete-quarter qtr tree)
  (if (empty? (course-tree-children tree))
      (course-next-offered (course-tree-crs tree) qtr)
      (course-next-offered
       (course-tree-crs tree)
       (quarter-after
        (max-list (map (lambda ([x : course-tree]) (tree-complete-quarter qtr x))
                       (course-tree-children tree)))))))

;; Returns the quickest course tree to complete out of a list
(: quickest-tree (-> Quarter (Listof course-tree) course-tree))
(define (quickest-tree qtr trees)
  (cond
    [(empty? trees) (error "no quickest tree in empty list")]
    [(empty? (rest trees)) (first trees)]
    [else
     (let ([current (first trees)]
           [best (quickest-tree qtr (rest trees))])
       (if (< (tree-complete-quarter qtr current) (tree-complete-quarter qtr best))
           current
           best))]))

;; Returns the longest course tree to complete out of a list
(: slowest-tree (-> Quarter (Listof course-tree) course-tree))
(define (slowest-tree qtr trees)
  (cond
    [(empty? trees) (error "no quickest tree in empty list")]
    [(empty? (rest trees)) (first trees)]
    [else
     (let ([current (first trees)]
           [worst (slowest-tree qtr (rest trees))])
       (if (> (tree-complete-quarter qtr current) (tree-complete-quarter qtr worst))
           current
           worst))]))

;; returns the quickest list of course trees out of a list of list of course trees
(: quickest-trees (-> Quarter (Listof (Listof course-tree)) (Listof course-tree)))
(define (quickest-trees qtr tree-lists)
  (cond
    [(empty? tree-lists) (error "no quickest list of trees in empty list")]
    [(empty? (rest tree-lists)) (first tree-lists)]
    [else
     (let ([current (first tree-lists)])
       (if (empty? current)
           empty
           (let ([best (quickest-trees qtr (rest tree-lists))])
             (if (< (tree-complete-quarter qtr (slowest-tree qtr current)) (tree-complete-quarter qtr (slowest-tree qtr best)))
                 current
                 best))))]))

(: combine-schedules (-> (HashTable Quarter course-set) (HashTable Quarter course-set) (HashTable Quarter course-set)))
(define (combine-schedules a b)
  (combine-schedules-recursive (hash->list a) b))

(: combine-schedules-recursive (-> (Listof (Pairof Quarter course-set)) (HashTable Quarter course-set) (HashTable Quarter course-set)))
(define (combine-schedules-recursive lst b)
  (if (empty? lst)
      b
      (let* ([remaining (combine-schedules-recursive (rest lst) b)]
             [this-quarter (left (first lst))]
             [these-courses (right (first lst))]
             [combined-courses (set-union these-courses
                                          (hash-ref b this-quarter
                                                    (lambda () (cast (set) course-set))))])
        (hash-set remaining this-quarter combined-courses))))

;; sorts all classes in a tree based on number of prerequisistes
;; currently unused
(: level-sort-tree (-> course-tree (Listof course-set)))
(define (level-sort-tree tree)
  (reverse (level-sort-tree-recursive tree)))

(: combine-two-lists (-> (Listof course-set) (Listof course-set) (Listof course-set)))
(define (combine-two-lists left right)
  (let* ([leftset (if (empty? left) (cast (set) course-set) (first left))]
         [rightset (if (empty? right) (cast (set) course-set) (first right))]
         [newset (set-union leftset rightset)]
         [recurse (not (and (empty? left) (empty? right)))]
         [leftrem (if (empty? left) empty (rest left))]
         [rightrem (if (empty? right) empty (rest right))])
    (if recurse
        (cons newset (combine-two-lists leftrem rightrem))
        empty)))

(: combine-lists (-> (Listof (Listof course-set)) (Listof course-set)))
(define (combine-lists lists)
  (foldl
   combine-two-lists
   empty
   lists))

(: level-sort-tree-recursive (-> course-tree (Listof course-set)))
(define (level-sort-tree-recursive tree)
  (cons (set (course-tree-crs tree))
        (combine-lists (map level-sort-tree-recursive (course-tree-children tree)))))

(: emptyschedule (HashTable Quarter course-set))
(define emptyschedule (hash))

(: pack-course-tree (-> Quarter course-tree (HashTable Quarter course-set)))
(define (pack-course-tree qtr tree)
  (let* ([top (course-tree-crs tree)]
         [children (course-tree-children tree)]
         [endqtr (tree-complete-quarter qtr tree)]
         [children-schedule (foldl
                             combine-schedules
                             emptyschedule
                             (map (curry pack-course-tree qtr) children))]
         [current-quarter (hash-ref children-schedule endqtr (lambda () (cast (set) course-set)))])
    (hash-set children-schedule endqtr (set-add current-quarter top))))
    

;; packs all classes into a schedule attempting to produce a schedule that
;; finishes all courses as soon as possible. Does not respect petty human limits
;; such as 16 units per quarter (these will be added in later)
(: pack-classes (-> Quarter (Setof course-tree) (HashTable Quarter course-set)))
(define (pack-classes qtr trees)
  (foldl combine-schedules emptyschedule (map (curry pack-course-tree qtr) (set->list trees))))

(module+ test

  (require typed/rackunit "test-data.rkt")

  (define fast-tree (course-tree-leaf FAKE_101))
  (define middle-tree (course-tree FAKE_102 (list fast-tree)))
  (define slow-tree (course-tree FAKE_151 (list middle-tree)))

  (check-equal?
   (tree-complete-quarter
    2002
    fast-tree) 2002)

  (check-equal?
   (tree-complete-quarter
    2002
    slow-tree) 2012)

  (check-equal?
   (quickest-tree 2002
                  (list
                   (course-tree FAKE_151 (list (course-tree-leaf FAKE_101)))
                   (course-tree FAKE_102 (list (course-tree-leaf FAKE_101)))))
   (course-tree FAKE_102 (list (course-tree-leaf FAKE_101))))

  (check-equal?
   (quickest-tree 2002
                  (list fast-tree middle-tree slow-tree))
   fast-tree)

  (check-equal?
   (slowest-tree 2002
                 (list fast-tree middle-tree slow-tree))
   slow-tree)

  (check-equal?
   (quickest-trees 2002
                   (list
                    (list middle-tree fast-tree)
                    (list slow-tree fast-tree)
                    (list slow-tree)))
   (list middle-tree fast-tree))

  (check-equal?
   (combine-schedules
    (hash 2002 (set FAKE_101))
    (hash 2002 (set (course-id "MATH" "141"))
          2004 (set FAKE_102)))
   (hash 2002 (set FAKE_101 (course-id "MATH" "141"))
         2004 (set FAKE_102)))

  (check-equal?
   (combine-schedules
    (hash 2002 (set FAKE_101))
    (hash))
   (hash 2002 (set FAKE_101)))

  (check-equal?
   (combine-schedules
    (hash)
    (hash 2002 (set FAKE_101)))
   (hash 2002 (set FAKE_101)))

  (check-equal?
   (combine-lists
    (list (list (set FAKE_101))
          (list (set FAKE_102))))
   (list (set FAKE_101 FAKE_102)))

  (check-equal?
   (level-sort-tree slow-tree)
   (list (set FAKE_101) (set FAKE_102) (set FAKE_151)))

  ;; TODO figure out what is wrong here
  #|(check-equal?
   (pack-classes 2002 (set middle-tree slow-tree))
   (hash 2002 (set FAKE_101)
         2004 (set FAKE_102)
         2012 (set FAKE_151)))|#

  (check-equal?
   (minimum-graduation-quarter (set FAKE_101 FAKE_102) FAKE_ENGINEERING 2008)
   2012)

  (check-equal?
   (minimum-graduation-quarter (set) FAKE_ENGINEERING 2008)
   2022)

  )

(provide
 minimum-graduation-quarter)
