#lang typed/racket

(require typed/rackunit)

(: pair (All (A B) (-> A B (Pairof A B))))
(define pair cons)

(: left (All (A B) (-> (Pairof A B) A)))
(define left car)

(: right (All (A B) (-> (Pairof A B) B)))
(define right cdr)

(: min-list (-> (Listof Integer) Integer))
(define (min-list lst)
  (foldl min (first lst) (rest lst)))

(: sum-list (-> (Listof Integer) Integer))
(define (sum-list lst)
  (foldl + (first lst) (rest lst)))

(: smallest-set (All (A) (-> (Listof (Setof A)) (Setof A))))
(define (smallest-set lst)
  (if (empty? lst)
      (error "no smallest element of empty list")
      (if (empty? (rest lst))
          (first lst)
          (let ([next (smallest-set (rest lst))])
            (if (<= (set-count (first lst)) (set-count next))
                (first lst)
                next)))))

(check-equal? (smallest-set (list (set 0 1 2) (set 0 1) (set 0 1 2 3)))
              (set 0 1))

(provide pair left right min-list sum-list smallest-set)
