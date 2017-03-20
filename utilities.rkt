#lang typed/racket

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

(provide pair left right min-list sum-list)