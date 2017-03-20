#lang typed/racket

(: pair (All (A B) (-> A B (Pairof A B))))
(define pair cons)

(: left (All (A B) (-> (Pairof A B) A)))
(define left car)

(: right (All (A B) (-> (Pairof A B) B)))
(define right cdr)

(provide pair left right)