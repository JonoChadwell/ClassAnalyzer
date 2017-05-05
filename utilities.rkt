#lang typed/racket

(: pair (All (A B) (-> A B (Pairof A B))))
(define pair cons)

(: left (All (A B) (-> (Pairof A B) A)))
(define left car)

(: right (All (A B) (-> (Pairof A B) B)))
(define right cdr)

(: min-list (-> (Listof Integer) Integer))
(define (min-list lst)
  (if (empty? lst)
      (error "Empty list has no minimum element")
      (foldl min (first lst) (rest lst))))

(: max-list (-> (Listof Integer) Integer))
(define (max-list lst)
  (if (empty? lst)
      (error "Empty list has no minimum element")
      (foldl max (first lst) (rest lst))))

(: sum-list (-> (Listof Integer) Integer))
(define (sum-list lst)
  (if (empty? lst)
      0
      (foldl + (first lst) (rest lst))))

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


(: hash-retain-keys (All (A B) (-> (-> A Boolean) (HashTable A B) (HashTable A B))))
(define (hash-retain-keys proc table)
  (make-immutable-hash
   (filter
    (lambda ([x : (Pairof A B)])
      (proc (left x)))
    (hash->list table))))

(: hash-retain-values (All (A B) (-> (-> B Boolean) (HashTable A B) (HashTable A B))))
(define (hash-retain-values proc table)
  (make-immutable-hash
   (filter
    (lambda ([x : (Pairof A B)])
      (proc (right x)))
    (hash->list table))))

(: list-contains (All (A) (-> A (Listof A) Boolean)))
(define (list-contains val lst)
  (if (member val lst)
      #t
      #f))

(module+ test
  (require typed/rackunit)

  (check-equal? (max-list '(1 2 3 6 5)) 6)

  (check-equal? (smallest-set (list (set 0 1 2) (set 0 1) (set 0 1 2 3)))
                (set 0 1))

  (check-equal?
   (hash-retain-keys
    (lambda ([x : Real]) (> x 1))
    (hash 1 "one"
          2 "two"
          3 "three"))
   (hash 2 "two"
         3 "three"))
  
  (check-equal?
   (hash-retain-values
    (lambda ([x : Real]) (> x 1))
    (hash "one" 1
          "two" 2
          "three" 3))
   (hash "two" 2
         "three" 3)))

(provide
 pair
 left
 right
 min-list
 max-list
 sum-list
 smallest-set
 hash-retain-keys
 hash-retain-values
 list-contains)
