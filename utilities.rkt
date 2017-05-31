#lang typed/racket

; "cons" but with less type system wonkyness around lists
(: pair (All (A B) (-> A B (Pairof A B))))
(define pair cons)

; an easier to read alias for car
(: left (All (A B) (-> (Pairof A B) A)))
(define left car)

; an easier to read alias for cdr
(: right (All (A B) (-> (Pairof A B) B)))
(define right cdr)

; returns the smallest value in a list
(: min-list (-> (Listof Integer) Integer))
(define (min-list lst)
  (if (empty? lst)
      (error "Empty list has no minimum element")
      (foldl min (first lst) (rest lst))))

; returns the largest value in a list
(: max-list (-> (Listof Integer) Integer))
(define (max-list lst)
  (if (empty? lst)
      (error "Empty list has no minimum element")
      (foldl max (first lst) (rest lst))))

; returns the sum of a list
(: sum-list (-> (Listof Integer) Integer))
(define (sum-list lst)
  (if (empty? lst)
      0
      (foldl + (first lst) (rest lst))))

; returns the smallest set out of a list of sets
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

; Filters the contents of a hash table based on its keys
(: hash-retain-keys (All (A B) (-> (-> A Boolean) (HashTable A B) (HashTable A B))))
(define (hash-retain-keys proc table)
  (make-immutable-hash
   (filter
    (lambda ([x : (Pairof A B)])
      (proc (left x)))
    (hash->list table))))

; Filters the contents of a hash table based on its values
(: hash-retain-values (All (A B) (-> (-> B Boolean) (HashTable A B) (HashTable A B))))
(define (hash-retain-values proc table)
  (make-immutable-hash
   (filter
    (lambda ([x : (Pairof A B)])
      (proc (right x)))
    (hash->list table))))

; Checks if a list contains a value
(: list-contains (All (A) (-> A (Listof A) Boolean)))
(define (list-contains val lst)
  (if (member val lst)
      #t
      #f))

(module+ test
  (require typed/rackunit)

  (check-equal? (pair 'a 'b) (cons 'a 'b))

  (check-equal? (left (pair 'a 'b)) 'a)

  (check-equal? (right (pair 'a 'b)) 'b)

  (check-equal? (min-list '(7 2 4 1 3 7 11)) 1)
  (check-exn exn:fail? (lambda () (min-list empty)))
  
  (check-equal? (max-list '(1 2 3 6 5)) 6)
  (check-exn exn:fail? (lambda () (max-list empty)))

  (check-equal? (sum-list '(3 1 2)) 6)
  (check-equal? (sum-list empty) 0)

  (check-equal? (smallest-set (list (set 0 1 2) (set 0 1) (set 0 1 2 3)))
                (set 0 1))
  (check-exn exn:fail? (lambda () (smallest-set empty)))

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
         "three" 3))

  (check-true (list-contains 'b '(a b c d)))
  (check-false (list-contains 'e '(a b c d))))

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
