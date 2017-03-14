#lang racket

(require racket/set rackunit "start.rkt")

(define CPE101 (course "CPE" "101"))
(define CPE102 (course "CPE" "102"))
(define CPE103 (course "CPE" "103"))
(define CPE225 (course "CPE" "225"))
(define CPE357 (course "CPE" "357"))

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
          (exactly (course "ENGL" "133"))
          (exactly (course "ENGL" "134")))))
(check-equal?
 (group-all "ENGL" "133" "ENGL" "134")
 (all-of (list
          (exactly (course "ENGL" "133"))
          (exactly (course "ENGL" "134")))))
