#lang racket

(require
  "ce-program.rkt"
  "cpe-program.rkt"
  "csc-program.rkt"
  "se-program.rkt")

(define majors
  (hash
   "CE" bs-ce-15-17
   "CPE" bs-cpe-17-19
   "CSC" bs-csc-15-17
   "SE" bs-se-17-19))

(provide majors)
