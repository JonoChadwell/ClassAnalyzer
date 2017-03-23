#lang racket

(require racket/set "start.rkt" "utilities.rkt")

(define unit-counts
  (hash
   (course "ARCE" "305") 2
   (course "ARCE" "372") 3
   (course "ARCE" "403") 3
   (course "BRAE" "345") 3
   (course "CHEM" "341") 3
   (course "CM" "334") 2
   (course "CM" "432") 3
   (course "CRP" "404") 3
   (course "CRP" "408") 3
   (course "IME" "314") 3
   (course "SS" "423") 5
   (course "ARCE" "372") 3
   (course "ARCE" "372") 3
   (course "ARCE" "372") 3
   (course "ARCE" "372") 3
   (course "ARCE" "372") 3
   (course "CE" "400") 2 ;; assuming max
   (course "CE" "406") 5
   (course "CE" "413") 2
   (course "CE" "470") 4 ;; assuming max
   (course "CE" "474") 2
   (course "CE" "493") 2
   (course "CE" "494") 6
   (course "CE" "495") 12
   (course "CE" "500") 3 ;; assuming max
   (course "CE" "555") 2
   (course "CE" "570") 4 ;; assuming max
   (course "CE" "571") 4 ;; assuming max
   (course "CE" "591") 1
   (course "CE" "592") 1
   (course "CE" "593") 2
   (course "CE" "594") 6
   (course "CE" "595") 12
   (course "CE" "596") 1
   (course "CE" "599") 12 ;; assuming max
   ))

(provide unit-counts)
