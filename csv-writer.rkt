#lang typed/racket

(require "utilities.rkt")

(: display-csv-value (-> String Output-Port Void))
(define (display-csv-value str file)
  (if (list-contains #\, (string->list str))
      (display (string-append "\"" (string-replace str "\"" "\"\"") "\"") file)
      (display str file)))

(: write-csv (-> String (Listof (Listof String)) Void))
(define (write-csv file-name data)
  (let ([file (open-output-file (string-append "generated/" file-name ".csv") #:mode 'text #:exists 'truncate)])
    (for-each (lambda ([row : (Listof String)])
                (cond
                  [(not (empty? row))
                   (display (first row) file)
                   (for-each (lambda ([value : String])
                               (display "," file)
                               (display-csv-value value file))
                             (rest row))])
                (displayln "" file))
              data)
    (close-output-port file))
  (displayln (string-append "Wrote file " file-name ".csv")))


(provide
 write-csv)
