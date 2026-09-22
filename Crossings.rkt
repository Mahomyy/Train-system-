#lang racket

(provide  list-of-crossings
         initial-crossing
         change-test-state
         crossing-open-test)

(define list-of-crossings (list 'C-1 'C-2))

(define initial-crossing 'C-1)

(define crossing-open-test #t)

(define (change-test-state bolean)  ;;this is for testing if the crossing is open or closed
  (cond ((and (eq? crossing-open-test #t) (eq? bolean #f))
  (set! crossing-open-test #f))
       ((and (eq? crossing-open-test #f) (eq? bolean #t))
  (set! crossing-open-test #t))))