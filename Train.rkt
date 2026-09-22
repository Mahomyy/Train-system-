#lang racket

(provide 
         initial-speed
         trains
         direction-train
        set-train-direction!
         get-selected-train-id
         get-selected-train
         find-train
         
        )
(define initial-speed  100)
(define trains (make-hash)) ; Initial hash of trains
(define train-directions (make-hash)) ; Initialize hash for storing train directions


(define (set-train-direction! train-id direction)
  (hash-set! train-directions train-id direction))


(define (direction-train train-id)
  (hash-ref train-directions train-id #f)) ; Default direction is false (forward)



(define (get-selected-train-id train-id-field)
  (send train-id-field get-value))



(define (get-selected-train train-id-field) 
  (let* ([selected-train-id (get-selected-train-id train-id-field)]
         [selected-train (hash-ref trains selected-train-id #f)])
    selected-train))


(define (find-train id train-list)
  (let loop ((remaining-trains train-list))
    (cond ((null? remaining-trains) #f)                 ; not found
          ((eq? (car (car remaining-trains)) id) (car remaining-trains))   ; found train
          (else (loop (cdr remaining-trains))))))       ; continue searching

