#lang racket

(provide
 shortest-path
 get-edge-label
 label->index
 next-label
 before-index
 before-label
 label->index-map
 label->index
 index->label
 next-index)

(require (prefix-in BFT: "a-d/graph-traversing/bft-labeled.rkt"))

(require (prefix-in g: "a-d/graph/labeled/adjacency-matrix.rkt"))



; BFT shortest path
(define (shortest-path from to)
  (define g simulator-graph)
  (define paths (make-vector (g:order g) '()))
  (vector-set! paths from (g:label g from)) 
  (BFT:bft g 
       BFT:root-nop
       (lambda (node label)
         (not (equal? label (g:label g to))))
       (lambda (from to edge-label)
         (define x (vector-ref paths to))
         (vector-set! paths to (append x (list (vector-ref paths from) (g:label g to)))))
       BFT:edge-nop
       (list from))
  (vector-ref paths to))


;;took inspiration from this

;(define (shortest-path g from to)
      ;(define paths (make-vector (order g) '()))
      ;(vector-set! paths from (list from))
      ;(bft g 
           ;root-nop
           ;(lambda (node) 
            ; (not (eq? node to)))
           ;(lambda (from to)
            ; (vector-set! paths to (cons to (vector-ref paths from))))
          ;; edge-nop
       ;    (list from))
     ; (vector-ref paths to))


(define label->index-map
  (make-hash))

(define (initialize-label->index-map g)
  (define label-index-list
    '(("1-6" . 0)
      ("S-5" . 1)
      ("S-6" . 2)
      ("2-3" . 3)
      ("S-12" . 4)
      ("S-11" . 5)
      ("S-10" . 6)
      ("1-1" . 7)
      ("S-7" . 8)
      ("S-25" . 9)
      ("1-8" . 10)
      ("2-1" . 11)
      ("S-1" . 12)
      ("S-2" . 13)
      ("S-3" . 14)
      ("2-2" . 15)
      ("S-8" . 16)
      ("2-5" . 17)
      ("S-4" . 18)
      ("2-6" . 19)
      ("2-7" . 20)
      ("S-16" . 21)
      ("2-8" . 22)
      ("1-3" . 23)
      ("S-27" . 24)
      ("S-26" . 25)
      ("1-4" . 26)
      ("1-5" . 27)
      ("S-20" . 28)
      ("2-4" . 29)
      ("S-23" . 30)
      ("S-24" . 31)
      ("S-9" . 32)
      ("1-2" . 33)
      ("S-28" . 34)
      ("1-7" . 35)))
  (for-each
   (lambda (pair)
     (hash-set! label->index-map (car pair) (cdr pair)))
   label-index-list))




(define (label->index label)
  (define sLabel (if (symbol? label) (symbol->string label) label))
  (let ((index (hash-ref label->index-map sLabel)))
    (if index
        index
        (error "Label not found in label->index" label))))

(define (display-label-index-map)
  (let ((keys (hash-keys label->index-map)))
    (for-each
     (lambda (key)
       (display "Label: ")
       (display key)
       (newline))
     keys)))


(define (index->label value)
  (let ((result #f))
    (hash-for-each label->index-map
                   (lambda (key val)
                     (when (equal? val value)
                       (set! result key)
                       ))) ; Stop iterating after finding the key
    result))


(define (next-index label direction)
  (let ((currentIndex (label->index label))) ; Get the current index of the label
    (if (and currentIndex (number? currentIndex)) ; Check if current index is a valid number
        (let* ((maxIndex 35)  ; Set the maximum index value
               (nextIndex (if direction
                               (if (= currentIndex 0) maxIndex (- currentIndex 1)) ; Decrement index if direction is true
                               (if (= currentIndex maxIndex) 0 (+ currentIndex 1))))) ; Increment index if direction is false
          (let loop ((index nextIndex))
            (if (member index '(1 2 4 5 6 8 9 12 13 14 16 18 21 24 25 28 30 31 32 34)) ; Check if the next index is in the skip list (list with switches)
                (loop (if direction
                          (if (= index 0) maxIndex (- index 1)) ; Decrement index if direction is true
                          (if (= index maxIndex) 0 (+ index 1)))) ; Increment index if direction is false
                index))) ; Return nextIndex if currentIndex is valid and not in the skip list
        (begin
          (displayln "Invalid or missing current index")
          #f)))) ; Return #f if currentIndex is invalid or missing




(define (next-label label direction)
  (index->label (next-index label direction))) 




(define (before-index label direction)
  (let ((currentIndex (label->index label))) ; Get the current index of the label
    (if (and currentIndex (number? currentIndex)) ; Check if current index is a valid number
        (let* ((maxIndex 35)  ; Set the maximum index value
               (beforeIndex (if direction
                                (if (= currentIndex maxIndex) 0 (+ currentIndex 1)) ; Increment index if direction is true
                                (if (= currentIndex 0) maxIndex (- currentIndex 1))))) ; Decrement index if direction is false
          (let loop ((index beforeIndex))
            (if (member index '(1 2 4 5 6 8 9 12 13 14 16 18 21 24 25 28 30 31 32 34)) ; Check if the before index is in the skip list (list with switches)
                (loop (if direction
                          (if (= index maxIndex) 0 (+ index 1)) ; Increment index if direction is true
                          (if (= index 0) maxIndex (- index 1)))) ; Decrement index if direction is false
                index))) ; Return beforeIndex if currentIndex is valid and not in the skip list
        (begin
          (displayln "Invalid or missing current index")
          #f)))) ; Return #f if currentIndex is invalid or missing

(define (before-label label direction)
  (index->label (before-index label direction)))



; Function to get the label on an edge based on switch-id and to-id
(define (get-edge-label switch-id to-id)
  (cond ((and (label-exists? switch-id) (label-exists? to-id))
         (let* ((switch-index (label->index switch-id))
                (to-index (label->index to-id)))
           (g:edge-label simulator-graph switch-index to-index)))
        (else 'edge-label-not-found)))

; Function to check if a label exists in the label->index-map
(define (label-exists? label)
  (hash-ref label->index-map label))



(define (status-connections switch-index)
  (define g simulator-graph)
  
  ; Helper function to extract label state from an edge
  (define (get-label-state label direction)
    (let ((char-index (if (> switch-index direction) 0 2)))
      (string->number (string (string-ref (symbol->string label) char-index)))))
  
  ; Function to execute for each edge
  (define (process-edge to label)
    (define label-state (get-label-state label switch-index))
    
    ; Check if the label state is valid
    (cond ((and (>= label-state 0) (<= label-state 2))
           (list label-state (g:label g to)))
          (else
           (error "Invalid label state in status-connections" label-state))))
  
  ; Execute the function for each edge of the given switch
  (g:for-all-edges g switch-index process-edge))


(define simulator-graph
  (let ((g (g:new #f 36)))
    ; labels
    (g:label! g 0 '1-6)
    (g:label! g 1 'S-5)
    (g:label! g 2 'S-6)
    (g:label! g 3 '2-3)
    (g:label! g 4 'S-12)
    (g:label! g 5 'S-11)
    (g:label! g 6 'S-10)
    (g:label! g 7 '1-1)
    (g:label! g 8 'S-7)
    (g:label! g 9 'S-25)
    (g:label! g 10 '1-8)
    (g:label! g 11 '2-1)
    (g:label! g 12 'S-1)
    (g:label! g 13 'S-2)
    (g:label! g 14 'S-3)
    (g:label! g 15 '2-2)
    (g:label! g 16 'S-8)
    (g:label! g 17 '2-5)
    (g:label! g 18 'S-4)
    (g:label! g 19 '2-6)
    (g:label! g 20 '2-7)
    (g:label! g 21 'S-16)
    (g:label! g 22 '2-8)
    (g:label! g 23  '1-3)
    (g:label! g 24  'S-27)
    (g:label! g 25  'S-26)
    (g:label! g 26  '1-4)
    (g:label! g 27  '1-5)
    (g:label! g 28  'S-20)
    (g:label! g 29  '2-4)
    (g:label! g 30  'S-23)
    (g:label! g 31  'S-24)
    (g:label! g 32  'S-9)
    (g:label! g 33 '1-2)
    (g:label! g 34 'S-28)
    (g:label! g 35 '1-7)

    ; Initialize the label->index-map
(initialize-label->index-map g)

    (g:add-edge! g (label->index '1-7)  (label->index '1-6)  '0-0)
    (g:add-edge! g (label->index '2-3)  (label->index 'S-12) '0-2)
    (g:add-edge! g (label->index 'S-12) (label->index 'S-11) '0-0)
    (g:add-edge! g (label->index 'S-11) (label->index 'S-10) '2-0)
    (g:add-edge! g (label->index 'S-10) (label->index '1-1)  '1-0)
    (g:add-edge! g (label->index 'S-7)  (label->index 'S-25) '1-1)
    (g:add-edge! g (label->index 'S-25) (label->index 'S-1)  '2-2)
    (g:add-edge! g (label->index 'S-25) (label->index '1-8)  '0-0)
    (g:add-edge! g (label->index '2-1)  (label->index 'S-1)  '0-1)
    (g:add-edge! g (label->index 'S-8)  (label->index 'S-4)  '2-0)
    (g:add-edge! g (label->index 'S-4)  (label->index '2-6)  '1-0) 
    (g:add-edge! g (label->index 'S-4)  (label->index '2-7)  '2-0)
    (g:add-edge! g (label->index 'S-10) (label->index 'S-16) '2-1)
    (g:add-edge! g (label->index 'S-16) (label->index '2-8)  '2-0)
    (g:add-edge! g (label->index 'S-8)  (label->index '2-5)  '1-0)
    (g:add-edge! g (label->index '1-3)  (label->index 'S-27) '0-1)
    (g:add-edge! g (label->index '1-3)  (label->index 'S-24) '0-2)
    (g:add-edge! g (label->index 'S-27) (label->index 'S-26) '0-2)
    (g:add-edge! g (label->index 'S-27) (label->index '1-2)  '2-0)
    (g:add-edge! g (label->index 'S-26) (label->index 'S-28) '1-2)
    (g:add-edge! g (label->index 'S-26) (label->index '1-4)  '0-0)
    (g:add-edge! g (label->index '1-4)  (label->index '1-5)  '0-0)
    (g:add-edge! g (label->index '1-5)  (label->index 'S-20) '0-1)
    (g:add-edge! g (label->index 'S-20) (label->index '2-4)  '0-0)
    (g:add-edge! g (label->index '2-4)  (label->index 'S-23) '0-0)
    (g:add-edge! g (label->index 'S-23) (label->index 'S-24) '1-0)
    (g:add-edge! g (label->index 'S-23) (label->index 'S-12) '2-1)
    (g:add-edge! g (label->index 'S-24) (label->index 'S-9)  '1-1)
    (g:add-edge! g (label->index 'S-9)  (label->index '1-2)  '0-0)
    (g:add-edge! g (label->index 'S-9)  (label->index 'S-11) '2-1)
    (g:add-edge! g (label->index 'S-28) (label->index '1-1)  '0-0)
    (g:add-edge! g (label->index 'S-28) (label->index '1-7)  '1-0)
    (g:add-edge! g (label->index '1-6)  (label->index 'S-5)  '0-1)
    (g:add-edge! g (label->index 'S-5)  (label->index 'S-6)  '0-0)
    (g:add-edge! g (label->index 'S-5)  (label->index 'S-7)  '2-0)
    (g:add-edge! g (label->index 'S-6)  (label->index '2-3)  '1-0)
    (g:add-edge! g (label->index 'S-20) (label->index 'S-6)  '2-2)
    (g:add-edge! g (label->index 'S-7)  (label->index 'S-2)  '2-1)
    (g:add-edge! g (label->index 'S-1)  (label->index 'S-3)  '0-0)
    (g:add-edge! g (label->index 'S-2)  (label->index 'S-8)  '2-0)
    (g:add-edge! g (label->index 'S-2)  (label->index '2-2)  '2-0)  
    ; return graph
    g))



