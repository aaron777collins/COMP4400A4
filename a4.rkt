; Right reduce
; (fn a (fn b (fn c i)))
(define (right-reduce fn list init)
  (if (null? list) init
      (fn (car list)
          (right-reduce fn (cdr list) init))))

; Left reduce
; ((((i • a) • b) • c)
(define (left-reduce fn init list)
  (if (null? list) init
      (left-reduce fn (fn init (car list)) (cdr list))))

; takes the input vector, say (a b c) and returns a flipped
; vector list ( (b . a) (c . a) )
(define vecFlip (lambda (lst)
                  (map
                   (lambda(x) (append x (list(car lst))))
                  (map list (cdr lst))
                  )
                  ))

; gets the list, flips the vectors and merges it into 1 list
; these are only vector to vector relations as an output
(define flipAndMergeVec (lambda (lst)
                          (
                           left-reduce
                           append
                           '()
                           (map vecFlip lst))
                          ))

; gets the first item for each in the list
(define firstOfList (lambda (lst) (map car lst)))

; checks if an element is a member in the lst
(define member? (lambda (x lst)
                 (if (null? lst)
                     #f
                     (if (eq? x (car lst))
                         #t
                         (member x (cdr lst))
                     )
                 )
            ))

(define second (lambda (lst) (car (cdr lst))))

; input example: {{b . a} {c . b} {b . c}}
(define groupVector (lambda (lst)

                      (map
                       (lambda (x) 
                                   (if (eq? (car x) (car (car lst)))
                                       ;(cons (cdr (car lst)) x)
                                       ;x
                                       (append x (cdr (car lst)))
                                       x
                                      )
                                   )
                       (cdr lst)
                       )

                      ))



;(flipAndMergeVec '((a b c) (b c d) (c b a) (d)))
;(vecFlip '(a b c))


;(firstOfList '((a b) (b c) (c b)))
;(flipAndMergeVec '((a b) (b c) (c b)))
;(firstOfList(flipAndMergeVec '((a b) (b c) (c b))))
;(member? 'a '(a b c))
;(length '(a b c))
;(groupVector (flipAndMergeVec '((a b) (b c) (c b))))
(flipAndMergeVec '((a b c) (b c d) (c b a) (d)))
;(groupVector (flipAndMergeVec '((a b c) (b c d) (c b a) (d))))
;(second '(a b))