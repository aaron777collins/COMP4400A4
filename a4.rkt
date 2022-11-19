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
; Takes the first element in the list and tries to
; group it with the rest of the vectors
(define groupVector (lambda (lst)
                      ; Checks if the list's first item can be merged with any other vectors
                      (if (eq? (hasDuplicateOfFirst (firstOfList lst)) #t)
                      (map
                       (lambda (x) ; Loops through list and tries to merge the vectors
                                   (if (eq? (car x) (car (car lst)))
                                       ;(cons (cdr (car lst)) x)
                                       ;x
                                       (append x (cdr (car lst)))
                                       x
                                      )
                                   )
                       (cdr lst)
                       )
                      lst
                      )

                      ))

; checks for duplicates in a list
(define hasDuplicates (lambda (lst)
                        (if (null? lst) #f
                          (if (member (car lst) (cdr lst))
                            #t
                            (hasDuplicates (cdr lst))
                          )
                        )
                        ))

; checks if the rest of the list has more of the first letter
(define hasDuplicateOfFirst (lambda (lst)

                         (if (null? lst) #f
                          (if (member (car lst) (cdr lst))
                            #t
                            #f
                          )
                        )

                              ))

; continually rotates and merges the list
(define rotateAndMerge (lambda (lst)
                         (if (eq? (hasDuplicates (firstOfList lst)) #t)
                             (rotateAndMerge (groupVector (append (cdr lst) (list (car lst)))))
                             lst
                             )
                           ))

; Deletes all occurances of a term from a list
(define delete
  (lambda (item lst)
    (cond
     ((null? lst) lst)
     ((eq? item (car lst)) (delete item (cdr lst)))
     (else (cons (car lst) (delete item (cdr lst)))))))

; Finds the letters that are missing from the graph given the list of possibly missing letters
(define getMissingLetters (lambda (lst missingLst)
                            (delete 'deletable
                            (map
                             (lambda (x)
                               (if (member? x (firstOfList lst))
                                   'deletable
                                   x
                                   )
                               )
                             missingLst
                             )
                            )

                            ))

; Adds the list of missing letters back into the graph as vectors (a becomes (a), etc)
(define addMissingLetters (lambda (lst missingLst)

                            (left-reduce
                             append
                             lst
                             (map (lambda (x) (list (list x))) missingLst)
                             )

                            ))

; Reverses the web graph
(define webGraphReversal (lambda (lst)
                           ; Adds the letters which were removed back into the list
                              (addMissingLetters
                               (rotateAndMerge (flipAndMergeVec LST)) ;reverses graph then rotates list to merge
                               (getMissingLetters (rotateAndMerge (flipAndMergeVec lst)) (firstOfList lst)) ; finds the removed letters 
                               )
                              
                              )
                           )

;(flipAndMergeVec '((a b c) (b c d) (c b a) (d)))
;(vecFlip '(a b c))


;(firstOfList '((a b) (b c) (c b)))
;(flipAndMergeVec '((a b) (b c) (c b)))
;(firstOfList(flipAndMergeVec '((a b) (b c) (c b))))
;(member? 'a '(a b c))
;(length '(a b c))
;(groupVector (flipAndMergeVec '((a b) (b c) (c b))))
;(flipAndMergeVec '((a b c) (b c d) (c b a) (d)))
;(groupVector (flipAndMergeVec '((a b c) (b c d) (c b a) (d))))
;(second '(a b))
;(hasDuplicates '(a b c))

;(rotateAndMerge (flipAndMergeVec '((a b c) (b c d) (c b a) (d))))
;(rotateAndMerge (flipAndMergeVec '((a b c) (b c d) (c b a) (d))))
;(addMissingLetters
;(rotateAndMerge (flipAndMergeVec '((a b) (b c) (c b))))
;(getMissingLetters (rotateAndMerge (flipAndMergeVec '((a b) (b c) (c b)))) '(a b c d))
;)
;(webGraphReversal  '((a b) (b c) (c b)))
(webGraphReversal '((a b c) (b c d) (c b a) (d)))