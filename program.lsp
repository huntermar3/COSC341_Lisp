;Hunter Martin
;COSC 341 Winter 25
;3-5-25

;function that checks if a list has an atom, L represents a list
(defun f1 (L)
    (cond ((null L) nil) ;if we traverse through the entire list and haven't found an atom then ret. nil
    ((atom (car L)) T) ;we check the first element of the list and if its an atom ret. true
    (T (f1 (cdr L))) ;if we didn't find an atom, check the remaining of the list
    )
)

;function that checks that if in a list the list has only one element 
(defun f2 (L)
    (cond ((null L) 0) ;we reached the end of the list and ret. 0
    ((and (listp (car L)) (equal (length (car L)) 1)) (+ 1 (f2 (cdr L)))) ;if the beginning of the list is a list 
                                                                          ;AND the length of that list is 1, add 1 and 
                                                                          ;recursive traverse the rest of the list

    (T (f2 (cdr L))) ;regardless we need to traverse the rest of the list 
    )
)

;Write a function f3 that takes a list of integers and returns a list containing only odd
;integers
(defun f3 (L)
    (cond ((null L) nil) ;return a empty list once we reached the end 
    ((oddp (car L))  (cons (car L) (f3 (cdr L))) ) ;if the number in the list is odd, add the odd number to the list and 
                                                   ;recursively traverse the remaining numbers of the list

    (T (f3 (cdr L)) ) ;if all the conditions are false, go through the rest of the list recursively
    )
)

;Write a function f4 that returns the minimum value of an integer list.
(defun f4 (L)
    (cond ((null L) nil)
    (                   )
    (T (f4 (cdr L)))



    )

)


;function that returns the squares of all the numbers in a list
(defun f8 (L)
    (cond ((null L) 0)
    ((setq square (* (car L) (car L)))  )
    (T (+ square (f8 (cdr L))) )
    
    )

)








