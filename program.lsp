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

;Write a function f3 that takes a list of integers and returns a list containing only odd integers
(defun f3 (L)
    (cond ((null L) nil) ;return a empty list once we reached the end 
    ((oddp (car L))  (cons (car L) (f3 (cdr L))) ) ;if the number in the list is odd, add the odd number to the list and 
                                                   ;recursively traverse the remaining numbers of the list

    (T (f3 (cdr L)) ) ;if all the conditions are false, go through the rest of the list recursively
    )
)

;Write a function f4 that returns the minimum value of an integer list.
(defun f4 (L)
    (cond ((null L) nil) ;if the list is empty return empty list
    ((null (cdr L)) (car L)) ;if its a single elements aka cdr is a empty list then ret. the only element 
    ( (< (car L) (f4(cdr L))) (car L) ) ;if the element we are looking at in the front (car) is smaller than the rest of the list ret. car
    (T (f4 (cdr L))) ;default case recursive through the rest of the list
    )
)

;Write a function f5 that reverses a list
(defun f5 (L)
    (cond ((null L) nil) ;if we hit an empty list ret. an empty list
    (T (append (f5 (cdr L)) (list (car L)))) ;else reverse cdr of L and append first element of L at the end
    )
)

;Write a function f6 that returns a list containing every other element
(defun f6 (L)
    (cond ((null L) nil) ;if we hit an empty list then ret. an empty list
    (T (cons (car L) (f6(cdr(cdr L)))) ) ;add every other element in the front of our recursive call. 2 cdr calls grabs every other element
    )
) 

;Write a function f7 that returns element at a given location of a list
(defun f7 (L num)
    (cond ((null L) nil   ) ;if we reach this case then the element must of NOT been in the list (out of range)
    ((equal num 1) (car L)) ;we found our element just ret it

    (T (f7 (cdr L) (- num 1))) ;default case if we didn't meet any of our other conditional statemts 
                               ;then recursively go through the rest of the list, and minus 1 to our num  

    )
)

;Write a function f8 that returns the sum of squares of all integers everywhere in a list.
(defun f8 (L)
    (cond ((null L) 0) ;if we reach an empty list then ret. 0
    ((listp (car L)) (+ (f8(car L)) (f8(cdr L))) )  ;if we hit a list, call f8 on the car and the cdr 
    (T (+ (* (car L) (car L) ) (f8 (cdr L)))) ;multiply the car by itself and add onto the recursive call 
    )   
)

;Write a function f9 that removes duplicates from a list. 
(defun f9 (L)
    (cond ((null L) nil) ;if we hit an empty list ret. an empty list
    ( (my_member (car L) (cdr L)) (f9 (cdr L))  ) ;if car is in the list, then ignore and continue traversing the list
    (T (cons (car L) (f9 (cdr L)))    ) ;the number must of not have been a duplicate
    )
)

(defun my_member (x L)
    (cond ((null L) nil) ;if L is empty then x is not in L
    ((equal x (car L)) T) ;if x is first element of L then x is in L
    (T (my_member x (cdr L))) ;else check x is in cdr of L
    )
)

;Write a function f10 that finds the intersection of two lists.
(defun f10 (list list2)
    (cond ( (null list) nil) ;since the 2 lists are the same lengths it doesn't matter when which one hits empty

    ( (my_member (car list) list2) (cons (car list) (f10(cdr list) list2))) ;if the car is in the second list somewhere 
                                                                            ;then add that element to the front and recursive check remaining elements
    
    (T (f10 (cdr list) list2) ) ;must of not had an intersection, continue traversing recursively
    )
)






