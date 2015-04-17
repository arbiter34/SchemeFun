(define (f lst)
(display lst)
; (a) ;
(if (null? lst)
; (b) ;
'()
; (c) ;
(cons (+ 1 (car lst)) (f (cdr lst)))))

(display (f '(3 1 4 1 5 9)))
(display "\n")

(define (member? e lst)
;Condition check (if else if else)
	(cond 
		;if reached end of list, return false
		((null? lst) #f)
		;if current head of list == e, return true
		((equal? (car lst) e) #t)
		;else recurse with rest of list
		(else (member? e (cdr lst)))
	)

)

(display (member? 4 '(4 5 2 9 0 1)))
(display "\n")

(display (member? 'one '(1 2 3 4)))
(display "\n")

(define (set? lst)
	;Condition check 
	(cond
		;End of list, no duplicates found, true - proper set
		((null? lst) #t)
		;Check if head of list is a member of the rest of the list, if so, return false - bad set
		((member? (car lst) (cdr lst)) #f)
		;Else recurse with rest of list
		(else (set? (cdr lst)))
	)
)

(display (set? '(a 1 b a 2 c 3)))
(display "\n")

(display (set? '(it was the best of times, it was the worst of times)))
(display "\n")

(define (union lst1 lst2)
	;Condition check
	(cond
		;If list 2 is null, end of recursion, return lst1
		((null? lst2) lst1)
		;If first member of lst2 is in lst 1, don't add to lst1, recurse
		((member? (car lst2) lst1) (union lst1 (cdr lst2)))
		;else first member of lst2 is not int lst1, add it to lst1 and recurse
		(else (union (append lst1 (list (car lst2))) (cdr lst2)))
	)

)

(display (union '(3 5 7 98 02 19 28) '(4 5 7 9 02 8918 093)))
(display "\n")

(define (intersect lst1 lst2)
	;Condition check
	(cond
		;If either list is null, end of recursion, return empty set
		((null? lst1) '())
		((null? lst2) '())
		;Else do stuff
		(else 
			;if the first member of lst1 is in lst2 append 
			(append (intersect (cdr lst1) lst2)
				(if (member? (car lst1) lst2)
					(list (car lst1))
					'())))
	)
)

(display (intersect '(3 5 7 98 02 19 28) '(4 5 7 9 02 8918 093)))
(display "\n")


(define (flatten lst1 lst2)
	;Check if end of part 1 recursion - one member left
	(cond 
		;lst1 is not a list, return single member as list
		((not (list? lst1)) (list lst1))
		;else, append lst1 and lst2 and store in lst1
		(else (set! lst1 (append lst1 lst2)))
	)
	
	;Check conditions for necessary recursion
	;If lst1 is null, return empty list
    (cond ((null? lst1) '())
    	;if lst one isn't pair(no embedding), return it as list
        ((not (pair? lst1)) (list lst1))
        ;else recurse on first member and recurse on rest of list, append results
        (else (append (flatten (car lst1) '())
                        (flatten (cdr lst1) '())
              )
    	)
    )
)


(display (flatten '(1 (2 3) 5) '(8 (13 (21 34) 55))))