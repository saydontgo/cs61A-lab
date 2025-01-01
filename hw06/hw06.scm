(define (cddr s) (cdr (cdr s)))

(define (cadr s) 
    (car (cdr s))
)

(define (caddr s) 
    (car (cddr s))
)

(define (ascending? lst) 
    (define (asc? lst val)
        (cond 
            ((null? lst) #t)
            ((>= (car lst) val) (asc? (cdr lst) (car lst)))
            (else #f)
        )
    )
    (asc? (cdr lst) (car lst))
)

(define (interleave lst1 lst2) 
    (cond 
        ((null? lst1) 
            (if (null? lst2)
                nil
                (cons (car lst2) (interleave nil (cdr lst2)))
            )
        )
        ((null? lst2) 
            (if (null? lst1)
                nil
                (cons (car lst1) (interleave nil (cdr lst1)))
            )
        )
        (else (cons (car lst1) (cons (car lst2) (interleave (cdr lst1) (cdr lst2)))))
    )
)

(define (my-filter func lst) 
    (cond 
    ((null? lst) nil)
    ((func (car lst)) (cons (car lst) (my-filter func (cdr lst))))
    (else (my-filter func (cdr lst)))
    )
)

(define (no-repeats lst) 
    
    (define (nr lst res)
        (cond 
            ((null? lst) nil)
            ((null?(my-filter ((lambda (x) (lambda (y) (= x y))) (car lst)) res)) 
                (cons (car lst) (nr (cdr lst) (cons (car lst) res)))
            )
            (else (nr (cdr lst) (cons (car lst) res)))
        )
    )
    (nr lst nil)
)
