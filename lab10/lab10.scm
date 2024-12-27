(define (over-or-under num1 num2)'
; cond写法
; (
;     cond 
;     ((< num1 num2) '-1)
;     ((= num1 num2) '0)
;     (else '1)
; )   

(
    if (= num1 num2) 0 (if (> num1 num2) 1 -1)
)
)

(define (make-adder num) '

(lambda (inc) (+ inc num))

)

(define (composed f g) '

(lambda (x) (f (g x)))
)

(define (square n) (* n n))

(define (pow base exp) '

(cond   ((= exp 0) 1)
        ((= exp 1) base)
        ((= exp 2) (square base))
        (else 
            (
                if (even? exp)
                (square (pow base (/ exp 2)))
                (* base (square (pow base (/ (- exp 1) 2))))
            )
        )
)
)
