; too-few-args!
(define (f x) 1)
(define (g x y . z) 2)
(define foo #f)
(apply (if foo f g) '(1))
