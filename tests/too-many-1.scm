; too-many-args!
(define (f x) 1)
(define (g x y . z) 2)
(define foo #t)
(apply (if foo f g) '(1 2 3 4))
