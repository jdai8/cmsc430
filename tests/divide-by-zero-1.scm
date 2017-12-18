; dbz
(guard (x ((eq? x 1) (halt 'dbz)))
       (define divide /)
       (divide 1 0))
