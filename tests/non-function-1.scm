; yum-yum
(guard (x ((= x 5) 'yum-yum))
       (define y 10)
       (apply y (list 100)))
