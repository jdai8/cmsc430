#lang racket

; by Jack Dai

(provide desugar)
(require "utils.rkt")

(define (i->temp i)
  (string->symbol (string-append "t" (number->string i))))

; (define (improper-list? l)
;   (if (cons? l)
;     (if (null? (cdr l))
;       #f
;       (improper-list? (cdr l)))
;     #t))

(define (wrap-with-lib e)
  `(let* ([%wind-stack '()]
          [common-tail (lambda (x y)
                         (let ((lx (length x))
                               (ly (length y)))
                           (let loop ([x (if (> lx ly) (drop x (- lx ly)) x)]
                                      [y (if (> ly lx) (drop y (- ly lx)) y)])
                             (if (eq? x y)
                               x
                               (loop (cdr x) (cdr y))))))]
          [%do-wind (lambda (new)
                      (unless (eq? new %wind-stack) 
                        (let ([tail (common-tail new %wind-stack)])
                          (begin
                            (let f ((l %wind-stack))
                              (unless (eq? l tail)
                                (begin
                                  (set! %wind-stack (cdr l))
                                  ((cdr (car l)))
                                  (f (cdr l)))))
                            (let f ([l new])
                              (unless (eq? l tail)
                                (begin
                                  (f (cdr l))
                                  ((car (car l)))
                                  (set! %wind-stack l))))))))]
          [%dynamic-wind (lambda  (pre body post)
                           (begin
                             (pre)
                             (set! %wind-stack (cons (cons pre post) %wind-stack))
                             (let ([v (body)])
                               (begin
                                 (set! %wind-stack (cdr %wind-stack))
                                 (post)
                                 v))))]
          [%handler-stack '()])
     ,e))

(define (desugar-aux e)
  (match e

         [`(letrec* ([,xs ,e0s] ...) ,e1)
           (desugar-aux
             `(let ,(map (lambda (x) (list x ''undefined)) xs)
                (begin
                  ,@(map (lambda (x e) `(set! ,x ,e)) xs e0s)
                  ,e1)))]

         [`(letrec ([,xs ,e0s] ...) ,e1)
           (desugar-aux
             `(let ,(map (lambda (x) (list x ''undefined)) xs)
                (let ,(map (lambda (i e) (list (i->temp i) e))
                             (range (length e0s))
                             e0s)
                  (begin ,@(map (lambda (i x) `(set! ,x ,(i->temp i))) 
                                (range (length e0s))
                                xs)
                         ,e1))))]

         [`(let* ([,xs ,e0s] ...) ,e1)
           (desugar-aux
             (if (null? xs)
               e1
               `(let ([,(car xs) ,(car e0s)])
                  ,(desugar-aux `(let* ,(map list (cdr xs) (cdr e0s)) ,e1)))))]

         [`(let ,bindings ,e)
           `(let ,(map (lambda (b) (list (car b) (desugar-aux (second b)))) bindings)
              ,(desugar-aux e))]

         [`(let ,x ([,xs ,ivs] ...) ,e)
           (desugar-aux `(letrec ([,x (lambda ,xs ,e)])
                           (,x ,@ivs)))]

         [`(lambda (,x ...) ,e)
           `(lambda ,x ,(desugar-aux e))]

         ; last is non-null
         [`(lambda (,xs ... . ,last) ,e)
           (define params (gensym 'params))
            `(lambda ,params
               ,(desugar-aux
                  `(let* ,(foldr (lambda (x binds) 
                                   (append `([,x (car ,params)] [,params (cdr ,params)])
                                           binds))
                                 `([,last ,params])
                                 xs)
                     ,e)))]

         [`(dynamic-wind ,e0 ,e1 ,e2)
           `(%dynamic-wind ,(desugar-aux e0) ,(desugar-aux e1) ,(desugar-aux e2))]

         [`(guard (,x ,cond-clauses ...) ,e)
           (desugar-aux
             `(let ([cc (call/cc (lambda (k) k))])
                (if (cons? cc)
                  (let ([,x (car cc)])
                    (cond ,@cond-clauses
                          ['#t (raise ,x)]))
                  (dynamic-wind
                    (lambda () (set! %handler-stack (cons cc %handler-stack)))
                    (lambda () ,e)
                    (lambda () (set! %handler-stack (cdr %handler-stack)))))))]

         [`(raise ,e)
           (desugar-aux 
             `((car %handler-stack) (cons ,e '())))]

         [`(delay ,e)
           (desugar-aux
             `(let ([_has-value '#f]
                    [_saved-value '()]
                    [_thunk (lambda () ,e)])
                (lambda (action)
                  (case action 
                    [(promise?) 'yes-promise]
                    [(force) (if _has-value _saved-value 
                               (begin 
                                 (set! _saved-value (_thunk))
                                 (set! _has-value '#t)
                                 _saved-value))]))))]

         [`(force ,p) 
           (desugar-aux `(,p 'force))]

         ['promise?
          (desugar-aux '(lambda (p) (and (procedure? p)
                                         (eq? 'yes-promise (p 'promise?)))))]

         [`(promise? ,p)
           `(,(desugar-aux 'promise?) ,(desugar-aux p))]

         [`(and ,e ...)
           (if (null? e)
             ''#t
             (desugar-aux 
               (match-let ([(cons h t) e])
                 (if (null? t)
                   h
                   `(if ,h (and ,@t) '#f)))))]

         [`(or ,e ...)
           (if (null? e)
             ''#f
             (desugar-aux 
               (match-let ([(cons h t) e])
                 (if (null? t)
                   h
                   (let ([t (gensym)])
                    `(let ([,t ,h])
                        (if ,t ,t (or ,@(cdr e)))))))))]

         [`(cond ,cond-clauses ...)
           (desugar-aux 
             (foldr (lambda (cond-clause a) 
                      (match cond-clause
                             [`(else ,e) e]
                             [`(,t ,e) `(if ,t ,e ,a)]
                             [`(,e) 
                               (let ([t (gensym)])
                                 `(let ([,t ,e])
                                    (if ,t ,t ,a)))]))
                    '(void) cond-clauses))]

         [`(case ,e0 ,case-clauses ...)
           (let ([t (gensym)])
             (desugar-aux
               `(let ([,t ,e0])
                  ,(foldr
                     (lambda (case-clause a)
                       (match case-clause
                              [`(else ,e) e]
                              [`(,dats ,e1)
                                `(if (memv ,t (quote ,dats)) ,e1 ,a)]))
                     '(void)
                     case-clauses))))]

         [`(if ,e0 ,e1 ,e2)
           `(if ,(desugar-aux e0) ,(desugar-aux e1) ,(desugar-aux e2))]

         [`(when ,e0 ,e1)
           (desugar-aux `(if ,e0 ,e1 (void)))]

         [`(unless ,e0 ,e1) 
           (desugar-aux `(if ,e0 (void) ,e1))]

         [`(set! ,x ,e)
           `(set! ,x ,(desugar-aux e))]

         [`(begin ,es ...)
           (match-define (cons h t) es)
           (desugar-aux
             (if (null? t)
               h
               `(let ([,(gensym) ,h])
                  ,(desugar-aux `(begin ,@t)))))]

         [`(call/cc ,e)
           `(%my-call-cc
              ,(desugar-aux
                 `(lambda (k)
                    (,e (let ([k-stack %wind-stack])
                          (lambda (x)
                            (begin (%do-wind k-stack)
                                   (k x))))))))]

         [`(let/cc ,x ,e)
           (desugar-aux `(call/cc (lambda (,x) ,e)))]

         [`(apply ,e0 ,e1)
           `(apply ,(desugar-aux e0) ,(desugar-aux e1))]

         ; guard so we don't desugar already desugared things
         [`(apply-prim ,x ...)
           `(apply-prim ,@x)]
         [`(prim ,x ...)
           `(prim ,@x)]

         [(? prim? p)
          `(lambda x (apply-prim ,p x))]

         [`(,(? prim? p) ,es ...)
           `(prim ,p ,@(map desugar-aux es))]

         [(? symbol? s)
          s]

         [`(quote ,dat)
           `(quote ,dat)]

         [`(,fn ,args ...)
           (cons (desugar-aux fn) (map desugar-aux args))]

         [else '()]))

(define (desugar e)
  ; wrap e in any special functions or definitions you need
  ; and then pass it into a helper function that performs the
  ; case-by-case translation recursively
  `(let ([%my-call-cc (lambda (x) (call/cc x))])
     ,(desugar-aux (wrap-with-lib e))))

; I, Jack Dai, pledge on my honor that I have not given or
; received any unauthorized assistance on this project.
