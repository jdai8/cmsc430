#lang racket

(provide assignment-convert
         alphatize
         anf-convert
         cps-convert)


(require "utils.rkt")


; The output of assignment 2:
; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (e e ...)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (set! x e)
;     | (call/cc e)
;     | x
;     | (quote dat)


(define (assignment-convert e)
  ; Suggestion: compute a set of mutated variables and save here.
  ; I.e., a set of all x where there exists a (set! x ...).
  (define alphatized (alphatize e))

  (define (join-mutated-vars es)
    (foldl set-union (set) (map find-mutated-vars es)))

  (define (find-mutated-vars e)
    (match e
           [`(let ([,xs ,e0s] ...) ,e1)
             (join-mutated-vars (cons e1 e0s))]
           [`(lambda ,x ,e)
             (find-mutated-vars e)]
           [`(apply ,e0 ,e1)
             (join-mutated-vars (list e0 e1))]
           [`(prim ,op ,es ...)
             (join-mutated-vars es)]
           [`(apply-prim ,op ,e)
             (find-mutated-vars e)]
           [`(if ,e0 ,e1 ,e2)
             (join-mutated-vars (list e0 e1 e2))]
           [`(set! ,x ,e) (set x)]
           [`(call/cc ,e) (find-mutated-vars e)]
           [(? symbol? e) (set)]
           [`(quote ,dat) (set)]
           [app
             (join-mutated-vars app)]))

  (define mutated-vars (find-mutated-vars alphatized))
    
  ; A strategy like this can help you to avoid boxing variables that don't need to be boxed
  ; and slowing down compiled programs as a result.
  (define (boxify e)
    (match e
           ; box mutated vars at initialization,
           ; e.g., (let ([x '()]) ...) -> (let ([x (prim make-vector '1 '())]) ...)
           ; What happens to (lambda (x) ...) if x is mutated?
           
           ; .. all all other forms in the language ...

           [`(let ([,xs ,e0s] ...) ,e1)
             `(let ,(map (lambda (x e)
                           (list x
                                 (let ([e+ (boxify e)])
                                   (if (set-member? mutated-vars x)
                                     `(prim make-vector '1 ,e+)
                                     e+))))
                         xs e0s)
                ,(boxify e1))]

           [`(lambda (,xs ...) ,e)
             (let ([x->new (foldl (lambda (x x->new) 
                                    (if (set-member? mutated-vars x)
                                      (hash-set x->new x (gensym))
                                      x->new))
                                  (hash)
                                  xs)])
               `(lambda ,(map (lambda (x) (hash-ref x->new x (lambda () x))) xs)
                  (let ,(map 
                          (lambda (k) (list k `(prim make-vector '1 ,(hash-ref x->new k)))) 
                          (hash-keys x->new))
                    ,(boxify e))))]

           [`(lambda ,x ,e)
             (if (set-member? mutated-vars x)
               (let ([sym (gensym)])
                 `(lambda ,sym
                    (let ([,x (prim make-vector '1 ,sym)]) ,(boxify e))))
               `(lambda ,x ,(boxify e)))]

           [`(apply ,e0 ,e1)
             `(apply ,(boxify e0) ,(boxify e1))]

           [`(prim ,op ,es ...)
             `(prim ,op ,@(map boxify es))]

           [`(apply-prim ,op ,e)
             `(apply-prim ,op ,(boxify e))]

           [`(if ,e0 ,e1 ,e2)
             `(if ,(boxify e0) ,(boxify e1) ,(boxify e2))]

           [`(set! ,x ,e0)
            `(prim vector-set! ,x '0 ,(boxify e0))]

           [`(call/cc ,e)
             `(call/cc ,(boxify e))]

           [(? symbol? x)
            (if (set-member? mutated-vars x)
                `(prim vector-ref ,x '0)
                x)]

           [`(quote ,dat)
             `(quote ,dat)]

           [app
             (map boxify app)]))
  (boxify alphatized))


; assignment-convert => 

;;; set! is removed and replaced with vector-set!
; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (e e ...)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (call/cc e)
;     | x
;     | (quote dat)

; alphatize both takes and produces this language as well

(define (alphatize e)
  ; Defining curried rename function is convenient for mapping over lists of es
  (define ((rename env) e)
    (match e
           ; Rename all variables 
           [`(let ([,xs ,e0s] ...) ,e1)
            (define xs+ (map gensym xs))
            (define e0s+ (map (rename env) e0s))
            (define env+ (foldl (lambda (x x+ env) (hash-set env x x+)) env xs xs+))
            `(let ,(map list xs+ e0s+) ,((rename env+) e1))]

           [`(lambda (,xs ...) ,e0)
            (define xs+ (map gensym xs))
            (define env+ (foldl (lambda (x x+ env) (hash-set env x x+)) env xs xs+))
            `(lambda ,xs+ ,((rename env+) e0))]

           [`(lambda ,x ,e)
             (let ([sym (gensym)])
               `(lambda ,sym ,((rename (hash-set env x sym)) e)))]

           [`(apply ,e0 ,e1)
             `(apply ,((rename env) e0) ,((rename env) e1))]

           [`(prim ,op ,es ...)
             `(prim ,op ,@(map (rename env) es))]

           [`(apply-prim ,op ,e)
             `(apply-prim ,op ,((rename env) e))]

           [`(if ,e0 ,e1 ,e2)
             `(if ,((rename env) e0) ,((rename env) e1) ,((rename env) e2))]

           [`(set! ,x ,e)
             `(set! ,(hash-ref env x) ,((rename env) e))]

           [`(call/cc ,e)
             `(call/cc ,((rename env) e))]

           [`(quote ,dat)
            `(quote ,dat)]

           [(? symbol? x)
            (hash-ref env x)]

           [app
             (map (rename env) app)]))

  ((rename (hash)) e))

; Flanagan, et al, 1993 (Essence of compiling with continuations)
(define (anf-convert e)
  (define (normalize-ae e k)
    (normalize e (lambda (anf)
                   (match anf
                     [(? symbol? x)
                      (k x)]
                     [`(lambda ,xs ,e0)
                      (k `(lambda ,xs ,e0))]
                     [`(quote ,dat)
                       (k `(quote ,dat))]
                     [else
                      (define ax (gensym 'a))
                      `(let ([,ax ,anf])
                         ,(k ax))]))))
  (define (normalize-aes es k)
    (if (null? es)
        (k '())
        (normalize-ae (car es) (lambda (ae)
                                 (normalize-aes (cdr es)
                                                (lambda (aes)
                                                  (k `(,ae ,@aes))))))))
  (define (normalize e k)
    (match e
           [`(let () ,e)
             (k (anf-convert e))]
           [`(let ([,xs ,e0s] ...) ,e1)
             (k `(let ([,(car xs) ,(anf-convert (car e0s))]) 
                   ,(anf-convert `(let ,(map list (cdr xs) (cdr e0s)) ,e1))))]

           [`(lambda ,x ,e)
             (k `(lambda ,x ,(anf-convert e)))]

           [`(apply ,e0 ,e1)
             (normalize-aes (list e0 e1) (lambda (aes)
                                           (k (cons 'apply aes))))]
           [`(prim ,op ,es ...)
             (normalize-aes es (lambda (aes)
                                 (k `(prim ,op ,@aes))))]
           [`(apply-prim ,op ,e)
             (normalize-ae e (lambda (ae)
                               (k `(apply-prim ,op ,ae))))]
           [`(if ,e0 ,e1 ,e2)
             (normalize-ae e0 (lambda (ae)
                                (k `(if ,ae ,(anf-convert e1) ,(anf-convert e2)))))]
           [`(call/cc ,e)
             (normalize-ae e (lambda (ae)
                               (k `(call/cc ,ae))))]
           [(? symbol? x)
            (k x)]

           [`(quote ,dat)
             (k `(quote ,dat))]

           [`(,es ...)
             (normalize-aes es k)]))

  (normalize e (lambda (x) x)))

; anf-convert =>

; e ::= (let ([x e]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (prim op ae ...)
;     | (apply-prim op ae)
;     | (if ae e e)
;     | (call/cc ae)
;     | ae
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)


(define (cps-convert e)
  (define (T-ae ae)
    (match ae
      [`(lambda (,xs ...) ,e)
        (define k (gensym 'k))
        `(lambda (,k ,@xs) ,(T-e e k))]

      [`(lambda ,x ,e)
        (define k (gensym 'k))
        (define sym (gensym))
        `(lambda ,sym
           (let ([,x (prim cdr ,sym)])
             (let ([,k (prim car ,sym)])
               ,(T-e e k))))]

      [else ae]))

  (define (T-e e cae)
    (match e
      [`(let ([,x ,e0]) ,e1)
       (define _x (gensym '_))
       (T-e e0 `(lambda (,_x ,x) ,(T-e e1 cae)))]

      [`(apply ,aef ,aergs)
        (define sym (gensym))
        `(let ([,sym (prim cons ,cae ,aergs)])
           (apply ,(T-ae aef) ,sym))]

      [`(prim ,op ,aes ...)
        (define sym (gensym))
        `(let ([,sym (prim ,op ,@(map T-ae aes))])
           (,cae '0 ,sym))]

      [`(apply-prim ,op ,ae)
        (define sym (gensym))
        `(let ([,sym (apply-prim ,op ,(T-ae ae))])
           (,cae '0 ,sym))]

      [`(if ,ae ,e0 ,e1)
       `(if ,ae ,(T-e e0 cae) ,(T-e e1 cae))]

      [`(call/cc ,ae)
        `(,(T-ae ae) ,cae ,cae)]

      [(? symbol? x)
       `(,cae '0 ,x)]

      [`(lambda . ,rest)
       `(,cae '0 ,(T-ae e))]

      [`(quote ,dat)
        `(,cae '0 (quote ,dat))]

      [`(,aef ,aes ...)
       `(,(T-ae aef) ,cae ,@(map T-ae aes))]))

  (T-e e '(lambda (k x) (let ([_1 (prim halt x)]) (k x)))))

; cps-convert => 

; e ::= (let ([x (apply-prim op ae)]) e)
;     | (let ([x (prim op ae ...)]) e)
;     | (let ([x (lambda x e)]) e)
;     | (let ([x (lambda (x ...) e)]) e)
;     | (let ([x (quote dat)]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (if ae e e)
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)


