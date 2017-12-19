#lang racket

(provide top-level)

(require "utils.rkt")

; By Jack Dai

(define (flatten-begins es)
  (foldr (lambda (e flattened)
           (match e
             [`(begin ,es ...)
               (append (flatten-begins es) flattened)]
             [else (cons e flattened)]))
         '()
         es))

(define (hoist-defines flat-es)
  (match-define (cons ds es)
    (foldr (lambda (e ds+es)
             (match-define (cons ds es) ds+es)
             (match e
               [`(define (,x ,args ... . ,rest) ,bodys ...)
                 (cons (cons `[,x (lambda (,@args . ,rest)
                                    ,(top-level `(begin ,@bodys)))]
                             ds)
                       es)]
               [`(define ,x ,e)
                 (cons (cons `[,x 'should-not-reach] ds)
                       (cons `(set! ,x ,e) es))]
               [else 
                 (cons ds 
                       (cons e es))]))
           (cons '() '())
           flat-es))
  `(letrec* ,ds (begin 'foo ,@(map top-level es))))

(define (top-level e)
  (define (cond-convert e)
    (match e
      [`(,e)
        `(,(top-level e))]
      [`(else ,es ...)
        `(else ,(top-level `(begin ,@es)))]
      [`(,e0 ,es ...)
        `(,(top-level e0) ,(top-level `(begin ,@es)))]))

  (define (convert-bindings bindings)
    (match-define `([,xs ,es] ...) bindings)
    (map list xs (map top-level es)))

  (match e
    [`(begin ,es ...)
      (hoist-defines (flatten-begins es))]

    [`(letrec* (,bs ...) ,e1s ...)
      `(letrec* ,(convert-bindings bs)
                ,(top-level `(begin ,@e1s)))]
    [`(letrec (,bs ...) ,e1s ...)
      `(letrec ,(convert-bindings bs)
               ,(top-level `(begin ,@e1s)))]
    [`(let* (,bs ...) ,e1s ...)
      `(let* ,(convert-bindings bs)
             ,(top-level `(begin ,@e1s)))]
    [`(let (,bs ...) ,e1s ...)
      `(let ,(convert-bindings bs)
            ,(top-level `(begin ,@e1s)))]
    [`(let ,loop (,bs ...) ,e1s ...)
      `(let ,loop ,(convert-bindings bs)
         ,(top-level `(begin ,@e1s)))]

    [`(lambda ,args ,es ...)
      `(lambda ,args ,(top-level `(begin ,@es)))]
    [`(guard (,x ,cond-clauses ...) ,es ...)
      `(guard (,x ,@(map cond-convert cond-clauses))
              ,(top-level `(begin ,@es)))]
    [`(raise ,e)
      `(raise ,(top-level e))]
    [`(delay ,e)
      `(delay ,(top-level e))]
    [`(force ,e)
      `(force ,(top-level e))]
    [`(and ,es ...)
      `(and ,@(map top-level es))]
    [`(or ,es ...)
      `(or ,@(map top-level es))]
    [`(match ,e ,match-clauses ...)
      'not-implemented]
    [`(cond ,cond-clauses ...)
      `(cond ,@(map cond-convert cond-clauses))]
    [`(case ,e ,case-clauses ...)
      `(case ,(top-level e)
         ,@(map (lambda (case-clause)
                  (list (car case-clause) (top-level `(begin ,@(cdr case-clause)))))
                case-clauses))]
    [`(if ,e0 ,e1 ,e2)
      `(if ,(top-level e0) ,(top-level e1) ,(top-level e2))]
    [`(when ,e ,es ...)
      `(when ,(top-level e) ,(top-level `(begin ,@es)))]
    [`(unless ,e ,es ...)
      `(unless ,(top-level e) ,(top-level `(begin ,@es)))]
    [`(set! ,x ,e)
      `(set! ,x ,(top-level e))]
    [`(call/cc ,e)
      `(call/cc ,(top-level e))]
    [`(apply ,e1 ,e2)
      (if (prim? e1) 
        `(apply ,(top-level e1) ,(top-level e2))
        (let ([gf (gensym 'f)])
          `(let ([,gf ,(top-level e1)])
             (if (procedure? ,gf)
               (apply ,gf ,(top-level e2))
               (raise '5)))))]

    [`(quasiquote ,qq)
      (match qq
        [(list 'unquote e) 
         (top-level e)]

        [`(,qq0 ,qqs ... . ,qqr)
          (match-define (cons qqs1 qqr1)
            (match qqs
              [`(,l ... ,unq ,x)
                (if (and (eq? unq 'unquote) (null? qqr))
                  (cons l (list unq x))
                  (cons qqs qqr))]
              [else (cons qqs qqr)]))

          (foldr (lambda (qq acc)
                   `(cons ,(top-level (list 'quasiquote qq)) ,acc))
                 (top-level (list 'quasiquote qqr1 ))
                 (cons qq0 qqs1))]

        [else `(quote ,qq)])]
    [(? prim?)
      e]
    [`(quote ,dat)
      e]
    [(? integer?)
      `(quote ,e)]
    [(? string?)
      `(quote ,e)]
    [#t
     '(quote #t)]
    [#f
     '(quote #f)]
    [(? symbol?)
     e]
    [`(,f ,args ...)
      (if (prim? f)
        (map top-level (cons f args))
        (let ([gf (gensym 'f)])
          `(let ([,gf ,(top-level f)])
             (if (procedure? ,gf)
               (,gf ,@(map top-level args))
               (raise '5)))))]))

; I, Jack Dai, pledge on my honor that I have not given or received any
;unauthorized assistance on this assignment.

