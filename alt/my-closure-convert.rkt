#lang racket

(require "utils.rkt")

(provide closure-convert
         proc->llvm)

; Pass that removes lambdas and datums as atomic and forces them to be let-bound
;   ...also performs a few small optimizations
(define (simplify-ae e)
  (define (wrap-aes aes wrap)
    (match-define (cons xs wrap+)
                  (foldr (lambda (ae xs+wrap)
                           (define gx (gensym 'arg))
                           (if (symbol? ae)
                               (cons (cons ae (car xs+wrap))
                                     (cdr xs+wrap))
                               (cons (cons gx (car xs+wrap))
                                     (lambda (e)
                                       (match ae
                                              [`(lambda ,xs ,body)
                                               `(let ([,gx (lambda ,xs ,(simplify-ae body))])
                                                  ,((cdr xs+wrap) e))]
                                              [`',dat
                                               `(let ([,gx ',dat])
                                                  ,((cdr xs+wrap) e))])))))
                         (cons '() wrap)
                         aes))
    (wrap+ xs))
  (match e
         [`(let ([,x (apply-prim ,op ,aes ...)]) ,e0)
          (wrap-aes aes (lambda (xs) `(let ([,x (apply-prim ,op ,@xs)]) ,(simplify-ae e0))))]
         [`(let ([,x (prim ,op ,aes ...)]) ,e0)
          (wrap-aes aes (lambda (xs) `(let ([,x (prim ,op ,@xs)]) ,(simplify-ae e0))))]

         [`(let ([,x (lambda ,xs ,elam)]) ,e0)
          `(let ([,x (lambda ,xs ,(simplify-ae elam))]) ,(simplify-ae e0))]

         [`(let ([,x ',dat]) ,e0)
          `(let ([,x ',dat]) ,(simplify-ae e0))]

         [`(apply ,ae0 ,ae1)
          (wrap-aes (list ae0 ae1) (lambda (xs) `(apply ,@xs)))]

         [`(if (lambda . ,_) ,et ,ef)
          (simplify-ae et)]
         [`(if '#f ,et ,ef)
          (simplify-ae ef)]
         [`(if ',dat ,et ,ef)
          (simplify-ae et)]
         [`(if ,(? symbol? x) ,et ,ef)
          `(if ,x ,(simplify-ae et) ,(simplify-ae ef))]

         [`(,aes ...)
          (wrap-aes aes (lambda (xs) xs))]))

; Helper to remove vararg lambdas/callsites
(define (remove-varargs e)
  (match e
         [`(let ([,x (apply-prim ,op ,y)]) ,e0)
          `(let ([,x (apply-prim ,op ,y)]) ,(remove-varargs e0))]
         [`(let ([,x (prim ,op ,xs ...)]) ,e0)
          `(let ([,x (prim ,op ,@xs)]) ,(remove-varargs e0))]
         [`(let ([,x (lambda (,xs ...) ,body)]) ,e0)
          ; turns (xs ...) into x and immediately into (x)
          ; by adding the needed car/cdr calls and let bindings
          (define gx (gensym 'rvp))
          (define gx+e
            (foldr (lambda (x gx+e)
                     (define gx (gensym 'rvp))
                     (cons gx
                           `(let ([,x (prim car ,gx)])
                              (let ([,(car gx+e) (prim cdr ,gx)])
                                ,(cdr gx+e)))))
                   (cons (gensym 'na) (remove-varargs body))
                   xs))
          `(let ([,x (lambda (,(car gx+e)) ,(cdr gx+e))])
             ,(remove-varargs e0))]
         [`(let ([,x (lambda ,y ,body)]) ,e0)
          `(let ([,x (lambda (,y) ,(remove-varargs body))])
             ,(remove-varargs e0))]
         [`(let ([,x ',dat]) ,e0)
          `(let ([,x ',dat]) ,(remove-varargs e0))]
         [`(apply ,f ,args)
          `(,f ,args)]
         [`(if ,x ,e0 ,e1)
          `(if ,x ,(remove-varargs e0) ,(remove-varargs e1))]
         [`(,f ,xs ...)
           (define g0 (gensym 'g))
           (define gs (map (lambda (_) (gensym 'g)) xs))
           (define lets 
             (foldl (lambda (x g prevg e)
                      `(let ([,g (prim cons ,x ,prevg)]) ,e))
                    `(,f ,(car gs))
                    xs
                    gs
                    (append (cdr gs) (list g0))))
           `(let ([,g0 '()]) ,lets)]))

; call simplify-ae on input to closure convert, then remove vararg callsites/lambdas
(define (closure-convert cps)
  (define scps (simplify-ae cps))
  (define no-varargs-cps (remove-varargs scps))
  (define (bottom-up e procs)
    (match e
      [`(let ([,x (apply-prim ,op ,arg)]) ,e0)
        (match-define `(,e0+ ,free+ ,procs+)
                      (bottom-up e0 procs))
        `((let ([,x (apply-prim ,op ,arg)]) ,e0+)
          ,(set-remove free+ x)
          ,procs+)]

      [`(let ([,x (prim ,op ,xs ...)]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
                     (bottom-up e0 procs))
       `((let ([,x (prim ,op ,@xs)]) ,e0+)
         ,(set-remove (set-union free+ (list->set xs)) x)
         ,procs+)]

      [`(let ([,x (lambda (,arg) ,body)]) ,e0)
       (match-define `(,e0+ ,free0+ ,procs0+)
                     (bottom-up e0 procs))
       (match-define `(,body+ ,freelam+ ,procs1+)
                     (bottom-up body procs0+))
       (define env-vars (set-remove freelam+ arg))
       (define ordered-env-vars (set->list env-vars))
       (define lamx (gensym 'lam))
       (define envx (gensym 'env))
       (define body++ (cdr (foldl (lambda (x count+body)
                                    (match-define (cons cnt bdy) count+body)
                                     (cons (+ 1 cnt)
                                           `(let ([,x (env-ref ,envx ,cnt)])
                                              ,bdy)))
                                  (cons 1 body+)
                                  ordered-env-vars)))
       `((let ([,x (make-closure ,lamx ,@ordered-env-vars)]) ,e0+)
         ,(set-remove (set-union free0+ env-vars) x)
         ((proc (,lamx ,envx ,arg) ,body++) . ,procs1+))]

      [`(let ([,x ',dat]) ,e0)
       (match-define `(,e0+ ,free+ ,procs+)
                     (bottom-up e0 procs))
       `((let ([,x ',dat]) ,e0+)
         ,(set-remove free+ x)
         ,procs+)]

      [`(if ,(? symbol? x) ,e0 ,e1)
       (match-define `(,e0+ ,free0+ ,procs0+)
                     (bottom-up e0 procs))
       (match-define `(,e1+ ,free1+ ,procs1+)
                     (bottom-up e1 procs0+))
       `((if ,x ,e0+ ,e1+)
         ,(set-union free1+ free0+ (set x))
         ,procs1+)]

      [`(,(? symbol? xs) ...)
       `((clo-app ,@xs)
         ,(list->set xs)
         ,procs)]))

  (match-define `(,main-body ,free ,procs) (bottom-up no-varargs-cps '()))
  (cons `(proc (main) ,main-body) procs))

(define (cons+ a b c)
  (cons (cons a (car c))
        (cons b (cdr c))))

(define (cdr+ b c)
  (cons (car c)
        (cons b (cdr c))))

(define (cdr-append b c) ; b is a list
  (cons (car c)
        (append b (cdr c))))

(define (e->llvm e)
  (match e
    [`(let ([,x (apply-prim ,op ,arg)]) ,e0)
      (cdr+
        (format "%~a = call i64 @~a(i64 %~a)"
                (c-name x)
                (prim-applyname op)
                (c-name arg))
        (e->llvm e0))]

    [`(let ([,x (prim cons ,a ,b)]) ,e0)
      (let ([p (gensym 'p)] [p1 (gensym 'p1)] [p-int (gensym 'pint)])
        (cdr-append
          (list
            (format "%~a = call i64* @alloc(i64 16)" p)
            (format "%~a = getelementptr inbounds i64, i64* %~a, i64 1" p1 p)
            (format "store i64 %~a, i64* %~a, align 8" (c-name a) p)
            (format "store i64 %~a, i64* %~a, align 8" (c-name b) p1)
            (format "%~a = ptrtoint i64* %~a to i64" p-int p)
            (format "%~a = or i64 %~a, 1" (c-name x) p-int))
          (e->llvm e0)))]

    [`(let ([,x (prim car ,l)]) ,e0)
      (let ([p (gensym 'p)] [p-int (gensym 'pint)])
        (cdr-append
          (list
            (format "%~a = and i64 %~a, -8" p-int (c-name l))
            (format "%~a = inttoptr i64 %~a to i64*" p p-int)
            (format "%~a = load i64, i64* %~a, align 8" (c-name x) p))
          (e->llvm e0)))]

    [`(let ([,x (prim cdr ,l)]) ,e0)
      (let ([p (gensym 'p)] [p-int (gensym 'pint)] [p1 (gensym 'p1)])
        (cdr-append
          (list
            (format "%~a = and i64 %~a, -8" p-int (c-name l))
            (format "%~a = inttoptr i64 %~a to i64*" p p-int)
            (format "%~a = getelementptr inbounds i64, i64* %~a, i64 1" p1 p)
            (format "%~a = load i64, i64* %~a, align 8" (c-name x) p1))
          (e->llvm e0)))]

    [`(let ([,x (prim ,op ,args ...)]) ,e0)
      (cdr+
        (format "%~a = call i64 @~a(~a)"
                (c-name x)
                (prim-name op)
                (string-join (map (lambda (arg) (format "i64 %~a" (c-name arg))) args) ", "))
        (e->llvm e0))]

    [`(let ([,x (make-closure ,lam ,env-vars ...)]) ,e0)
      (let ([cloptr (gensym 'cloptr)] [fptr0 (gensym 'fptr)] 
            [eptr0 (gensym 'eptr)] [nvars (length env-vars)])
        (cdr-append
          (flatten 
            (list
              (format "%~a = call i64* @alloc(i64 ~a)"
                      cloptr (* (+ nvars 1) 8))
              (map (lambda (env-var i)
                     (define eptr (gensym 'eptr))
                     (list (format "%~a = getelementptr inbounds i64, i64* %~a, i64 ~a"
                                   eptr cloptr i)
                           (format "store i64 %~a, i64* %~a"
                                   (c-name env-var) eptr)))
                   env-vars
                   (range 1 (+ nvars 1)))
              (format "%~a = getelementptr inbounds i64, i64* %~a, i64 0"
                      eptr0 cloptr)
              (format "%~a = ptrtoint void(i64,i64)* @~a to i64"
                      fptr0 (c-name lam))
              (format "store i64 %~a, i64* %~a"
                      fptr0 eptr0)
              (format "%~a = ptrtoint i64* %~a to i64"
                      (c-name x) cloptr)))
          (e->llvm e0)))]

    [`(let ([,x (env-ref ,env ,i)]) ,e0)
      (let ([envptr (gensym 'envptr)])
        (cdr-append
          (list
            (format "%~a = getelementptr inbounds i64, i64* %~a, i64 ~a"
                    envptr (c-name env) i)
            (format "%~a = load i64, i64* %~a, align 8"
                    (c-name x) envptr))
          (e->llvm e0)))]

    [`(let ([,x (quote ,dat)]) ,e0)
      (define cx (c-name x))
      (define (n->llvm n)
        (let ([g (gensym 'gptr)])
          (list (format "%~a = alloca i64" g)
                (format "store i64 ~a, i64* %~a" n g)
                (format "%~a = load i64, i64* %~a" cx g))))

      (match dat
        [(? string?)
         (define nbytes (+ (string-length dat) 1))
         (cons+
           (format "@~a = private unnamed_addr constant [~a x i8] c\"~a\\00\", align 8"
                   cx nbytes dat)
           (format (string-append "%~a = call i64 @const_init_string("
                                  "i8* getelementptr inbounds ([~a x i8], [~a x i8]* @~a, i32 0, i32 0))")
                   cx nbytes nbytes cx)
           (e->llvm e0))]

        [(? symbol?)
         (define s (symbol->string dat))
         (define nbytes (+ (string-length s) 1))
         (cons+
           (format "@~a = private unnamed_addr constant [~a x i8] c\"~a\\00\", align 8"
                   cx nbytes s)
           (format (string-append "%~a = call i64 @const_init_symbol("
                                  "i8* getelementptr inbounds ([~a x i8], [~a x i8]* @~a, i32 0, i32 0))")
                   cx nbytes nbytes cx)
           (e->llvm e0))]

        [(? integer?)
         (cdr+
           (format "%~a = call i64 @const_init_int(i64 ~a)"
                   cx dat)
           (e->llvm e0))]

        [#t (cdr-append (n->llvm 31) (e->llvm e0))]
        [#f (cdr-append (n->llvm 15) (e->llvm e0))]
        ['() (cdr-append (n->llvm 0) (e->llvm e0))]
        [else (pretty-print "should never happen")])]

    [`(clo-app ,f ,arg)
      (cons
        '()
        (let ([cloptr (gensym 'cloptr)] [gf (gensym 'f)] 
              [fptr (gensym 'fptr)] [i0ptr (gensym 'i0ptr)])
          (list
            (format "%~a = inttoptr i64 %~a to i64*"
                    cloptr (c-name f))
            (format "%~a = getelementptr inbounds i64, i64* %~a, i64 0"
                    i0ptr cloptr)
            (format "%~a = load i64, i64* %~a, align 8"
                    gf i0ptr)
            (format "%~a = inttoptr i64 %~a to void(i64,i64)*"
                    fptr gf)
            (format "musttail call fastcc void %~a(i64 %~a, i64 %~a)"
                    fptr (c-name f) (c-name arg))
            "ret void")))]

    [`(if ,guard ,e0 ,e1)
      (let ([cmp (gensym 'cmp)] [then (gensym 'then)] [else_ (gensym 'else)])
        (match-define (cons then-globals then-llvm) (e->llvm e0))
        (match-define (cons else-globals else-llvm) (e->llvm e1))
        (cons
          (append then-globals else-globals) 
          (flatten
            (list
              (format "%~a = icmp ne i64 %~a, 15"
                      cmp (c-name guard))
              (format "br i1 %~a, label %~a, label %~a"
                      cmp then else_)
              (string-append (symbol->string then) ":")
              then-llvm
              (string-append (symbol->string else_) ":")
              else-llvm))))]))

; Walk procedures and emit llvm code as a string
; (string-append "  %r0 = opcode i64 %a, %b \n"
;                "  %r1 = ... \n")
(define (proc->llvm procs)
  (match-define (cons globals llvm)
  (foldl (lambda (proc globals+s)
           (match-define `(proc (,lam ,args ...) ,body) proc)
           (match-define (cons globals llvm) (e->llvm body))
           (define llvm-pretty (map (lambda (s) (string-append "  " s)) llvm))
           (cons+ 
             globals
             (string-join 
               (flatten
                 (list
                   (match args
                     [`(,env ,arg)
                       (define genv (gensym 'genv))
                       (list
                         (format "define void @~a(i64 %~a, i64 %~a) {"
                                 (c-name lam) genv (c-name arg))
                         (format "  %~a = inttoptr i64 %~a to i64*"
                                 (c-name env) genv))]
                     [else
                       "define void @proc_main() {"])
                   llvm-pretty
                   "}"))
               "\n")
             globals+s))
         (cons '()
               (list
                 (string-join
                   (list
                     "define i32 @main() {"
                     "  call fastcc void @proc_main()"
                     "  ret i32 0"
                     "}")
                   "\n")))
         procs))
  (string-join
    (list
      (string-join (flatten globals) "\n")
      (string-join llvm "\n\n"))
    "\n"))

