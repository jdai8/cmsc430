# CMSC 430 Final Project

A Scheme compiler that targets LLVM IR.

```bash
$ make
$ racket compile.rkt tests/amb.scm
$ ./amb
'(solution . (3 . (4 . (5 . ()))))
$ racket tests.rkt all
Test-final: expected #f, got 2
Test hash-1.scm failed!
Test coverage: 94.12%
```

## Part 1 - Primitive operations

### Math

- `+ : Int, Int -> Int` - adds two integers.

- `- : Int, Int -> Int` - subtracts two integers.

- `* : Int, Int -> Int` - multiples two integers.

- `/ : Int, Int -> Int` - divides two integers. Raises exception `1` if dividing by zero

- `number? : x -> Bool` - returns true if the value is a number.

### Comparisons

- `<  : Int, Int -> Bool` - returns true if the first integer is strictly less than the second.

- `<= : Int, Int -> Bool` - returns true if the first integer is less than or equal to the second.

- `=  : Int, Int -> Bool` - returns true if integers are equal.

- `>= : Int, Int -> Bool` - returns true if the first integer is greater than or equal to the second.

- `>  : Int, Int -> Bool` - returns true if the first integer is strictly greater than the second.

- `not : x -> Bool` - returns true if the value is not `#f`

### Lists

- `cons : x, y -> Cons x y` - creates a cons cell of two values.

- `cdr : Cons x y -> y` - returns the second value of a cons cell.

- `car : Cons x y -> x` - returns the first value of a cons cell.

- `null? : x -> Bool` - returns true if a value is null.

- `append : List, x -> y` - adds the second argument to the end of the list.

- `list : x ... -> List` - creates a list with its arguments.

### Other

- `void : () -> void` - returns void.

- `halt : x -> ()` - exits the program with given value.

## Part 2 - Error handling

I've implemented some better handling of run-time errors. These errors now produce more helpful messages.

Tests that produce errors have a comment at the top with their expected value.
This gets `read` by testing code.

Errors that produce exceptions (division by zero, calling non-functions) raise integers,
rather than (uninterned) symbols or strings.
I chose to do this because it makes exception types easier to check - we have `eq?` and `=`, but not `equal?`.

1. Division by zero

   Instead of throwing a floating point exception, programs now raise exception `1`.

   Implemented by adding a few lines to `desugar.rkt`:
   ```racket
    ; solve divide by zero
    [`(/ ,a ,b)
      `(if (prim = ,b '0)
        ,(desugar-aux '(raise '1))
        (prim / ,a ,b))]
    ['/
    `(lambda (a b)
        ,(desugar-aux '(/ a b)))]
   ```

   Tested by `divide-by-zero-0.scm` and `divide-by-zero-1.scm`.

2. Too few arguments

   Instead of exposing an "expected cons" error, calling functions with too few arguments
   now halts with the alarming, but more helpful value `'too-few-args!`.

   Implemented by adding `null?` checks in front of each `car` when pulling out named arguments.
   From `desugar.rkt`:
   ```racket
    [`(lambda (,xs ... . ,last) ,e)
      (define params (gensym 'params))
      `(lambda ,params
          ,(desugar-aux
            `(let* ,(foldr (lambda (x binds)
                              ; handle too few arguments
                              (append `([_ (if (null? ,params) (halt 'too-few-args!) 'foo)]
                                        [,x (car ,params)]
                                        [,params (cdr ,params)])
                                      binds))
                            `([,last ,params])
                            xs)
                ,e)))]
   ```
   From `closure-convert.rkt`:
   ```racket
    (cons gx
          `(let ([,n0 (prim null? ,gx)])
            (if ,n0
              (let ([,few 'too-few-args!])
                (let ([,hlt (prim halt ,few)])
                  (,hlt ,hlt)))
              (let ([,x (prim car ,gx)])
                (let ([,(car gx+e) (prim cdr ,gx)])
                  ,(cdr gx+e))))))
   ```

   Tested by `too-few-0.scm` and `too-few-1.scm`.

3. Too many arguments

   Previously, calling a function with too many arguments actually behaved as if you had called it correctly
   (with extra arguments ignored). However, to prevent possible bugs, programs now halt with `'too-many-args!`
   if you try to do this.

   Implemented by adding a `null?` check after pulling off all named arguments.
   From `closure-convert.rkt`:
   ```racket
    `(let ([,n1 (prim null? ,last)])
      (if ,n1
        ,(remove-varargs body vst)
        (let ([,many 'too-many-args!])
          (let ([,hlt (prim halt ,many)])
            (,hlt ,hlt)))))
    ```

    Tested by `too-many-0.scm` and `too-many-1.scm`.

4. Handling `(void)`, `#t`, and `#f`

   Programs that evaluate to void now print `(void)` instead of `"unrecognized value 39"`.
   Similarly for `#t` and `#f`.

   Implemented by adding a few simple checks in `header.cpp`:
   ```c++
    else if (v == V_VOID)
        printf("(void)");
    else if (v == V_TRUE)
        printf("#t");
    else if (v == V_FALSE)
        printf("#f");
   ```

   Tested by `void-0.scm` and `void-1.scm`.

5. Applying non-function values

   Rather than seg-faulting, calling a non-function value now raises exception `5`.

   Implemented by checking `procedure?` before each `apply` and function application.
   Doing this in `top-level` lets us avoid checking the library functions added in `desugar`:
   ```racket
    [`(,f ,args ...)
      (if (prim? f)
        (map top-level (cons f args))
        (let ([gf (gensym 'f)])
          `(let ([,gf ,(top-level f)])
             (if (procedure? ,gf)
               (,gf ,@(map top-level args))
               (raise '5)))))]
    ; similar for apply
   ```

   Tested by `non-function-0.scm` and `non-function-1.scm`.

#### Unbound variables

Although this isn't a *run-time* error, I also added a more friendly compile-time error message for unbound variables.

We can `raise` during `alphatize` if a variable isn't in scope:
```racket
  [(? symbol? x)
    (hash-ref env x (lambda () (raise (~a "unbound variable: " x))))]
```
This is tested by `unbound.scm`.

#### Other run-time errors

- Using not-yet-initialized `letrec` or `letrec*` variables.

  Currently, I artifically initialize variables to `'undefined`, which could "leak" into user code.
  I considered solving this by initializing to `(lambda () (raise 'uninitialized))`, but that would require
  modifying callsites as well, which is more work.

- Buffer overflow: it's possible to `vector-set!` somewhere out of bounds.

- Integer overflow: `(+ 4294967296 1)` returns `1`.

- `eq?` doesn't work for symbols, since they are strings under the hood.

## Part 3 - HAMT

I've (partially) implemented `hash`, `hash-ref`, `hash-set`, and `hash-remove`, using Thomas Gilray's
HAMT implementation.

Unfortunately, my `hash-remove` does not completely work, as demonstrated by the test `hash-1.scm`.

Also, when trying to implement the default thunk for `hash-ref`, I kept getting `"expected cons"` errors
when trying to call the lambda.
I thought this was because desugaring a zero-argument thunk adds an placeholder variable, and then we convert all
lambdas to taking an argument list.
However, after adding two `prim_cons`s, it still gives me the error.

Please see my code at the bottom of `header.cpp`. Pull requests are appreciated 😛.

## Part 4 - Boehm GC

I've attempted to implement the tagging portion of Boehm GC.
I chose to use a tagging scheme that allocates a size 2 vector for each heap-allocated object.
The first element of this vector holds the tag.
In doing this, I made changes to `header.cpp` and `closure-convert.rkt`, where I modified `make-closure`,
`env-ref`, and `clo-app` cases to properly handle boxed closures.
However, this change led to many seg faults, and I've been unable to figure out why.
As a result, the changes are on a separate branch, `tagging`.

Again, any advice on how to fix this would be really appreciated.


## Credits

I used Thomas Gilray and Kristopher Micinski's HAMT implementation.

I used Thomas Gilray's `cps.rkt` and `closure-convert.rkt`.

While I would have liked to use the Boehm garbage collector, I didn't get there.


*I pledge on my honor that I have not given or received any unauthorized assistance on this assignment.*
