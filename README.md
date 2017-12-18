# CMSC 430 Final Project

A Scheme compiler that targets LLVM IR.

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

### Other

- `void : () -> void` - returns void.

- `halt : x -> ()` - exits the program with given value.

## Part 2 - Error handling

These errors now raise integer exceptions between 1 and 5 (based on the order below).
Using integers is easier for checking exception types, since we have `eq?` and `=`, but not `equal?`.

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

   Instead of exposing a "expected cons" error, calling functions with too few arguments
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

4. Handling `void`

   Programs that evaluate to void now print `(void)` instead of "unrecognized value 39".

   Implemented by adding a simple check in `header.cpp`:
   ```c++
    else if (v == V_VOID)
        printf("(void)");
   ```

   Tested by `void-0.scm` and `void-1.scm`.

*I, Jack Dai, pledge on my honor that I have not given or received any unauthorized
 assistance on this assignment.*
