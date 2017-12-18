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

   Tested by `divide-by-zero.scm` and `divide-by-zero-1.scm`.

*I, Jack Dai, pledge on my honor that I have not given or received any unauthorized
 assistance on this assignment.*
