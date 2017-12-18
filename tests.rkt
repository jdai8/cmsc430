#lang racket

; Adapted from Thomas Gilray's assignment 5 testing apparatus

(require "desugar.rkt")
(require "cps.rkt")
(require "utils.rkt")
(require "top-level.rkt")
(require "compile.rkt")


(define testdir "tests")

(define (path->test file)
  (define path (build-path testdir file))
  (cons (path->string file)
        (lambda ()
          (call-with-input-file
            path 
            (lambda (input)
              (define s (port->string input))
              (define prog (read-begin (open-input-string s)))
              (define expected-val
                (if (string-prefix? s ";")
                  ; pull out the expected error value
                  (read (open-input-string (substring (car (string-split s "\n")) 1)))
                  (eval-top-level prog)))
              (test-final compile expected-val prog))))))

(define tests
  (map path->test 
       (filter (lambda (p)
                 (not (string-suffix? (path->string p) "swp")))
               (directory-list testdir))))

(define (run-test/internal is-repl . args)
  ;; Run all tests, a specific test, or print the available tests
  (match args
         [(list "all")
          (define correct-count
            (foldl (lambda (testcase count)
                     (match testcase
                            [(cons test-name exec)
                             (define exec-result
                               (with-handlers ([exn:fail? identity])
                                              (exec)))
                             (if (eq? exec-result #t)
                               (+ count 1)
                               (begin
                                 (display "Test ")
                                 (display test-name)
                                 (display " failed!")
                                 (newline)
                                 count))]))
                   0
                   tests))
          (display "Test coverage: ")
          (display (/ (round (/ (* 10000 correct-count) (length tests))) 100.0))
          (display "%")
          (newline)]

         [(list test-name)
          #:when (assoc test-name tests)
          (match (assoc test-name tests)
                 [(cons _ exec)
                  (define exec-result
                    (with-handlers ([exn:fail? identity])
                                   (exec)))
                  (define passed (eq? exec-result #t))
                  (displayln (if passed "Test passed!" "Test failed!"))
                  (unless is-repl
                          (exit (if (eq? exec-result #t) 0 1)))])]
         [else
          (display "Available tests: ")
          (newline)
          (display
           (string-join
            (map car tests)
            ", "))
          (newline)]))

(define run-test
  (curry run-test/internal #t))

(apply
 run-test/internal
 (cons
  #f
    (vector->list (current-command-line-arguments))))



