#lang racket

(provide compile)

(require "top-level.rkt"
         "desugar.rkt"
         "cps.rkt"
         "closure-convert.rkt"
         "utils.rkt")

(define (sequence . fs)
  (apply compose (reverse fs)))

; wrap in a guard to display uncaught errors
(define (top-level-guard e)
  `(guard (x (else (halt (cons "uncaught error" x))))
          ,e))

(define compile
  (sequence 
    top-level-guard
    top-level
    desugar
    simplify-ir
    assignment-convert
    alphatize
    anf-convert
    cps-convert
    closure-convert
    proc->llvm))

(module+ main
         (define file (vector-ref (current-command-line-arguments) 0))
         (define name (car (string-split (last (string-split file "/")) ".")))
         (define ll-name (string-append name ".ll"))
         (define llvm (call-with-input-file file
                                            (sequence read-begin
                                                      compile)
                                            #:mode 'text))
         (call-with-output-file ll-name
                                (lambda (out) 
                                  (display (port->string (open-input-file "header.ll")) out)
                                  (display llvm out))
                                #:mode 'text
                                #:exists 'replace)
         (exit (system/exit-code (format "clang++ ~a -o ~a && chmod +x ~a" ll-name name name))))

