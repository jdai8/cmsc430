#lang racket

(provide compile)

(require "top-level.rkt" 
         "desugar.rkt" 
         "cps.rkt"
         "closure-convert.rkt"
         "utils.rkt")

(define (sequence . fs)
  (apply compose (reverse fs)))

(define compile
  (sequence top-level
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
         (call-with-input-file file 
                               (sequence read-begin
                                         compile
                                         display)
                               #:mode 'text))

