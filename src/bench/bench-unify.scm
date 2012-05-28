#!/bin/sh
# -*- scheme -*-
# benchmarking unification
######################################################################
# results in /tmp/unify-*.scm where
# generating extension:
#	unify-0.scm
# residual programs:				unification spec wrt
#	unify-cst-555.scm			(cst 555)
#	unify-var-555.scm			(var 555)
#	unify-bin-var-1-var-1.scm		(bin (var 1) (var 1))
#	unify-bin-var-1-bin-cst-4711-var-1.scm	(bin (var 1) (bin (cst 4711) (var 1)))
######################################################################
scheme48 -h 4000000 << \EOF-STRING
,config ,load cogen-interfaces.scm
,load-package pgg
,open pgg auxiliary pgg-library cogen-boxops signals pretty-print

(define (ntimes n thunk)
    (let loop ((n n))
      (if (> n 0)
	  (begin (thunk) (loop (- n 1))))))

(display "************************************") (newline)
(display "benchmarking unification with pointers") (newline)

(display (list "size:" (count-cells (file->list "examples/unify.scm"))))

(writelpp (cogen-driver '("examples/unify.scm") '(main 0 1))
	  "/tmp/unify-0.scm")

(display ("binding-time analysis")) (newline)

,time (begin (cogen-driver '("examples/unify.scm") '(main 0 1)) 'done)
,time (begin (cogen-driver '("examples/unify.scm") '(main 0 1)) 'done)
,time (begin (cogen-driver '("examples/unify.scm") '(main 0 1)) 'done)
,time (begin (cogen-driver '("examples/unify.scm") '(main 0 1)) 'done)

(load "cogen-ctors.scm")
(load "/tmp/unify-0.scm")
(load "examples/unify-aux.scm")

(define test1 (dynamic-parse-term '(cst 555)))
(define test2 (dynamic-parse-term '(cst 444)))

,time (ntimes 10000 (lambda () (dynamic-unify test1 test2)))
,time (ntimes 10000 (lambda () (dynamic-unify test1 test2)))
,time (ntimes 10000 (lambda () (dynamic-unify test1 test2)))
,time (ntimes 10000 (lambda () (dynamic-unify test1 test2)))

(start-memo 1 $goal '(0 1) (list  '(cst 555) 'YYY))
(writelpp *residual-program* "/tmp/unify-cst-555.scm")

(load "/tmp/unify-cst-555.scm")
,time (ntimes 10000 (lambda () ($goal-1 test1)))
,time (ntimes 10000 (lambda () ($goal-1 test1)))
,time (ntimes 10000 (lambda () ($goal-1 test1)))
,time (ntimes 10000 (lambda () ($goal-1 test1)))

(define test3 (dynamic-parse-term '(bin (var 1) (var 1))))
(define test4 (dynamic-parse-term '(bin (var 1) (bin (cst 4711) (var 1)))))

(start-memo 1 $goal '(0 1) (list  '(bin (var 1) (var 1)) 'YYY))
(writelpp *residual-program* "/tmp/unify-bin-var-1-var-1.scm")

,time (ntimes 10000 (lambda () (dynamic-unify test3 test3)))

(load "/tmp/unify-bin-var-1-var-1.scm")

,time (ntimes 10000 (lambda () ($goal-1 test3)))

(start-memo 1 $goal '(0 1) (list  '(bin (var 1) (bin (cst 4711) (var 1))) 'YYY))
(writelpp *residual-program* "/tmp/unify-bin-var-1-bin-cst-4711-var-1.scm")

,time (ntimes 10000 (lambda () (dynamic-unify test4 test4)))

(load "/tmp/unify-bin-var-1-bin-cst-4711-var-1.scm")

,time (ntimes 10000 (lambda () ($goal-1 test4)))

(define (n-test-terms n term)
  (let loop ((n n) (test-terms '()))
    (if (> n 0)
	(loop (- n 1) (cons (dynamic-parse-term term) test-terms))
	test-terms)))

,exit
\EOF-STRING
