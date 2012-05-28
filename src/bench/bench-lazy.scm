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

(display "************************************") (newline)
(display "benchmarking unification with pointers") (newline)

(display (list "size:" (count-cells (file->list "examples/2lazy.scm"))))

(writelpp (cogen-driver '("examples/2lazy.scm") '(lazy-2int 0 0 0 1))
	  "/tmp/2lazy-0.scm")

(display ("binding-time analysis")) (newline)

,time (begin (cogen-driver '("examples/2lazy.scm") '(lazy-2int 0 0 0 1)) 'done)
,time (begin (cogen-driver '("examples/2lazy.scm") '(lazy-2int 0 0 0 1)) 'done)
,time (begin (cogen-driver '("examples/2lazy.scm") '(lazy-2int 0 0 0 1)) 'done)
,time (begin (cogen-driver '("examples/2lazy.scm") '(lazy-2int 0 0 0 1)) 'done)

(load "cogen-ctors.scm")
(load "examples/2lazy-support.scm")
(load "/tmp/2lazy-0.scm")

,time (ntimes 1000 (lambda () (lazy-2int lazy1 'f '(42) '())))

(start-memo 1 $goal '(0 0 0 1) (list lazy1 'f '(42) 'DYN))
(writelpp *residual-program* "/tmp/2lazy1-42.scm")
(load "/tmp/2lazy1-42.scm")

,time (ntimes 1000 (lambda () ($goal-1 '())))

(start-memo 1 $goal '(0 0 0 1) (list lazy1 'f '((CV 1)) 'DYN))
(writelpp *residual-program* "/tmp/2lazy1-CV1.scm")
(load "/tmp/2lazy1-CV1.scm")

,time (ntimes 1000 (lambda () ($goal-1 '(42))))


,time (ntimes 1000 (lambda () (lazy-2int lazy2 'f '(#t 5 7) '())))

(load "/tmp/2lazy-0.scm")
(start-memo 1 $goal '(0 0 0 1) (list lazy2 'f '((CV 1) (CV 2) (CV 3)) 'DYN))
(writelpp *residual-program* "/tmp/2lazy2-CV1-CV2-CV3.scm")
(load "/tmp/2lazy2-CV1-CV2-CV3.scm")

(display "*10000")
,time (ntimes 10000 (lambda () ($goal-1 '(#t 5 7))))

(start-memo 1 $goal '(0 0 0 1) (list lazy2 'f '(#t (CV 1) (CV 2)) 'DYN))
(writelpp *residual-program* "/tmp/2lazy2-true-CV2-CV3.scm")
(load "/tmp/2lazy2-true-CV2-CV3.scm")

(display "*10000")
,time (ntimes 10000 (lambda () ($goal-1 '(5 7))))

,exit
\EOF-STRING
