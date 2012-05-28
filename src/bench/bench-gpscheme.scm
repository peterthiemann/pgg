#!/bin/sh
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

(display (list "size:" (count-cells (file->list "examples/gpscheme.scm"))))

(writelpp (cogen-driver '("examples/gpscheme.scm") '(wrapper 1 0 0 0 0 0))
	  "/tmp/gpscheme-0.scm")

(display ("binding-time analysis")) (newline)

,time (begin (cogen-driver '("examples/gpscheme.scm") '(wrapper 1 0 0 0 0 0)) 'done)
,time (begin (cogen-driver '("examples/gpscheme.scm") '(wrapper 1 0 0 0 0 0)) 'done)
,time (begin (cogen-driver '("examples/gpscheme.scm") '(wrapper 1 0 0 0 0 0)) 'done)
,time (begin (cogen-driver '("examples/gpscheme.scm") '(wrapper 1 0 0 0 0 0)) 'done)
,time (begin (cogen-driver '("examples/gpscheme.scm") '(wrapper 1 0 0 0 0 0)) 'done)

,exit
\EOF-STRING
