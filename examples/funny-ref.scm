(define (main d1 d2)
  (let* ((a d1)
	 (b (lambda (x)
	      (set! a x)
	      x)))
    (cons a b)))
