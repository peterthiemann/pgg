(define-syntax
  fluid-let
  (syntax-rules ()
    ((fluid-let ((var value) ...) e1 e2 ...)
     (fluid-let-aux () ((var value) ...) e1 e2 ...))))

(define-syntax
  fluid-let-aux
  (syntax-rules ()
    ;; base case
    ((fluid-let-aux ((var value x) ...) () e1 e2 ...)
     (let ((body (lambda () e1 e2 ...))
	   (x value) ...)
       (let ((swap
	      (lambda ()
		(let ((t var))
		  (set! var x)
		  (set! x t))
		...)))
	 (dynamic-wind swap body swap))))
    ;; generate temporaries
    ((fluid-let-aux (b ...) ((var value) more ...) e1 e2 ...)
     (fluid-let-aux (b ... (var value newtemp)) (more ...) e1 e2 ...))))

(define (main x1 y1)
  (define x x1)
  (define y y1)
  (define (g) (list x y))
  (list
   (fluid-let ((x 17) (y 4))
     (g))
   (g)))
