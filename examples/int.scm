(define-data my-list (my-nil) (my-cons my-car my-cdr)) 
(define-primitive cons - pure)
(define-primitive apply - apply)

(define (main exp names values)
  (let loop ((names names) (values values) (env (my-nil)))
    (if (null? names)
	(int exp env)
	(loop (cdr names) (cdr values)
	      (my-cons (my-cons (car names) (car values)) env)))))

(define (int exp env)
  (let loop ((exp exp))
    (define (int* exp*)
      (let recur ((exp* exp*))
	(if (null? exp*)
	    '()
	    (cons (loop (car exp*))
		  (recur (cdr exp*))))))
    (define (apply-prim op args)
      (apply (eval op (interaction-environment))
	     args))
    (cond
     ((constant? exp)
      exp)
     ((not (pair? exp))
      (lookup exp env))
     ((eq? (car exp) 'IF)
      (let ((test-exp (cadr exp))
	    (then-exp (caddr exp))
	    (else-exp (cadddr exp)))
	(if (loop test-exp)
	    (loop then-exp)
	    (loop else-exp))))
     ((eq? (car exp) 'LAMBDA)
      (lambda (y)
	(int (caddr exp) (my-cons (my-cons (caadr exp) y) env))))
     ((eq? (car exp) 'APPLY)
      ((loop (cadr exp))
       (loop (caddr exp))))
     (else
      (apply-prim (car exp) (int* (cdr exp)))))))

(define (constant? e)
  (or (boolean? e)
      (number? e)
      (and (pair? e) (eq? (car e) 'QUOTE))))

(define (lookup v env)
  (let loop ((env env))
    (if (eq? v (my-car (my-car env)))
	(my-cdr (my-car env))
	(loop (my-cdr env)))))

