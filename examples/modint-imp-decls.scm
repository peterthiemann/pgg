(define-memo _memo 1)
(define-memo _access 1 'deferred)

(define-primitive zero? - pure)	; (all t t); (all x (-> (* b x) b))
(define-primitive null? - pure)
(define-primitive eq? - pure)
(define-primitive eqv? - pure)
(define-primitive + - pure)
(define-primitive - - pure)

(define-primitive dyn-error (-> b b) dynamic)

;;; abstract data type for register block

(define-primitive make-register - opaque)
(define-primitive register-ref - opaque)
(define-primitive register-set! - opaque)

;;; auxiliary

(define-syntax access
  (syntax-rules ()
    ((access modulename f)
     (let ((modulename1 modulename))
       (letrec ((jump (lambda-without-memoization ()
                        (_access modulename1
				 (lambda (mod-name mod-body)
				   (if (null? mod-body)
				       (dyn-error "invalid index")
				       (if (eq? modulename1 mod-name)
					   (f mod-name mod-body)
					   (jump))))))))
	 (jump))))))

(define-syntax copy
  (syntax-rules ()
    ((copy n l)
     (make-register n l))))
