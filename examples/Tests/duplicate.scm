(define (main x)
  (let ((f (if #t (lambda (y) y) 0)))
    (f (f x))))
