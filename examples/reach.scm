(define (f x)
  (let* ((r (make-cell x))
	 (d (cell-ref r))
	 (y (cell-set! r 5))
	 (s (+ (cell-ref r) (cell-ref r))))
    (+ d s)))
