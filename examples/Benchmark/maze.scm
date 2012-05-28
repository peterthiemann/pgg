; Generated by CF Analysis Development Version, June 1, 1995
; (cf:control 'poly 'if-split '!const-split '!verbose '!times '(cons-limit 8) '!check '!inline '!simplify '!clean)
(define cadr@1 (lambda (x) (car (cdr x))))
(define cddr@1 (lambda (x) (cdr (cdr x))))
(define caddr@1 (lambda (x) (car (cdr (cdr x)))))
(define cdddr@1 (lambda (x) (cdr (cdr (cdr x)))))
;(define list@1 (lambda a a))
(define list->vector@1
  (lambda (a)
    (letrec ((length@1
              (lambda (a)
                (letrec ((loop
                          (lambda (a len)
                            (if (null? a) len (loop (cdr a) (+ 1 len))))))
                  (loop a 0)))))
      (if (null? a)
          (vector)
          (let ((v (make-vector (length@1 a) (car a))))
            (letrec ((loop
                      (lambda (i a)
                        (if (null? a)
                            v
                            (begin (vector-set! v i (car a))
                                   (loop (+ 1 i) (cdr a)))))))
              (loop 1 (cdr a))))))))
;(define call/cc@1 (lambda (f) (letcc x (begin (f x)))))
(define bitwise-and (lambda (x y) (fxlogand x y)))
(define bitwise-or (lambda (x y) (fxlogor x y)))
(define bitwise-not (lambda (x) (fxlognot x)))
;;; PJT CHANGE BEGIN
;(define-structure (harr nrows
;			ncols
;			elts))
(define-data harr
  (make-harr harr-nrows harr-ncols))

;(define-structure (wall
;		   owner      	; Box that owns this wall.
;		   neighbor	; The other box bordering this wall.
;		   bit))	; Integer -- a bit identifying this wall in OWNER's box.
(define-data wall
  (make-wall wall-owner wall-neighbor wall-bit))

;(define-structure (box
;		   reachable	; Union/find set -- all reachable boxs.
;		   id)		; Identifying info (e.g., the coords of the box).
;		   ((walls -1)	; A bitset telling which walls are still standing.
;		    (parent #f)	; For DFS spanning tree construction.
;		    (mark #f)))    ; For marking the solution path.
(define-data box
  (really-make-box box-reachable box-id really-box-walls really-box-parent really-box-mark))
(define make-box
  (lambda (reachable id)
    (really-make-box reachable id (make-cell -1) (make-cell #f) (make-cell #f))))
(define box-walls
  (lambda (box) (cell-ref (really-box-walls box))))
(define box-parent
  (lambda (box) (cell-ref (really-box-parent box))))
(define box-mark
  (lambda (box) (cell-ref (really-box-mark box))))
(define set-box-walls!
  (lambda (box value) (cell-set! (really-box-walls box) value)))
(define set-box-parent!
  (lambda (box value) (cell-set! (really-box-parent box) value)))
(define set-box-mark!
  (lambda (box value) (cell-set! (really-box-mark box) value)))
;;; PJT CHANGE END (this should be automatic)

(define harr
  (lambda (r c) (make-harr r c (make-vector (fx* r c)))))
(define href
  (lambda (ha x y)
    (let ((r (fx/ y 2)) (c (fx/ x 3)))
      (vector-ref
        (harr-elts ha)
        (fx+ (fx* (harr-ncols ha) r) c)))))
(define hset!
  (lambda (ha x y val)
    (let ((r (fx/ y 2)) (c (fx/ x 3)))
      (vector-set!
        (harr-elts ha)
        (fx+ (fx* (harr-ncols ha) r) c)
        val))))
(define href/rc
  (lambda (ha r c)
    (vector-ref
      (harr-elts ha)
      (fx+ (fx* (harr-ncols ha) r) c))))
(define harr-tabulate
  (lambda (nrows ncols proc)
    (let ((v (make-vector (fx* nrows ncols))))
      (begin (letrec ((G13
                       (lambda (r)
                         (if (fx< r 0)
                             (void)
                             (begin (letrec ((G14
                                              (lambda (c i)
                                                (if (fx= c ncols)
                                                    (void)
                                                    (begin (vector-set!
                                                             v
                                                             i
                                                             (proc (fx* 3
                                                                        c)
                                                                   (fx+ (fx* 2
                                                                             r)
                                                                        (bitwise-and
                                                                          c
                                                                          1))))
                                                           (G14 (fx+ c 1)
                                                                (fx+ i 1))
                                                           (void))))))
                                      (G14 0 (fx* r ncols)))
                                    (G13 (fx- r 1))
                                    (void))))))
               (G13 (fx- nrows 1)))
             (make-harr nrows ncols v)))))
(define harr-for-each
  (lambda (proc harr)
    (vector-for-each proc (harr-elts harr))))
(define south-west 1)
(define south 2)
(define south-east 4)
(define gen-maze-array
  (lambda (r c)
    (harr-tabulate
      r
      c
      (lambda (x y) (my-make-box (base-set 1) (cons x y))))))
(define make-wall-vec
  (lambda (harr)
    (let ((nrows (harr-nrows harr)))
      (let ((ncols (harr-ncols harr)))
        (let ((xmax (fx* 3 (fx- ncols 1))))
          (let ((walls (box ())))
            (let ((add-wall
                   (lambda (o n b)
                     (set-box!
                       walls
                       (cons (make-wall o n b) (unbox walls))))))
              (begin (letrec ((G15
                               (lambda (x)
                                 (if (fx< x 0)
                                     (void)
                                     (begin (letrec ((G17
                                                      (lambda (y)
                                                        (if (fx<= y 1)
                                                            (void)
                                                            (begin (let ((hex
                                                                          (href harr
                                                                                x
                                                                                y)))
                                                                     (begin (if (not (zero?
                                                                                       x))
                                                                                (add-wall
                                                                                  hex
                                                                                  (href harr
                                                                                        (fx- x
                                                                                             3)
                                                                                        (fx- y
                                                                                             1))
                                                                                  south-west)
                                                                                (void))
                                                                            (add-wall
                                                                              hex
                                                                              (href harr
                                                                                    x
                                                                                    (fx- y
                                                                                         2))
                                                                              south)
                                                                            (if (fx< x
                                                                                     xmax)
                                                                                (add-wall
                                                                                  hex
                                                                                  (href harr
                                                                                        (fx+ x
                                                                                             3)
                                                                                        (fx- y
                                                                                             1))
                                                                                  south-east)
                                                                                (void))))
                                                                   (G17 (fx- y
                                                                             2))
                                                                   (void))))))
                                              (G17 (fx+ (fx* (fx- nrows 1)
                                                             2)
                                                        (bitwise-and
                                                          x
                                                          1))))
                                            (G15 (fx- x 3))
                                            (void))))))
                       (G15 (fx* (fx- ncols 1) 3)))
                     (if (fx> ncols 1)
                         (let ((rmoc-x
                                (fx+ 3 (fx* 6 (fx/ (fx- ncols 2) 2)))))
                           (begin (let ((rmoc-hex (href harr rmoc-x 1)))
                                    (begin (if (fx< rmoc-x xmax)
                                               (add-wall
                                                 rmoc-hex
                                                 (href harr xmax 0)
                                                 south-east)
                                               (void))
                                           (add-wall
                                             rmoc-hex
                                             (href harr (fx- rmoc-x 3) 0)
                                             south-west)))
                                  (letrec ((G16
                                            (lambda (x)
                                              (if (fx< x 3)
                                                  (void)
                                                  (begin (add-wall
                                                           (href harr x 1)
                                                           (href harr
                                                                 (fx- x 3)
                                                                 0)
                                                           south-west)
                                                         (add-wall
                                                           (href harr x 1)
                                                           (href harr
                                                                 (fx+ x 3)
                                                                 0)
                                                           south-east)
                                                         (G16 (fx- x 6))
                                                         (void))))))
                                    (G16 (fx- rmoc-x 6)))))
                         (void))
                     (list->vector@1 (unbox walls))))))))))
(define pick-entrances
  (lambda (harr)
    (begin (dfs-maze harr (href/rc harr 0 0) for-each-hex-child)
           (let ((nrows (harr-nrows harr)) (ncols (harr-ncols harr)))
             (letrec ((tp-lp
                       (lambda (max-len entrance exit@1 tcol)
                         (if (fx< tcol 0)
                             (list@1 entrance exit@1)
                             (let ((top-box
                                    (href/rc harr (fx- nrows 1) tcol)))
                               (begin (reroot-maze top-box)
                                      (let ((G18
                                             (letrec ((bt-lp
                                                       (lambda (max-len
                                                                entrance
                                                                exit@1
                                                                bcol)
                                                         (if (fx< bcol 0)
                                                             (list@1
                                                               max-len
                                                               entrance
                                                               exit@1)
                                                             (let ((this-len
                                                                    (path-length
                                                                      (href/rc
                                                                        harr
                                                                        0
                                                                        bcol))))
                                                               (if (fx> this-len
                                                                        max-len)
                                                                   (bt-lp
                                                                     this-len
                                                                     tcol
                                                                     bcol
                                                                     (fx- bcol
                                                                          1))
                                                                   (bt-lp
                                                                     max-len
                                                                     entrance
                                                                     exit@1
                                                                     (fx- bcol
                                                                          1))))))))
                                               (bt-lp
                                                 max-len
                                                 entrance
                                                 exit@1
                                                 (fx- ncols 1)))))
                                        (if (and (pair? G18)
                                                 (pair? (cdr G18))
                                                 (pair? (cddr@1 G18))
                                                 (null? (cdddr@1 G18)))
                                            ((lambda (max-len
                                                      entrance
                                                      exit@1)
                                               (tp-lp
                                                 max-len
                                                 entrance
                                                 exit@1
                                                 (fx- tcol 1)))
                                             (car G18)
                                             (cadr@1 G18)
                                             (caddr@1 G18))
                                            ((lambda (G21)
                                               (match:error
                                                 G21
                                                 '(match
                                                    (let bt-lp ((max-len
                                                                 max-len)
                                                                (entrance
                                                                 entrance)
                                                                (exit exit)
                                                                (bcol
                                                                 (fx- ncols
                                                                      1)))
                                                      (if (fx< bcol 0)
                                                          (list max-len
                                                                entrance
                                                                exit)
                                                          (let ((this-len
                                                                 (path-length
                                                                   (href/rc
                                                                     harr
                                                                     0
                                                                     bcol))))
                                                            (if (fx> this-len
                                                                     max-len)
                                                                (bt-lp
                                                                  this-len
                                                                  tcol
                                                                  bcol
                                                                  (fx- bcol
                                                                       1))
                                                                (bt-lp
                                                                  max-len
                                                                  entrance
                                                                  exit
                                                                  (fx- bcol
                                                                       1))))))
                                                    ((max-len
                                                       entrance
                                                       exit)
                                                     (tp-lp
                                                       max-len
                                                       entrance
                                                       exit
                                                       (fx- tcol 1))))))
                                             G18)))))))))
               (tp-lp -1 #f #f (fx- ncols 1)))))))
(define for-each-hex-child
  (lambda (proc harr box@1)
    (let ((walls (box-walls box@1)))
      (let ((id (box-id box@1)))
        (let ((x (car id)))
          (let ((y (cdr id)))
            (let ((nr (harr-nrows harr)))
              (let ((nc (harr-ncols harr)))
                (let ((maxy (fx* 2 (fx- nr 1))))
                  (let ((maxx (fx* 3 (fx- nc 1))))
                    (begin (if (not (bit-test walls south-west))
                               (proc (href harr (fx- x 3) (fx- y 1)))
                               (void))
                           (if (not (bit-test walls south))
                               (proc (href harr x (fx- y 2)))
                               (void))
                           (if (not (bit-test walls south-east))
                               (proc (href harr (fx+ x 3) (fx- y 1)))
                               (void))
                           (if (and (fx> x 0)
                                    (or (fx<= y maxy)
                                        (zero? (modulo x 6))))
                               (let ((nw (href harr (fx- x 3) (fx+ y 1))))
                                 (if (not (bit-test
                                            (box-walls nw)
                                            south-east))
                                     (proc nw)
                                     (void)))
                               (void))
                           (if (fx< y maxy)
                               (let ((n (href harr x (fx+ y 2))))
                                 (if (not (bit-test (box-walls n) south))
                                     (proc n)
                                     (void)))
                               (void))
                           (if (and (fx< x maxx)
                                    (or (fx<= y maxy)
                                        (zero? (modulo x 6))))
                               (let ((ne (href harr (fx+ x 3) (fx+ y 1))))
                                 (if (not (bit-test
                                            (box-walls ne)
                                            south-west))
                                     (proc ne)
                                     (void)))
                               (void)))))))))))))
(define make-maze
  (lambda (nrows ncols)
    (let ((boxs (gen-maze-array nrows ncols)))
      (let ((walls
             (permute-vec! (make-wall-vec boxs) (random-state 50))))
        (begin (dig-maze walls (fx* nrows ncols))
               (let ((G24 (pick-entrances boxs)))
                 (if (and (pair? G24)
                          (pair? (cdr G24))
                          (null? (cddr@1 G24)))
                     ((lambda (entrance exit@1)
                        (let ((exit-box (href/rc boxs 0 exit@1)))
                          (let ((walls (box-walls exit-box)))
                            (begin (reroot-maze
                                     (href/rc boxs (fx- nrows 1) entrance))
                                   (mark-path exit-box)
                                   (set-box-walls!
                                     exit-box
                                     (bitwise-and
                                       walls
                                       (bitwise-not south)))
                                   (list@1 boxs entrance exit@1)))))
                      (car G24)
                      (cadr@1 G24))
                     ((lambda (G27)
                        (match:error
                          G27
                          '(match
                             (pick-entrances boxs)
                             ((entrance exit)
                              (let* ((exit-box (href/rc boxs 0 exit))
                                     (walls (box-walls exit-box)))
                                (reroot-maze
                                  (href/rc boxs (fx- nrows 1) entrance))
                                (mark-path exit-box)
                                (set-box-walls!
                                  exit-box
                                  (bitwise-and walls (bitwise-not south)))
                                (list boxs entrance exit))))))
                      G24))))))))
(define pmaze
  (lambda (nrows ncols)
    (let ((G30 (make-maze nrows ncols)))
      (if (and (pair? G30)
               (pair? (cdr G30))
               (pair? (cddr@1 G30))
               (null? (cdddr@1 G30)))
          ((lambda (boxs entrance exit@1)
             (print-hexmaze boxs entrance))
           (car G30)
           (cadr@1 G30)
           (caddr@1 G30))
          ((lambda (G33)
             (match:error
               G33
               '(match
                  (make-maze nrows ncols)
                  ((boxs entrance exit) (print-hexmaze boxs entrance)))))
           G30)))))
(define print-hexmaze
  (lambda (harr entrance)
    (let ((nrows (harr-nrows harr)))
      (let ((ncols (harr-ncols harr)))
        (let ((display@1 (lambda (x) (void))))
          (let ((write-char@1 (lambda (x) (void))))
            (let ((ncols2 (fx* 2 (fx/ ncols 2))))
              (begin (letrec ((G36
                               (lambda (c)
                                 (if (fx>= c ncols)
                                     (void)
                                     (begin (display@1 "   ")
                                            (write-char@1
                                              (if (fx= c entrance)
                                                  #\space
                                                  #\_))
                                            (G36 (fx+ c 2))
                                            (void))))))
                       (G36 1))
                     (write-char@1 #\space)
                     (letrec ((G40
                               (lambda (c)
                                 (if (fx>= c ncols2)
                                     (void)
                                     (begin (display@1
                                              (if (fx= c entrance)
                                                  #\space
                                                  #\_))
                                            (display@1 "/")
                                            (display@1
                                              (dot/space
                                                harr
                                                (fx- nrows 1)
                                                (fx+ c 1)))
                                            (display@1 "\\")
                                            (G40 (fx+ c 2))
                                            (void))))))
                       (G40 0))
                     (if (odd? ncols)
                         (write-char@1
                           (if (fx= entrance (fx- ncols 1)) #\space #\_))
                         (void))
                     (letrec ((G37
                               (lambda (r)
                                 (if (fx< r 0)
                                     (void)
                                     (begin (write-char@1 #\/)
                                            (letrec ((G38
                                                      (lambda (c)
                                                        (if (fx>= c ncols2)
                                                            (void)
                                                            (begin (write-char@1
                                                                     (dot/space
                                                                       harr
                                                                       r
                                                                       (fx- c
                                                                            1)))
                                                                   (display-hexbottom
                                                                     (box-walls
                                                                       (href/rc
                                                                         harr
                                                                         r
                                                                         c)))
                                                                   (G38 (fx+ c
                                                                             2))
                                                                   (void))))))
                                              (G38 1))
                                            (if (odd? ncols)
                                                (begin (write-char@1
                                                         (dot/space
                                                           harr
                                                           r
                                                           (fx- ncols 1)))
                                                       (write-char@1 #\\))
                                                (void))
                                            (letrec ((G39
                                                      (lambda (c)
                                                        (if (fx>= c ncols2)
                                                            (void)
                                                            (begin (display-hexbottom
                                                                     (box-walls
                                                                       (href/rc
                                                                         harr
                                                                         r
                                                                         c)))
                                                                   (write-char@1
                                                                     (dot/space
                                                                       harr
                                                                       (fx- r
                                                                            1)
                                                                       (fx+ c
                                                                            1)))
                                                                   (G39 (fx+ c
                                                                             2))
                                                                   (void))))))
                                              (G39 0))
                                            (if (odd? ncols)
                                                (display-hexbottom
                                                  (box-walls
                                                    (href/rc
                                                      harr
                                                      r
                                                      (fx- ncols 1))))
                                                (if (zero? r)
                                                    (void)
                                                    (write-char@1 #\\)))
                                            (G37 (fx- r 1))
                                            (void))))))
                       (G37 (fx- nrows 1)))))))))))
(define bit-test
  (lambda (j bit) (not (zero? (bitwise-and j bit)))))
(define dot/space
  (lambda (harr r c)
    (if (and (fx>= r 0) (box-mark (href/rc harr r c)))
        #\.
        #\space)))
(define display-hexbottom
  (lambda (hexwalls)
    (let ((write-char@1 (lambda (x) (void))))
      (begin (write-char@1
               (if (bit-test hexwalls south-west) #\\ #\space))
             (write-char@1 (if (bit-test hexwalls south) #\_ #\space))
             (write-char@1
               (if (bit-test hexwalls south-east) #\/ #\space))))))
(define my-make-box
  (lambda (r i)
    (let ((x (make-box r i)))
      (if (eq? (box-parent x) #f)
          x
          (error "my-make-box" "Not #f parent" x)))))
(define vector-for-each
  (lambda (proc v)
    (letrec ((lp
              (lambda (i)
                (if (fx>= i 0)
                    (begin (proc (vector-ref v i)) (lp (fx- i 1)))
                    (void)))))
      (lp (fx- (vector-length v) 1)))))
(define permute-vec!
  (lambda (v random-state)
    (begin (letrec ((lp
                     (lambda (i)
                       (if (fx> i 1)
                           (begin (let ((elt-i (vector-ref v i))
                                        (j (random-int i random-state)))
                                    (begin (vector-set!
                                             v
                                             i
                                             (vector-ref v j))
                                           (vector-set! v j elt-i)))
                                  (lp (fx- i 1)))
                           (void)))))
             (lp (fx- (vector-length v) 1)))
           v)))
(define dig-maze
  (lambda (walls nboxs)
    (call/cc@1
      (lambda (quit)
        (vector-for-each
          (lambda (wall)
            (let ((c1 (wall-owner wall)))
              (let ((set1 (box-reachable c1)))
                (let ((c2 (wall-neighbor wall)))
                  (let ((set2 (box-reachable c2)))
                    (if (not (set-equal? set1 set2))
                        (let ((walls (box-walls c1))
                              (wall-mask (bitwise-not (wall-bit wall))))
                          (begin (union! set1 set2)
                                 (set-box-walls!
                                   c1
                                   (bitwise-and walls wall-mask))
                                 (if (fx= (set-size set1) nboxs)
                                     (quit #f)
                                     (void))))
                        (void)))))))
          walls)))))
(define dfs-maze
  (lambda (maze root do-children)
    (letrec ((search
              (lambda (node parent)
                (begin (set-box-parent! node parent)
                       (do-children
                         (lambda (child)
                           (if (not (eq? child parent))
                               (search child node)
                               (void)))
                         maze
                         node)))))
      (search root #f))))
(define reroot-maze
  (lambda (new-root)
    (begin (letrec ((lp
                     (lambda (node new-parent)
                       (let ((old-parent (box-parent node)))
                         (begin (set-box-parent! node new-parent)
                                (if old-parent
                                    (lp old-parent node)
                                    (void)))))))
             (lp new-root #f)))))
(define path-length
  (lambda (box@1)
    (letrec ((G41
              (lambda (len node)
                (if node (G41 (fx+ len 1) (box-parent node)) len))))
      (G41 0 (box-parent box@1)))))
(define mark-path
  (lambda (node)
    (letrec ((lp
              (lambda (node)
                (begin (set-box-mark! node #t)
                       (let ((G42 (box-parent node)))
                         (if G42 (lp G42) (void)))))))
      (lp node))))
(define random-state (lambda (n) (cons n #f)))
(define rand
  (lambda (state)
    (let ((seed (car state)) (A 111) (M 5) (Q 44488) (R 3399))
      (let ((hi (fx/ seed Q)))
        (let ((lo (modulo seed Q)))
          (let ((test (fx- (fx* A lo) (fx* R hi))))
            (let ((val (if (fx> test 0) test (fx+ test M))))
              (begin (set-car! state val) val))))))))
(define random-int
  (lambda (n state) (modulo (rand state) n)))
(define base-set (lambda (nelts) (cons nelts ())))
(define get-set-root
  (lambda (s)
    (letrec ((lp
              (lambda (r)
                (let ((next (cdr r)))
                  (if (pair? next)
                      (lp next)
                      (begin (if (not (eq? r s))
                                 (letrec ((lp
                                           (lambda (x)
                                             (let ((next (cdr x)))
                                               (if (eq? r next)
                                                   (void)
                                                   (begin (set-cdr! x r)
                                                          (lp next)))))))
                                   (lp s))
                                 (void))
                             r))))))
      (lp s))))
(define set-equal?
  (lambda (s1 s2) (eq? (get-set-root s1) (get-set-root s2))))
(define set-size (lambda (s) (car (get-set-root s))))
(define union!
  (lambda (s1 s2)
    (let ((r1 (get-set-root s1)))
      (let ((r2 (get-set-root s2)))
        (let ((n1 (set-size r1)))
          (let ((n2 (set-size r2)))
            (let ((n (fx+ n1 n2)))
              (if (fx> n1 n2)
                  (begin (set-cdr! r2 r1) (set-car! r1 n))
                  (begin (set-cdr! r1 r2) (set-car! r2 n))))))))))
;;(pmaze 1000 120)
