(load "../sdf/manager/load")
(manage 'new-environment 'combinators)

(define min-arity-table (make-key-weak-eqv-hash-table))
(define max-arity-table (make-key-weak-eqv-hash-table))

(define (restrict-min-arity proc nargs)
  (hash-table-set! min-arity-table proc nargs)
  proc)

(define (restrict-max-arity proc nargs)
  (hash-table-set! max-arity-table proc nargs)
  proc)

(define (get-min-arity proc)
  (or (hash-table-ref/default min-arity-table proc #f)
      (let ((a (procedure-arity proc)))
        (procedure-arity-min a))))

(define (get-max-arity proc)
  (or (hash-table-ref/default max-arity-table proc #f)
      (let ((a (procedure-arity proc)))
	(procedure-arity-max a))))

(define (parallel-apply f g)
  (assert (eqv? (get-min-arity f) (get-min-arity g)))
  (assert (eqv? (get-max-arity f) (get-max-arity g)))
  (let ((n (get-min-arity f))
        (m (get-max-arity f)))
    (define (the-combination . args)
      (assert (and (<= n (length args)) (or (not m) (>= m (length args)))))
      (let-values ((fv (apply f args))
		   (gv (apply g args)))
      	(apply values (append gv fv))))
    (restrict-min-arity the-combination n)
    (restrict-max-arity the-combination m)))

;; the compose in sdf package does not accept variable arity procedures (min max)
(define (compose f g)
  (define (the-composition . args)
    (call-with-values (lambda () (apply f args))
      g))
  (restrict-min-arity the-composition (get-min-arity f))
  (restrict-max-arity the-composition (get-max-arity f)))


(define (discard-argument i) 
  (assert (exact-nonnegative-integer? i))
  (define (args-manipulator . args)
    (apply values (list-remove args i)))
  (restrict-min-arity args-manipulator 0)
  (restrict-max-arity args-manipulator #f))


(define ((curry-argument i) x)
  (define (args-manipulator . args)
    (apply values (list-insert args i x)))
  args-manipulator)


(define (permute-arguments . permspec)
  (let ((permute (make-permutation permspec)))
    (define (args-manipulator . args)
      (apply values (permute args)))
    args-manipulator))


((compose 
   (discard-argument 1)
   (lambda (x y) (+ x y))) 
 1 2 3)
'expect 4

((compose
   ((curry-argument 0) 10)
   (lambda (x y) (+ x y)))
 1)
'expect 11

((compose
   (permute-arguments 1 0)
   (lambda (x y) (- x y)))
 4 2)
'expect -2
