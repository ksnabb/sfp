; compose does not need to change, cause f should have arity 1 and g can have any arity
; as long as g output is one value

; parallel-compose needs to check that f and g has the same min and max arity, h arity should be 2

; spread-combine can have multiple solutions, of which all require a fixed arity for both
; f and g
; one is to require f and g to both have min arity == max arity
; two is to only use f and g min arity value for spreading the args
; three is to let the user define how many args go to f and how many to g
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

(define (parallel-combine h f g)
  (assert (eqv? 2 (get-arity h)))
  (assert (eqv? (get-min-arity f) (get-min-arity g)))
  (assert (eqv? (get-max-arity f) (get-max-arity g)))
  (let ((n (get-min-arity f))
	(m (get-max-arity f)))
    (define (the-combination . args)
      (assert (and (<= n (length args)) (or (not m) (>= m (length args)))))
      (h (apply f args) (apply g args)))
    (restrict-min-arity the-combination n)
    (restrict-max-arity the-combination m)))

((parallel-combine (lambda (x y) (+ x y)) + +) 1 1)
