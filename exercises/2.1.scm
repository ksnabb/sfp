(load "../sdf/manager/load")
(manage 'new-environment 'combinators)

(define (compose f g) 
  (assert (= 1 (get-arity f)))
  (let ((n (get-arity g)))
    (define (the-composition . args)
      (assert (= (length args) n))
      (f (apply g args)))
    (restrict-arity the-composition n)))

;((compose (lambda (x) (+ x 1)) (lambda (x) (+ x 2))) 1)
'expect-value: 4

;((compose (lambda (x) (+ x 1)) (lambda (x) (+ x 2))) 1 2)
'expect-value: 'fail

(define (parallel-combine h f g)
  (assert (= 2 (get-arity h)))
  (let ((n (get-arity f)))
    (assert (= n (get-arity g)))
    (define (the-combination . args)
      (assert (= n (length args)))
      (h (apply f args) (apply g args)))
    (restrict-arity the-combination n)))

;((parallel-combine 
;   (lambda (x y) (+ x y))
;   (lambda (x) (+ x 1))
;   (lambda (x) (+ x 1))) 1)
'expect-value: 4

;((parallel-combine 
;   (lambda (x y) (+ x y))
;   (lambda (x y) (+ x 1))
;   (lambda (x) (+ x 1))) 1)
'expect-value: 'fail

;((parallel-combine 
;   (lambda (x y) (+ x y))
;   (lambda (x) (+ x 1))
;   (lambda (x) (+ x 1))) 1 2)
'expect-value: 'fail
