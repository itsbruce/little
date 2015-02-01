; More functional revisiting of Little Schemer
;
(define add1
  (lambda (n)
    (+ 1 n) ))

(define sub1
  (lambda (n)
    (- n 1) ))

(define zero?
  (lambda (n)
    (eq? 0 n) ))

; Properly tail recursive
(define o+
  (lambda (x y)
    (if (zero? y) x (o+ (add1 x) (sub1 y)) )))

; Properly tail recursive
(define o-
  (lambda (x y)
    (if (zero? y) x (o- (sub1 x) (sub1 y)) )))

; Properly tail recursive without a helper function
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      ((null? (cdr tup)) (car tup))
      (else (addtup (cons (o+ (car tup) (cadr tup)) (cddr tup)))) )))
