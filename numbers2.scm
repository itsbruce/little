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

; Not at all tail recursive but no cond when not needed
(define x
  (lambda (i n)
    (if (zero? n) 0 (o+ i (x i (sub1 n)))) ))

(define tup+
  (lambda (t1 t2)
    (cond
      ((null? t1) t2)
      ((null? t2) t1)
      (else (cons (o+ (car t1) (car t2)) (tup+ (cdr t1) (cdr t2)))) )))
