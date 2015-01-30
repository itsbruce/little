(define ..
  (lambda (n m)
    (cond
      ((= n m) (list n))
      (else (cons n (.. ((if (< n m) + -) n 1) m))) )))

(define ..
  (lambda (n . m)
    (let
      ((n (if (null? m) 0 n))
       (m (if (null? m) n (car m))) )
      (cond
	((= n m) (list n))
	(else (cons n (.. ((if (< n m) + -) n 1) m))) ))))

(define add1
  (lambda (n)
    (+ n 1) ))

(define sub1
  (lambda (n)
    (- n 1) ))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))) )))
;      (else (o+ (add1 n) (sub1 m))) )))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))) )))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))) )))

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))) )))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))) )))

(define o>
  (lambda (n m)
    (cond
      ((= n 0) #f)
      ((= m 0) #t)
      (else (o> (sub1 n) (sub1 m))) )))

(define o<
  (lambda (n m)
    (cond
      ((= m 0) #f)
      ((= n 0) #t)
      (else (o< (sub1 n) (sub1 m))) )))

(define o=
  (lambda (n m)
    (not (or (o> n m) (o< n m))) ))

(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (^ n (sub1 m)))) )))

(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))) )))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))) )))

(define rempick
  (lambda (n lat)
    (cond
      ((null? lat) '())
      ((= n 1) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))) )))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))) )))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))) )))

(define eqan?
  (lambda (a b)
    (and
      (and (atom? a) (atom? b))
      (or
	(and (number? a) (number? b) (= a b))
	(and (not (number? a)) (not (number? b)) (eq? a b)) ))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))) )))

(define one?
  (lambda (n)
    (and (number? n) (eqan? n 1)) ))
