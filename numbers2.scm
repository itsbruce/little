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

; OK, here I accept the "no input is negative" schtick.  At least it's
; tailrec
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))) )))

; tailrec plus schtick
(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))) )))

; tailrec plus schtick
(define o=
  (lambda (n m)
    (cond
      ((and (zero? n) (zero? m)) #t)
      ((or (zero? n) (zero? m)) #f)
      (else (o= (sub1 n) (sub1 m))) )))

(define ^
  (lambda (n m)
    (cond
      ((zero? n) 0)
      ((zero? m) 1)
      (else (x n (^ n (sub1 m)))) )))

(define len
  (lambda (l)
    (if (null? l) 0 (o+ 1 (len (cdr l)))) ))

; Why not be zero based, ffs?  Meh.  tailrec at least. No cond.
; I realise this was a teaching aid but this could have been done 
; zero based etc with no confusion
(define pick
  (lambda (n l)
    (if (eq? n 1) (car l) (pick (sub1 n) (cdr l))) ))

; Not even tailrec.
(define rempick
  (lambda (n l)
    (if (eq? n 1) (cdr l) (cons (car l) (rempick (sub1 n) (cdr l)))) ))

(define no-nums
  (lambda (l)
    (if (null? l) '()
      (let ((a (car l)) (rest (no-nums (cdr l))))
        (if (number? a) rest (cons a rest)) ))))

(define all-nums
  (lambda (l)
    (if (null? l) '()
      (let ((a (car l)) (rest (all-nums (cdr l))))
        (if (number? a) (cons a rest) rest) ))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((number? a1) (and (number? a2) (o= a1 a2)))
      ((number? a2) #f)
      (else (eq? a1 a2)) )))

(define occur
  (lambda (a lat)
    (if (null? lat) 0
      (let ((rest (occur a (cdr lat))))
        (if (eq? a (car lat)) (add1 rest) rest) ))))
