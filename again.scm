(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x))) ))

(define (lat? list)
  (or
    (null? list)
    (and
      (not (atom? list))
      (atom? (car list))
      (lat? (cdr list)) )))

(define lat?
  (lambda (l)
    (or
      (null? l)
      (and
	(not (atom? l))
	(atom? (car l))
	(lat? (cdr l) )))))


(define member?
  (lambda (a lat)
    (and
      (not (null? lat))
      (or
	(eq? a (car lat))
	(member? a (cdr lat)) ))))
