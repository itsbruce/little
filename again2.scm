; Revisiting Little Schemer after experience with many functional languages
;
(define atom?
  (lambda (a)
    (and (not (null? a)) (not (pair? a))) ))

(define lat?
  (lambda (l)
    (or
      (null? l)
      (and
	(not (atom? l))
	(atom? (car l))
	(lat? (cdr l)) ))))

(define member?
  (lambda (a l)
    (cond 
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))) )))
