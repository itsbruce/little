(define rember
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))) )))

(define firsts
  (lambda (l)
    (cond
      ((null? l) l)
      ((cons (car (car l)) (firsts (cdr l)))) )))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))) )))
