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

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cons old(cdr lat))))
      (else (cons (car lat) (insertL new old (cdr lat)))) )))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (subst new old (cdr lat)))) )))

(define subst2
  (lambda (new o1 o2 lat)
    (cond 
      ((null? lat) '())
      ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))) )))
