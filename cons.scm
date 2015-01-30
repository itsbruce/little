(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))) )))

(define firsts
  (lambda (list)
    (cond
      ((null? list) '())
      (else (cons (car (car list)) (firsts (cdr list)))) )))

(define insertR
  (lambda (n o lat)
    (cond
      ((null? lat) '())
      ((eq? o (car lat)) (cons o (cons n (cdr lat))))
      (else (cons (car lat) (insertR n o (cdr lat)))) )))

(define insertL
  (lambda (n o lat)
    (cond
      ((null? lat) '())
      ((eq? o (car lat)) (cons n lat))
      (else (cons (car lat) (insertL n o (cdr lat)))) )))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))) )))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 (new o1 o2 (cdr lat))))) )))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))) )))

(define multiInsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (multiInsertR new old (cdr lat)))))
      (else (cons (car lat) (multiInsertR new old (cdr lat)))) )))

(define multiInsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cons old (multiInsertL new old (cdr lat)))))
      (else (cons (car lat) (multiInsertL new old (cdr lat)))) )))

(define multiSubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (multiSubst new old (cdr lat))))
      (else (cons (car lat) (multiSubst new old (cdr lat)))) )))
