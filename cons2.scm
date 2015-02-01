; Revisiting Little Schemer with benefit of FP experience
;
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

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))) )))

; Remove some code duplicaton here and use if because choices are binary
; Generally more functional approach from here on but not worrying
; about tail position
(define multiinsertR
  (lambda (new old lat)
    (if (null? lat) '()
      (let ((a (car lat))
            (rest (multiinsertR new old (cdr lat))) )
        (cons a (if (eq? old a) (cons new rest) rest)) ))))

; Replacing cond with if because choices are binary
(define multiinsertL
  (lambda (new old lat)
    (if (null? lat) '()
      (let ((a (car lat))
            (rest (cons a (multiinsertL new old (cdr lat)))) )
        (if (eq? old a) (cons new rest) rest) ))))

(define multisubst
  (lambda (new old lat)
    (if (null? lat) '()
      (let ((a (car lat)))
        (cons (if (eq? old a) new a) (multisubst new old (cdr lat))) ))))
