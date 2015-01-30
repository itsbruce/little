(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else (cons (car l) (rember-f test? a (cdr l)))) )))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
	((null? l) '())
	((test? a (car l)) (cdr l))
	(else (cons (car l) ((rember-f test?) a (cdr l)))) ))))

(define insertL-f
  (lambda (test?)
    (lambda (n o lat)
      (cond
	((null? lat) '())
	((test? o (car lat)) (cons n lat))
	(else (cons (car lat) ((insertL test?) n o (cdr lat)))) ))))

(define insertR-f
  (lambda (test?)
    (lambda (n o lat)
      (cond
	((null? lat) '())
	((test? o (car lat)) (cons o (cons n (cdr lat))))
	(else (cons (car lat) ((insertR test?) n o (cdr lat)))) ))))

(define seql
  (lambda (a b c)
    (cons a (cons b c)) ))

(define seqr
  (lambda (a b c)
    (cons b (cons a c)) ))

(define seqs
  (lambda (a b c)
    (cons a c) ))

(define insert-g
  (lambda (seq)
    (lambda (n o lat)
      (cond
	((null? lat) '())
	((eq? o (car lat)) (seq n o (cdr lat)))
	(else (cons (car lat) ((insert-g seq) n o (cdr lat)))) ))))

(define insertL (insert-g (lambda (a b c) (cons a (cons b c)))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
	((null? lat) '())
	((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
	(else (cons (car lat) ((multirember-f test?) a (cdr lat)))) ))))

(define multiremberT
  (lambda (test? lat)
      (cond
	((null? lat) '())
	((test? (car lat)) (multiremberT test? (cdr lat)))
	(else (cons (car lat) (multiremberT test? (cdr lat)))) )))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      ((eq? a (car lat))
       (multirember&co a (cdr lat)
		       (lambda (newlat seen)
			 (col newlat (cons (car lat) seen)) )))
      (else (multirember&co a (cdr lat)
			    (lambda (newlat seen)
			      (col (cons (car lat) newlat) seen)) )))))

(define a-friend (lambda (x y) (null? y)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (define strategy
      (lambda (a)
	(cond
	  ((eq? a oldL) (lambda (lst) (cons new (cons a lst))))
	  ((eq? a oldR) (lambda (lst) (cons a (cons new lst))))
	  (else (lambda (lst) (cons a lst))) )))
    (let loop ((f (lambda (lst) (and lst))) (l lat))
      (if (null? l) (f l)
	(loop (lambda (x) (f ((strategy (car l)) x))) (cdr l)) ))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (define sequence
      (lambda (a lst)
	(cond
	  ((eq? a oldL) (cons new (cons a lst)))
	  ((eq? a oldR) (cons a (cons new lst)))
	  (else (cons a lst)) )))
    (let loop ((f (lambda (lst) (and lst))) (l lat))
      (if (null? l) (f l)
	(loop (lambda (x) (f (sequence (car l) x))) (cdr l)) ))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (define sequence
      (lambda (a lst)
	(cond
	  ((eq? a oldL) (cons new (cons a lst)))
	  ((eq? a oldR) (cons a (cons new lst)))
	  (else (cons a lst)) )))
    (fold-right sequence '() lat) ))

(define evens-only*
  (lambda (l)
    (cond
      ((not (pair? (car l)))
       (if (even? (car l)) (cons (car l) (evens-only* (cdr l)))
	 (evens-only* (cdr l)) ))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))) )))
