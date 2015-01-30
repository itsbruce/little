(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
	 ((eqan? a (car l)) (rember* a (cdr l)))
	 (else (cons (car l) (rember* a (cdr l)))) ))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))) )))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
	 ((eqan? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
	 (else (cons (car l) (insertR* new old (cdr l)))) ))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))) )))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
	 ((eqan? a (car l)) (add1 (occur* a (cdr l))))
	 (else (occur* a (cdr l))) ))
      (else (o+ (occur* a (car l)) (occur* a (cdr l)))) )))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
	 ((eqan? old (car l)) (cons new (subst* new old (cdr l))))
	 (else (cons (car l) (subst* new old (cdr l)))) ))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))) )))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
	 ((eqan? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
	 (else (cons (car l) (insertL* new old (cdr l)))) ))
       (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))) )))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eqan? a (car l)) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))) )))

(define leftmost
  (lambda (l)
    (cond
;      ((null? l) '())
      ((atom? (car l)) (car l))
      (else (leftmost (car l))) )))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((null? l1) (null? l2))
      ((null? l2) #f)
      ((atom? (car l1))
       (cond
	 ((not (atom? (car l2))) #f)
	 (else (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))) ))
      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))) )))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((null? l1) (null? l2))
      ((null? l2) #f)
      (else (and (equals? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))) )))

(define equals?
  (lambda (s1 s2)
    (cond
      ((atom? s1) (and (atom? s2) (eq? s1 s2)))
      ((atom? s2) #f)
      (else (eqlist? s1 s2)) )))

(define flatten
  (lambda (l)
    (cond
      ((not (pair? l)) l)
      (else
	(let ((left (flatten (car l))) (right (flatten (cdr l))))
	  (cond
	    ((null? left) right)
	    ((not (pair? left))
	     (cons left (cond
			  ((null? right) right)
			  ((pair? right) right)
			  (else (list right)) )))
	    (else (cons (car left) (flatten (cons (cdr left) right)))) ))))))
	  
