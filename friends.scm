(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))) )))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))) )))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))) )))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1))
      ((null? set2) #f)
      (else (and (member (car set1) set2) (subset? (cdr set1) set2))) )))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1)) ))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((or (null? set1) (null? set2)) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))) )))

(define intersect
  (lambda (set1 set2)
    (cond
      ((or (null? set1) (null? set2)) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)) )))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((null? set2) set1)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))) )))

(define intersect-all
  (lambda (l-set)
    (cond
      ((not (pair? l-set)) l-set)
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersect-all (cdr l-set)))) )))

(define a-pair?
  (lambda (l)
    (cond
      ((null? l) #f)
      ((atom? l) #f)
      ((null? (cdr l)) #f)
      ((null? (cdr (cdr l))) #t)
      (else #f) )))

(define p-first
  (lambda (p)
    (car p) ))

(define p-second
  (lambda (p)
    (car (cdr p)) ))

(define p-third
  (lambda (p)
    (car (cdr (cdr p))) ))

(define build
  (lambda (f s)
    (cons f (cons s '())) ))
