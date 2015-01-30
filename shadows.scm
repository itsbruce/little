(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))) )))

(define value
  (lambda (nexp)
    (define operator (lambda (nexp) (car nexp)))
    (define sub-exp-1 (lambda (nexp) (car (cdr nexp))))
    (define sub-exp-2 (lambda (nexp) (cdr (cdr nexp))))
    (cond
      ((number? nexp) nexp)
      ((eq? (operator nexp) '+) (o+ (value (sub-exp-1 nexp)) (value ((sub-exp-2 nexp))))
      ((eq? (operator nexp) '-) (o- (value (sub-exp-1 nexp)) (value (sub-exp-2 nexp))))
      ((eq? (operator nexp) '^) (o^ (value (sub-exp-1 nexp)) (value (sub-exp-2 nexp)))) ))))

(define value
  (lambda (nexp)
    (define operator (lambda (nexp) (car nexp)))
    (define sub-exp-1 (lambda (nexp) (car (cdr nexp))))
    (define sub-exp-2 (lambda (nexp) (cdr (cdr nexp))))
    (define omap
      (lambda ()
	`((+ ,o+)
	  (- ,o-)
	  (^ ,o^) )))
    (cond
      ((number? nexp) nexp)
      ((assq (operator nexp) omap) => (lambda (func) (func (sub-exp-1 nexp) (sub-exp-2 nexp)))) )))

(define atom-to-function
  (lambda (a)
    (cond
      ((eq? a '+) o+)
      ((eq? a '-) o-)
      ((eq? a '^) o^) )))

(define value
  (lambda (nexp)
    (define operator (lambda (nexp) (car nexp)))
    (define sub-exp-1 (lambda (nexp) (car (cdr nexp))))
    (define sub-exp-2 (lambda (nexp) (cdr (cdr nexp))))
    (cond
      ((number? nexp) nexp)
      (else ((atom-to-function operator) (value (sub-exp-1 nexp)) (value (sub-exp-2 nexp)))) )))

