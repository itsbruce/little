(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a n lat)
    (cond
      ((number? n) (keep-looking a (pick n lat) lat))
      (else (eq? a n)) )))

(define shift
  (lambda (pair)
    (build (first (first pair)) (build (second (first pair)) (second pair))) ))
