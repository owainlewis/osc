;; *****************************************
;; Scheme std lib
;; ******************************************

(define caar
  (lambda (pair)
    (car (car pair))))

(define cadr
  (lambda (pair)
    (car (cdr pair))))

(define cdar
  (lambda (pair)
    (cdr (car pair))))

(define cddr
  (lambda (pair)
    (cdr (cdr pair))))
