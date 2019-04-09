; Developed using DrRacket, but it's basically Scheme
#lang racket

(define (transpose xss)
  (apply map list xss))

(define (multiply-row-columns Row Columns)
  (map (lambda (Col) (apply + (map * Row Col))) Columns))

(define (matrix-multiplication M1 M2)
  (map (lambda (Row) (multiply-row-columns Row (transpose M2))) M1))

(define (mult-mtx Matrices)
  (if (= (length Matrices) 1) (print Matrices)
      (mult-mtx (append (list (matrix-multiplication (car Matrices) (cadr Matrices))) (cddr Matrices)))))

; The matrices must be passed as a list of matrices (list of list of lists).
(define (demo)
  ( eval '(mult-mtx '(
              ((1 2)
               (3 4)
	       (1 0)
	       (0 1))
              
             ((1 2 3)
	      (4 5 6))
              
              ((1 0 0)
	       (0 0 1)
	       (0 1 0))
              )))
  )