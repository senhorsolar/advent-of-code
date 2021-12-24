;; Load modules
(load "utils.scm")
(use-modules (srfi srfi-1)) ;; for zip

;; List of numbers from input
(define lines (map string->number
		   (get-lines (get-filename 1))))

(define (increase? pair)
  (let ((a (car pair))
	(b (cadr pair)))
    (> b a)))

(define part1
  (let* ((pairs (zip lines (cdr lines))))
    (length (filter increase? pairs))))
	 
(define part2
  (let* ((three-windows
	  (map
	   (lambda (ls) (apply + ls))
	   (zip lines (cdr lines) (cddr lines))))
	 (pairs (zip three-windows (cdr three-windows))))
    (length (filter increase? pairs))))

(display-ln part1 part2)
