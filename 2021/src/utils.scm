;; Include modules
(use-modules (ice-9 textual-ports)
	     (rnrs io ports))

;; Get input filename given day
(define (get-filename day)
  (string-concatenate `("input/day" ,(number->string day) ".txt")))

;; Get all lines in a file as a list
(define (get-lines filename)
  (letrec* ((port (open-file filename "r"))
	    (port->list
	     (lambda (ls)
	       (let ((line (get-line port)))
		 (if (eof-object? line)
		     ls
		     (port->list (cons line ls)))))))
    (reverse (port->list `()))))

;; Sum over list
(define (sum ls)
  (apply + ls))

;; Displays each element in list per line
(define (display-ln arg . args)
  (begin
    (display arg)
    (display "\n")
    (if (not (null? args))
	(apply display-ln args))))

(define (false? obj)
  (not obj))

(define (true? obj)
  (not (false? obj)))

(define (all? ls)
  (= (length ls)
     (length
      (filter true? ls))))

(define (any? ls)
  (letrec
      ((check
	(lambda (ls)
	  (if (null? ls)
	      #f
	      (if (true? (car ls))
		  #t
		  (check (cdr ls)))))))
    (check ls)))
	      
(define (char->number char)
  (string->number (string char)))
