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
