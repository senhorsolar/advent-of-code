(load "utils.scm")
(use-modules (srfi srfi-1))

(define (get-parsed-input)
  (letrec*
      ((filename (get-filename 4))
       (port (open-file filename "r"))
       (order (map
	       string->number
	       (string-split (get-line port) #\,)))
       (get-boards
	(lambda (cur-board boards)
	  (let ((line (get-line port)))
	    (cond ((eof-object? line)
		   (cons cur-board boards))
		  ((string=? line "")
		   (get-boards `() (cons cur-board boards)))
		  (else
		   (get-boards
		    (cons (filter true?
				  (map
				   string->number
				   (string-split line #\space)))
				  cur-board)
		    boards)))))))
    (begin
      (get-line port) ;; read past empty line
      (let ((boards (get-boards `() `())))
	`(,order . ,boards)))))

(define (get-order) (car (get-parsed-input)))
(define (get-boards) (cdr (get-parsed-input)))

(define mark-val #f)
(define (marked? val)
  (false? val))

;; Set board(row, col) = false
(define (mark-board board x)
  (for-each
   (lambda (row)
     (for-each
      (lambda (idx-val)
	(let ((idx (car idx-val))
	      (y (cadr idx-val)))
	  (if (if (number? y)
		   (= y x)
		   mark-val)
	      (list-set! row idx #f))))
      (zip (iota (length row)) row)))
   board))

;; Sum over all non-marked numbers in board
(define (sum-board board)
  (apply +
	 (map (lambda (row)
		(apply + (filter true? row)))
	 board)))

(define (get-row board i)
  (list-ref board i))

(define (get-rows board)
  (map (lambda (i)
	 (get-row board i))
       (iota (length board))))
       
(define (get-col board j)
  (map (lambda (row)
	 (list-ref row j))
       board))

(define (get-cols board)
  (map (lambda (j)
	 (get-col board j))
       (iota (length board))))

(define (get-diagonals board)
  (let ((idxs (iota (length board)))
	(f
	 (lambda (idx-row)
	   (let ((idx (car idx-row))
		 (row (cadr idx-row)))
	     (list-ref row idx)))))
    (list (map f (zip idxs board))
	  (map f (zip (reverse idxs) board)))))

(define (get-lists board)
  (append (get-rows board)
		       (append (get-cols board)
			       (get-diagonals board))))
	
(define (check-win board)
  (let ((all-marked?
	 (lambda (ls)
	   (= (length ls) (length (filter marked? ls))))))
    (any? (map all-marked? (get-lists board)))))

(define part1
  (letrec* ((boards (get-boards))
	    (order (get-order))
	    (get-first-win
	     (lambda (order val)
	       (begin
		 (for-each (lambda (board) (mark-board board val))
			   boards)
		 (let ((wins (filter check-win boards)))
		   (if (not (null? wins))
		       (car wins)
		       #f)))))
	    (iterate
	     (lambda (order)
	       (if (not (null? order))
		   (let* ((val (car order))
			 (win (get-first-win order val)))
		     (if win
			 (* (sum-board win) val)
			 (iterate (cdr order))))))))
    (iterate order)))

(define part2
  (letrec* ((boards (get-boards))
	    (order (get-order))
	    (get-all-wins
	     (lambda (order boards val)
	       (begin
		 (for-each (lambda (board) (mark-board board val))
			   boards)
		 (let ((wins (filter check-win boards)))
		   (if (not (null? wins))
		       wins
		       #f)))))
	    (iterate
	     (lambda (order boards last-win)
	       (if (not (null? order))
		   (let* ((val (car order))
			  (wins (get-all-wins order boards val)))
		     (if wins
			 (iterate (cdr order)
				  (fold (lambda (win boards) (delete win boards))
					boards
					wins)
				  (cons (car wins) val))
			 (iterate (cdr order)
				  boards
				  last-win)))
		   (let* ((board (car last-win))
			  (val (cdr last-win)))
		     (* (sum-board board) val))))))
    (iterate order boards `())))

(define test-board
  `(
    (1 2 #f)
    (1 #f 1)
    (#f 5 1)))

(define test-win (check-win test-board))

(display-ln part1 part2)
