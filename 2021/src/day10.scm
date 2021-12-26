(load "utils.scm")
(use-modules (ice-9 match)
	     (srfi srfi-1))

(define (get-input)
  (map string->list
       (get-lines (get-filename 10))))

(define (classify chunks)
  (letrec* ((open-chunk (string->char-set "([{<"))
	    (close-chunk (string->char-set ")]}>"))
	    (pairs `((#\( . #\))
		     (#\[ . #\])
		     (#\{ . #\})
		     (#\< . #\>)))
	    (loop
	     (lambda (chunks stack)
	       (if (null? chunks)
		   (if (null? stack)
		       'complete
		       (list `incomplete stack))
		   (let ((char (car chunks)))
		     (if (char-set-contains? open-chunk char)
			 (loop (cdr chunks) (cons char stack))
			 (let* ((open-char (car stack))
				(close-char (assoc-ref pairs open-char)))
			   (if (char=? close-char char)
			       (loop (cdr chunks) (cdr stack))
			       `(corrupted ,char)))))))))
    (loop chunks `())))

(define part1
  (let ((point-table
	 `((#\) . 3)
	   (#\] . 57)
	   (#\} . 1197)
	   (#\> . 25137)))
	(lines (get-input)))
    (apply +
	   (map
	    (lambda (chunks)
	      (match (classify chunks)
		     (`complete 0)
		     ((`incomplete _) 0)
		     ((`corrupted char)
		      (assoc-ref point-table char))))
	    lines))))

(define (get-middle-score scores)
  (let* ((n (length scores))
	 (mid-idx (truncate (/ n 2))))
    (list-ref (sort scores <) mid-idx)))

(define part2
  (let ((point-table
	 `((#\( . 1)
	   (#\[ . 2)
	   (#\{ . 3)
	   (#\< . 4)))
	(lines (get-input)))
    (get-middle-score
     (filter true?
	     (map
	      (lambda (chunks)
		(match (classify chunks)
		       (`complete #f)
		       ((`incomplete stack)
			(fold (lambda (cur prev)
				(+ (* 5 prev) cur))
			      0
			      (map (lambda (c)
				     (assoc-ref point-table c))
				   stack)))
		       ((`corrupted _) #f)))
	      lines)))))

(define get-incomplete
  (let ((lines (get-input)))
    (filter true?
	    (map (lambda (chunks)
		   (let ((classification (classify chunks)))
		     (match classification
			    (`complete #f)
			    ((`corrupted _) #f)
			    ((`incomplete stack) stack))))
		 lines))))
	    	
(display-ln part1 part2)
