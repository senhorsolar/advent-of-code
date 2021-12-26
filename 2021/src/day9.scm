(load "utils.scm")
(use-modules (ice-9 hash-table))

(define (get-input)
  (let* ((lines (get-lines (get-filename 9))))
    (list->vector
     (map (lambda (line)
	    (list->vector (map char->number (string->list line))))
	  lines))))

(define (mat-ref mat i j)
  (vector-ref (vector-ref mat i) j))

(define (mat-set mat i j val)
  (vector-set! (vector-ref mat i) j val))

(define (get-nrows mat) (vector-length mat))
(define (get-ncols mat) (vector-length (vector-ref mat 0)))

(define (mat-ref? mat i j)
  (let ((nrows (get-nrows mat))
	(ncols (get-ncols mat)))
    (if (and (>= i 0) (< i nrows) (>= j 0) (< j ncols))
	(mat-ref mat i j)
	#f)))

(define test-mat (get-input))

(define (get-neighbors mat i j)
  (filter true?
	  `(,(mat-ref? mat (1- i) j) ;; above
	    ,(mat-ref? mat (1+ i) j) ;; below
	    ,(mat-ref? mat i (1- j)) ;; left
	    ,(mat-ref? mat i (1+ j)) ;; right
	   )))

(define (low-point? mat i j)
  (let ((neighbors (get-neighbors mat i j))
	(height (mat-ref mat i j)))
    (all? (map (lambda (neighbor) (< height neighbor))
	       neighbors))))

(define (get-low-points mat)
  (let ((nrows (get-nrows mat))
	(ncols (get-ncols mat)))
    (apply append
	   (map (lambda (i)
		  (map (lambda (j) (cons i j))
		       (filter (lambda (j)
				 (low-point? mat i j))
			       (iota ncols))))
		(iota nrows)))))

(define (basin-size mat low-point)
  (letrec* ((visited (make-hash-table))
	    (foo
	     (lambda (i j)
	       (let ((val (mat-ref? mat i j)))
		 (if (and val (not (= val 9)) (not (hash-ref visited `(,i ,j))))
		     (begin
		       (hash-set! visited `(,i ,j) #t)
		       (1+ (+ (foo (1- i) j)
			      (foo (1+ i) j)
			      (foo i (1- j))
			      (foo i (1+ j)))))
		     0))))
	    (i (car low-point))
	    (j (cdr low-point)))
    (foo i j)))
		     

(define part1
  (let ((mat (get-input)))
    (apply +
	   (map (lambda (point)
		  (let* ((i (car point))
			 (j (cdr point))
			 (height (mat-ref mat i j)))
		    (1+ height)))
		(get-low-points mat)))))

(define part2
  (let* ((mat (get-input))
	(basin-sizes (sort (map (lambda (low-point)
				  (basin-size mat low-point))
				(get-low-points mat))
			   >))
	(top-3 `(,(list-ref basin-sizes 0)
		 ,(list-ref basin-sizes 1)
		 ,(list-ref basin-sizes 2))))
    (apply * top-3)))
    
(display-ln part1 part2)
