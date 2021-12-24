(load "utils.scm")
(use-modules (ice-9 hash-table)
	     (srfi srfi-1))

;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(define (get-segments)
  (let* ((lines (get-lines (get-filename 5)))
	 (matches
	  (map (lambda (line)
		 (map match:substring
		      (list-matches "[0-9]+" line)))
	       lines)))
    (map (lambda (match)
	   (cons (cons (string->number (list-ref match 0))    ;; x1
		       (string->number (list-ref match 1)))   ;; y1
		 (cons (string->number (list-ref match 2))    ;; x2
		       (string->number (list-ref match 3))))) ;; y2
	 matches)))

(define (first-point segment)
  (car segment))

(define (second-point segment)
  (cdr segment))

(define (get-x point)
  (car point))

(define (get-y point)
  (cdr point))

(define (get-x1 segment)
  (get-x (first-point segment)))

(define (get-x2 segment)
  (get-x (second-point segment)))

(define (get-y1 segment)
  (get-y (first-point segment)))

(define (get-y2 segment)
  (get-y (second-point segment)))

(define (horiz-segment? segment)
  (= (get-y1 segment) (get-y2 segment)))

(define (vert-segment? segment)
  (= (get-x1 segment) (get-x2 segment)))

(define (diag-segment? segment)
  (= (abs (- (get-x1 segment) (get-x2 segment)))
     (abs (- (get-y1 segment) (get-y2 segment)))))

(define (get-horiz-segments segments)
  (filter horiz-segment? segments))

(define (get-vert-segments segments)
  (filter vert-segment? segments))

(define (get-diag-segments segments)
  (filter diag-segment? segments))

(define (enter-point grid x y)
  (let* ((key `(,x ,y))
	(cur-val (hash-ref grid key))
	(new-val
	 (if cur-val
	     (+ 1 cur-val)
	     1)))
    (hash-set! grid key new-val)))

(define (get-range p1 p2)
  (let ((size (+ 1 (abs (- p1 p2))))
	(step
	 (if (> p1 p2)
	     -1
	     1)))
    (iota size p1 step)))

(define part1
  (letrec*
      ((segments (get-segments))
       (grid (make-hash-table))
       (vert-segments (get-vert-segments segments))
       (horiz-segments (get-horiz-segments segments)))
    (begin
      (for-each (lambda (vert-segment)
		  (let ((x (get-x1 vert-segment))
			(y1 (get-y1 vert-segment))
			(y2 (get-y2 vert-segment)))
		    (for-each (lambda (y)
				(enter-point grid x y))
			      (get-range y1 y2))))
		vert-segments)
      (for-each (lambda (horiz-segment)
		  (let ((y (get-y1 horiz-segment))
			(x1 (get-x1 horiz-segment))
			(x2 (get-x2 horiz-segment)))
		    (for-each (lambda (x)
				(enter-point grid x y))
			      (get-range x1 x2))))
		horiz-segments)
      (hash-count (lambda (key value)
		    (>= value 2))
		  grid))))

(define part2
  (letrec*
      ((segments (get-segments))
       (grid (make-hash-table))
       (vert-segments (get-vert-segments segments))
       (horiz-segments (get-horiz-segments segments))
       (diag-segments (get-diag-segments segments)))
    (begin
      (for-each (lambda (vert-segment)
		  (let ((x (get-x1 vert-segment))
			(y1 (get-y1 vert-segment))
			(y2 (get-y2 vert-segment)))
		    (for-each (lambda (y)
				(enter-point grid x y))
			      (get-range y1 y2))))
		vert-segments)
      (for-each (lambda (horiz-segment)
		  (let ((y (get-y1 horiz-segment))
			(x1 (get-x1 horiz-segment))
			(x2 (get-x2 horiz-segment)))
		    (for-each (lambda (x)
				(enter-point grid x y))
			      (get-range x1 x2))))
		horiz-segments)
      (for-each (lambda (diag-segment)
		  (let ((x1 (get-x1 diag-segment))
			(x2 (get-x2 diag-segment))
			(y1 (get-y1 diag-segment))
			(y2 (get-y2 diag-segment)))
		    (for-each (lambda (point)
				(let ((x (car point))
				      (y (cadr point)))
				  (enter-point grid x y)))
			      (zip (get-range x1 x2) (get-range y1 y2)))))
		diag-segments)
      (hash-count (lambda (key value)
		    (>= value 2))
		  grid))))

(display part1 part2)
