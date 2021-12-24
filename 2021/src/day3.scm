(load "utils.scm")
(use-modules (srfi srfi-1))

(define binary-nums
  (let ((lines (get-lines (get-filename 3))))
    (map
     (lambda (line)
       (map (lambda (c)
	      (string->number (string c)))
	    (string->list line)))
     lines)))

(define (bin->dec bin-list)
  (letrec ((rec
	    (lambda (ls s factor)
	      (if (null? ls)
		  s
		  (rec (cdr ls)
		       (+ s
			  (* factor
			     (car ls)))
		       (* factor 2))))))
    (rec
     (reverse bin-list) 0 1)))

(define (sum-lists ls1 ls2)
  (letrec* ((sum-lists-tail
	     (lambda (ls1 ls2 ret)
	       (if (null? ls1)
		   (reverse ret)
		   (let ((x (+ (car ls1) (car ls2))))
		     (sum-lists-tail
		      (cdr ls1)
		      (cdr ls2)
		      (cons x ret)))))))
    (sum-lists-tail ls1 ls2 `())))

(define (find-common comp nums)
  (let ((summed-nums (fold
		      sum-lists (car nums) (cdr nums)))
	(len (length nums)))
    (map (lambda (s)
	   (if (comp s (/ len 2))
	       1
	       0))
	 summed-nums)))

(define (find-most-common nums)
  (find-common >= nums))

(define (find-least-common nums)
  (find-common < nums))

(define common-bits
  (find-most-common binary-nums))

(define rare-bits
  (find-least-common binary-nums))

(define (flip-bits bits)
  (if (null? bits)
      `()
      (let ((bit
	     (if (= (car bits) 1)
		 0
		 1)))
	(cons
	 bit
	 (flip-bits (cdr bits))))))

(define part1
  (let ((gamma (bin->dec common-bits))
	(epsilon (bin->dec (flip-bits common-bits))))
    (* gamma epsilon)))

(define (filter-bits criteria nums pos)
  (if
   (= (length nums) 1)
   (car nums)
   (let*
       ((goal-bits (criteria nums))
	(filtered-bits
	 (filter
	  (lambda (bits)
	    (= (list-ref bits pos)
	       (list-ref goal-bits pos)))
	  nums)))
     (filter-bits criteria
		  filtered-bits
		  (+ pos 1)))))

(define part2
  (let
      ((oxy-rating (bin->dec (filter-bits find-most-common binary-nums 0)))
       (co2-rating (bin->dec (filter-bits find-least-common binary-nums 0))))
    (* oxy-rating co2-rating)))

(display-ln part1 part2)
