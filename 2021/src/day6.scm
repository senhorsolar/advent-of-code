(load "utils.scm")

(define (get-input)
  (map string->number
       (string-split
	(car (get-lines
	      (get-filename 6)))
	#\,)))

(define (count ls v)
  (length (filter (lambda (x)
		    (= x v))
		  ls)))

(define (get-linput)
  (let ((input (get-input)))
    (map (lambda (i)
	   (count input i))
	 (iota 9))))

(define (count-fish days)
  (letrec*
      ((step
	(lambda (input)
	  (list (list-ref input 1)  ;; 0
		(list-ref input 2)  ;; 1
		(list-ref input 3)  ;; 2
		(list-ref input 4)  ;; 3
		(list-ref input 5)  ;; 4
		(list-ref input 6)  ;; 5
		(+ (list-ref input 7)  ;; 6
		   (list-ref input 0))
		(list-ref input 8)  ;; 7
		(list-ref input 0))))  ;; 8
       (loop
	(lambda (input days-left)
	  (if (> days-left 0)
	      (loop (step input)
		    (- days-left 1))
	      input))))
    (apply + (loop
	      (get-linput)
	      days))))

(define part1
  (count-fish 80))

(define part2
  (count-fish 256))

(display-ln part1 part2)
