(load "utils.scm")

(define (get-positions)
  (map string->number
       (string-split
	(car (get-lines (get-filename 7)))
	#\,)))

(define positions (get-positions))

(define min-pos
  (apply min positions))

(define max-pos
  (apply max positions))

(define part1
  (letrec ((cost-to-move
	    (lambda (target-pos)
	      (apply + (map (lambda (pos)
			      (abs (- target-pos pos)))
			    positions)))))
    (apply min (map cost-to-move
		    (iota (1+ (- max-pos min-pos)))))))
  
(define part2
  (letrec ((sum-to-n
	    (lambda (n)
	      (/ (* n (1+ n)) 2)))
	   (cost-to-move
	    (lambda (target-pos)
	      (apply + (map (lambda (pos)
			      (sum-to-n (abs (- target-pos pos))))
			    positions)))))
    (apply min (map cost-to-move
		    (iota (1+ (- max-pos min-pos)))))))

(display-ln part1 part2)
