(load "utils.scm")
(use-modules (ice-9 match)) ;; for pattern matching

(define (get-input)
  (map (lambda (line)
	 (map (lambda (s)
		(string-split (string-trim-both s #\space)
			      #\space))
	      (string-split line #\|)))
       (get-lines (get-filename 8))))

(define (get-patterns entry)
  (car entry))

(define (get-output entry)
  (cadr entry))

(define part1
  (letrec* ((entries (get-input))
	    (outputs (map get-output entries)))
    (apply +	
	   (map (lambda (output)
		  (apply +
			 (map (lambda (s)
				(let ((len (string-length s)))
				  (if (or (= len 2)  ;; 1
					  (= len 4)  ;; 4
					  (= len 3)  ;; 7
					  (= len 7)) ;; 8
				      1
				      0)))
			      output)))
		outputs))))

;; digit => len
;; 0 => 6  a b c - e f g
;; 1 => 2  - - c - - f - *
;; 2 => 5  a - c d e - g 
;; 3 => 5  a - c d - f g
;; 4 => 4  - b c d - f g *
;; 5 => 5  a b - d - f g
;; 6 => 6  a b - d e f g
;; 7 => 3  a - c - - f - *
;; 8 => 7  a b c d e f g *
;; 9 => 6  a b c d - f g

(define part2
  (letrec* ((entries (get-input))
	    (get-output-value
	     (lambda (entry)
	       (let* ((patterns (get-patterns entry))
		      (output (get-output entry))
		      (two-set (string->char-set
				(car (filter (lambda (s) (= (string-length s) 2)) patterns))))
		      (four-set (string->char-set
				 (car (filter (lambda (s) (= (string-length s) 4)) patterns)))))
		 (string->number
		  (list->string
		   (map (lambda (s)
			  (let* ((c-set (string->char-set s))
				 (len (char-set-size c-set))
				 (len-2 (char-set-size (char-set-intersection two-set c-set)))
				 (len-4 (char-set-size (char-set-intersection four-set c-set))))
			    (match `(,len ,len-4 ,len-2)
				   ((2 _ _) #\1)
				   ((3 _ _) #\7)
				   ((4 _ _) #\4)
				   ((7 _ _) #\8)
				   ((5 2 _) #\2)
				   ((5 3 1) #\5)
				   ((5 3 2) #\3)
				   ((6 4 _) #\9)
				   ((6 3 1) #\6)
				   ((6 3 2) #\0))))			    
			output)))))))
    (apply +
	   (map get-output-value
		entries))))

(display-ln part1 part2)
