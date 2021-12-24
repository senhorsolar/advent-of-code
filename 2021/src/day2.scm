;; Load modules
(load "utils.scm")
(use-modules (srfi srfi-1))

(define filename (get-filename 2))

;; Get list of commands (forward | down . distance)
(define commands
  (let* ((parse-line
	  (lambda (line)
	    (let* ((pair (string-split line #\space))
		   (direction (list-ref pair 0))
		   (distance (string->number (list-ref pair 1))))
	      (cons direction distance))))
	 (lines (get-lines filename)))
    (map parse-line lines)))

(define (get-dir command)
  (car command))

(define (get-dist command)
  (cdr command))

(define (inc-pos pos dir amount)
  (let ((cur-amount (assoc-ref pos dir)))
    (set! pos
	  (assoc-set! pos dir (+ cur-amount amount)))))

(define (inc-horiz pos amount)
  (inc-pos pos "horizontal" amount))

(define (inc-depth pos amount)
  (inc-pos pos "depth" amount))

(define (inc-aim pos amount)
  (inc-pos pos "aim" amount))

(define (get-horiz pos)
  (assoc-ref pos "horizontal"))

(define (get-depth pos)
  (assoc-ref pos "depth"))

(define (get-aim pos)
  (assoc-ref pos "aim"))

(define part1
  (letrec ((move
	    (lambda (command pos)
	      (let* ((dir (get-dir command))
		     (dist (get-dist command)))
		(cond ((string=? dir "forward")
		       (inc-horiz pos dist))
		      ((string=? dir "up")
		       (inc-depth pos (- dist)))
		      ((string=? dir "down")
		       (inc-depth pos dist)))
		pos)))
	   (pos (list (cons "horizontal" 0)
		      (cons "depth" 0))))
    (fold move pos commands)
    (* (get-depth pos) (get-horiz pos))))
    
(define part2
  (letrec ((move
	    (lambda (command pos)
	      (let* ((dir (get-dir command))
		     (dist (get-dist command)))
		(cond ((string=? dir "forward")
		       (begin
			 (inc-horiz pos dist)
			 (inc-depth pos (* (get-aim pos) dist))))
		      ((string=? dir "up")
		       (inc-aim pos (- dist)))
		      ((string=? dir "down")
		       (inc-aim pos dist)))
		pos)))
	   (pos (list (cons "horizontal" 0)
		      (cons "depth" 0)
		      (cons "aim" 0))))
    (fold move pos commands)
    (* (get-depth pos) (get-horiz pos))))

(display-ln part1 part2)
