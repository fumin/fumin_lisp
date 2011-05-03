(with-open-file (stream "code:greplin1.txt" :direction :input)
  (defconstant mary (read-line stream))
  (defconstant mary-len-1 (- (length mary) 1))
  (defconstant nmary
    (loop for i from 0 to mary-len-1
	  collect i)))

(defun greplin1 ()
  (let* ((gans (walk nmary))
	 (centre (caar gans))
	 (width (cdr gans)))
    (subseq mary (- centre width) (+ centre width 1))))
    

(defun walk (maryn &optional (i 1))
  "Calls itself recursively, each time
  enlarge the width by 1"
  (unless maryn (return-from walk nil))
  (cond ((walk (isthere maryn i) (+ i 1)))
	(t (cons maryn (- i 1)))))

(defun isthere (maryn i)
  "Finds all remaining candidate positions
   that has a symmetrical letter at +i and -i"
  (find-all-if #'(lambda (x)
		   (and (<= i x)
			(<= (+ x i) mary-len-1)
			(eq (aref mary (+ x i))
			    (aref mary (- x i)))))
	       maryn))

(defun greplin2 ()
  (spd (+ (fibo 29) 1)))

(defun fibo (n)
  (round (/ (expt (/ (+ 1 (sqrt 5)) 2) n) (sqrt 5))))

(defun spd (x)
  "The sum of prime divisors of X"
  (loop for prime in (prime-numbers<x (sqrt x))
	when (eq 0 (mod x prime))
	  sum prime))    

(defun prime-numbers<x (x)
  (loop for i from 2 to (floor x)
	if (primep i) collect it))

(defun primep (i)
  (loop for j from 2 to (floor (sqrt i))
	when (eq 0 (mod i j))
	  do (return-from primep nil)
	finally (return i)))

(defun greplin3 ()
  (g3 '(3 4 9 14 15 19 28 37 47 50 54 56 59 61 70 73 78 81 92 95 97 99)))

(defun g3 (input)
  (loop for item in (maplist #'(lambda (x) (yo (reverse (rest x)) (car x)))
			     (reverse input))
	sum item))

(defun yo (seq x)
  "Find the number of subsequences
  in this seq that sums to x."
  ;;;(format t "in yo: seq=~s, x=~s~%" seq x)
  (let ((ans 0) temp lyoyo ryoyo (len/2 (floor (length seq) 2)))
    (setq temp
	  (mapcar #'(lambda (y) (yoyo (append1 y 0)))
		  (multiple-value-list (split seq len/2))))
    (setq lyoyo (first temp)) (setq ryoyo (reverse (second temp)))
    ;;; lyoyo is (small ... big), ryoyo is (big ... small)
    ;;;(princ lyoyo) (terpri) (princ ryoyo) (terpri)
    (loop for lowe = (first lyoyo) then (first lyoyo)
	  for highe = (first ryoyo) then (first ryoyo)
	  ;;;do (format t "lowe: ~s, highe: ~s~%" lowe highe)
	  when (or (null ryoyo) (and (not (null lyoyo)) (< (+ lowe highe) x)))
	    do (pop lyoyo)
	  else
	    when (or (null lyoyo) (> (+ lowe highe) x))
	      do (pop ryoyo)
	    else
	      do (incf ans)
	      and when (and (< 1 (length lyoyo)) (eql (second lyoyo) lowe))
	            do (pop lyoyo)
	          else
	            do (pop ryoyo)
	  until (and (null lyoyo) (null ryoyo)))
    ;;;(princ ans) (terpri)
    ans))

(defun yoyo (half-list)
  "Find the all sums of all sub-combinations in this half-list.
  The returns sub-sums are sorted (small ... big)."
  (when (eql 1 (length half-list)) (return-from yoyo half-list))
  (let ((result-yoyo (yoyo (cdr half-list)))
	(car-half-list (car half-list)))
    (merge 'list result-yoyo
	   (mapcar #'(lambda (x) (+ x car-half-list)) result-yoyo) #'<)))