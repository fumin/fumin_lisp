(defvar *uni-table* (make-hash-table :test #'equal)) (defvar *uni-total* 0)
(defvar *bi-table* (make-hash-table :test #'equal)) (defvar *bi-total* 0)
(defvar *tri-table* (make-hash-table :test #'equal)) (defvar *tri-total* 0)

(defconstant c1 1/3) (defconstant c2 1/3) (defconstant c3 1/3)
(defconstant unk-list (list "<EMAIL>" "<NUM>" "<UNK>"))
(dolist (x unk-list) (setf (gethash x *uni-table*) 0)) 

(defun classify-into-unk (cleaned-str)
  (cond ((is-email-p cleaned-str) (return-from classify-into-unk "<EMAIL>"))
        ((is-num-p   cleaned-str) (return-from classify-into-unk "<NUM>"  ))
        (t                        (return-from classify-into-unk "<UNK>"  )))) 

(defun probability (strlist)
  (let (w1)
    (+ (* (/ (nil-is-0 (gethash (setf w1 (third strlist)) *uni-table*)) 
             *uni-total*)
          c1)
       (* (/ (nil-is-0 (gethash (cons (second strlist) w1) *bi-table*)) 
             *bi-total*)
          c2)
       (* (/ (nil-is-0 (gethash strlist *tri-table*)) 
             *tri-total*)
          c3))))

(defun prob-file (file)
  (let ((previous (cons "" "")) 1_2 (*p* 1)) (declare (special *p*))
    (with-open-file (stream file :direction :input)
      (setf 1_2 (find-1-2 (read-line stream  nil 'eof) 0 nil))
      (unless (and (listp 1_2) (eql (length 1_2) 2)) (return-from prob-file 'error)))
    (calculate-prob (second 1_2) "" "") 
    (calculate-prob (first 1_2) (if (gethash (second 1_2) *uni-table*) 
                                    (second 1_2) 
                                    (classify-into-unk (second 1_2))) 
                    "")
    (setf *p* (/ 1 *p*))
    (with-open-file (stream file :direction :input) 
      (loop until (eq (setf previous 
                            (space-sep (read-line stream  nil 'eof) 0 (car previous) (cdr previous) #'calculate-prob)) 
                      'eof)))
    *p*))

(defun analyse-file (file)
  (let ((previous (cons "" "")) 1_2 (pre-uni-total *uni-total*))
    (with-open-file (stream file :direction :input)
      (setf 1_2 (find-1-2 (read-line stream  nil 'eof) 0 nil))
      (unless (and (listp 1_2) (eql (length 1_2) 2)) (return-from analyse-file 'error)))
    (with-open-file (stream file :direction :input)
      (loop until (eq (setf previous 
                            (space-sep (read-line stream  nil 'eof) 0 (car previous) (cdr previous) #'record-freq)) 
                      'eof)))
    (remhash (list "" "" (second 1_2)) *tri-table*)(remhash (list "" "" (classify-into-unk (second 1_2))) *tri-table*)
    (remhash (list "" (second 1_2) (first 1_2)) *tri-table*)(remhash (list "" (classify-into-unk (second 1_2)) (first 1_2)) *tri-table*)
    (remhash (list "" (second 1_2) (classify-into-unk (first 1_2))) *tri-table*)(remhash (list "" (classify-into-unk (second 1_2)) (classify-into-unk (first 1_2))) *tri-table*)
    (remhash (cons "" (second 1_2)) *bi-table*)(remhash (cons "" (classify-into-unk (second 1_2))) *bi-table*)
    (decf *tri-total* 2)(decf *bi-total*)
    (dolist (table (list *uni-table* *bi-table* *tri-table*))
      (maphash (lambda (key val) 
                 (when (eql val 0) (remhash key table))) 
               table))
    (- *uni-total* pre-uni-total)))

(defun find-1-2 (string start rv)
  "return value rv has the properties:
  (car rv) = second word in file, (second rv) = first word in file"
  (unless (stringp string) (return-from find-1-2 string))
  (let* ((end (position #\space string :start start))
         (substr (subseq string start end)))
    (unless (equal (clean-str substr) "")
      (push (clean-str substr) rv))
    (cond ((eql 2 (length rv)) (return-from find-1-2 rv))
          (end (find-1-2 string (+ 1 end) rv))
	  (t (return-from find-1-2 rv)))))

(defun space-sep (string start pre1 pre2 fn)
  "the recursive func to split a line of text by white space.
  parameter fn is the function that does the hard work, which
  our case can be either the two: calculate-prob, record-freq"
  (unless (stringp string) (return-from space-sep string))
  (let* ((end (position #\space string :start start))
         (substr (clean-str (subseq string start end))))
    (cond ((and end (equal substr "")) (space-sep string (+ 1 end) pre1 pre2 fn))
          (end (space-sep string (+ 1 end) (funcall fn substr pre1 pre2) pre1 fn))
          ((equal substr "") (return-from space-sep (cons pre1 pre2)))
	  (t (return-from space-sep (cons (funcall fn substr pre1 pre2) pre1))))))

(defun calculate-prob (str pre1 pre2)
  (let (str-occurance)
    (unless (setf str-occurance (gethash str *uni-table*))
            (setf str (classify-into-unk str)))
    (setf *p*
          (* *p*
             (+ (* (/ (if str-occurance str-occurance (gethash str *uni-table*)) *uni-total*)
                   c1)
                (* (/ (nil-is-0 (gethash (cons pre1 str) *bi-table*)) *bi-total*)
                   c2)
                (* (/ (nil-is-0 (gethash (list pre2 pre1 str) *tri-table*)) *tri-total*)
                   c3))))
    str))

(defun record-freq (str pre1 pre2)
  (when (eql (setf (gethash str *uni-table*) 
                   (incf0-on-nil (gethash str *uni-table*)))
             0)
        (setf str (record-as-unk str))) 
  (setf (gethash (cons pre1 str) *bi-table*) 
        (incf1-on-nil (gethash (cons pre1 str) *bi-table*)))
  (setf (gethash (list pre2 pre1 str) *tri-table*) 
        (incf1-on-nil (gethash (list pre2 pre1 str) *tri-table*)))
  (incf *uni-total*)(incf *bi-total*)(incf *tri-total*)
  str)

(defun record-as-unk (cleaned-str)
  (setf cleaned-str (classify-into-unk cleaned-str))
  (setf (gethash cleaned-str *uni-table*)
        (incf (gethash cleaned-str *uni-table*)))
  cleaned-str)

(defun is-email-p (str)
  (let (p@)
    (when (null (setf p@ (position #\@ str))) (return-from is-email-p nil))
    (if (find #\. (subseq str (position #\@ str))) 
        t
        nil)))

(defun is-num-p (str)
  (every (lambda (x) (find x (list #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\-)))
         str))

(defun clean-str (str)
  (delete-if (lambda (x) (member x (list #\" #\. #\! #\, #\? #\: #\;)))
             str))

(defun nil-is-0 (x)
  (if (null x)
      0
      x))

(defun incf1-on-nil (x)
  (if (null x)
      1
      (incf x)))

(defun incf0-on-nil (x)
  (if (null x)
      0
      (incf x)))