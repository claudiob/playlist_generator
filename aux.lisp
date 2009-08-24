
;;-------------------------------------------------------------------------
;;
;; AUXILIARY FUNCTIONS (FOR DEBUGGING)
;;
;;-------------------------------------------------------------------------

;; Code from Paradigms of AI Programming - 1991 Peter Norvig

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun start-dbg (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun stop-dbg (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(start-dbg :Load)



;;-------------------------------------------------------------------------
;;
;; OTHER AUXILIARY FUNCTIONS
;;
;;-------------------------------------------------------------------------

(defun n-first (n)
  #'(lambda (list)
      (butlast list (max 0 (- (length list) n)))))

(defun first-n (list n)
  "Return the first N elements of LIST."
  (butlast list (max 0 (- (length list) n))))

(defun string->list (string)
  "Convert a string to a list of words."
  (read-from-string (concatenate 'string "(" string ")")))

(defun el->string (el)
  (with-output-to-string (s) (princ el s)))

(defun ssubseq (sequence start end) 
  (let ((from (if (< start 0) 0
                (if (> start (length sequence)) (length sequence) start)))
        (to (if (< end 0) 0
              (if (> end (length sequence)) (length sequence) end))))
    (subseq sequence from to)))

(defun ssubseq2 (sequence start end)
  (if (or (< start 0) (> start (length sequence))
          (< end 0) (> end (length sequence)))
      nil (subseq sequence start end)))

(defun replace-substring (in-string old new) 
  (let ((result ""))
    (do ((begin 0)
	 (end (search old in-string) 
	      (search old in-string :start2 begin)))
	((>= begin (length in-string)) 'done)
      (if end
	  (progn (setf result (concatenate 'string result 
					   (subseq in-string begin end)
					   new))
		 (setf begin (+ end (length old))))
	  (progn (setf result (concatenate 'string result 
					   (subseq in-string begin
						   (length in-string))))
		 (setf begin (length in-string)))))
    result))

(defun read-file-to-string (file)
  "Opens a reads a file. Returns the contents as a single string"
    (with-open-file (in file :direction :input :if-does-not-exist nil) 
      (with-output-to-string (out)
        (let ((eof (gensym)))
          (do ((line (read-line in nil eof)
                     (read-line in nil eof)))
              ((eq line eof))
            (format out "~A~%" line))))))