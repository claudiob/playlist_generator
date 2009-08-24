;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;;                                                  file: mod-lisp.lisp  ;;
;;                                                                       ;;
;;                                   author: Claudio Baccigalupo @ IIIA  ;;
;;                  mod-lisp was originally developed by: Marc Battyani  ;;
;;                                                                       ;;
;;                                                                       ;;
;;                                                     date: 2006.04.27  ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Web interface for cbr-play-rec
;;
;; This code implements the web interface; actually it has been tested
;; on two different server configurations:
;; - LispWorks on MacOsX
;; - CMUCL on Debian

;; version 1

;;-------------------------------------------------------------------------
;;
;; SETTING UP THE WEB SERVER
;;
;;-------------------------------------------------------------------------

;; This section deals with connecting to the Apache Web Server on a
;; specific port, and starting listening on that port. Basically, the
;; Apache configuration file should redirect requests coming on that port
;; to the Lisp module.


#+lispworks
(require "comm")

;; 22.12.06 Changed because 3000 was already taken
(defconstant +apache-port+ 3000
  "Port to connect to Apache.")

(defconstant +default-page+ "index.html"
  "Page to show when with no specific request.")

(defvar *apache-socket* nil
 "Socket to apache.")

(defvar *close-apache-socket* t
  "Close the socket to Apache after request?")

(defvar *apache-nb-use-socket* 0
  "Number of requests sent in the socket.")

;; Apache-Listen contains the main LISTENING loop. For every incoming
;; request, it calls Decode-HTTP-Request to decode it, and if it is a
;; valid request it calls Eval-HTTP-Request to evaluate it.

(defun apache-listen (*apache-socket*)
  "Get every request waiting on the socket from Apache and process it."
  (let ((*close-apache-socket* t))
    (unwind-protect
      (loop for *apache-nb-use-socket* from 0
	    for request = (decode-http-request)
	    while request 
            do (eval-http-request request)
               (force-output *apache-socket*)
	    until *close-apache-socket*)
      (close *apache-socket*))))

#+lispworks
(defun make-apache-instream (handle)
  "Open a stream to Apache (LispWorks only!)."
  (mp:process-run-function 
   (format nil "apache socket ~d" handle) '()
   'apache-listen (make-instance 'comm:socket-stream :socket handle
                                 :direction :io :element-type 'base-char)))

#+cmu
(defun make-apache-listener (port)
  "Create a listener for Apache on PORT (CMUCL/x86 only!)."
  (let ((socket (ext:create-inet-listener port)))
    (unwind-protect
        (loop
         (mp:process-wait-until-fd-usable socket :input)
         (multiple-value-bind (new-fd remote-host)
             (ext:accept-tcp-connection socket)
           (let ((stream (sys:make-fd-stream new-fd :input t :output t)))
             (mp:make-process #'(lambda () (apache-listen stream))))))
      (unix:unix-close socket))))

(defun start-apache-listener ()
  "Start accepting requests from Apache."
  #+lispworks
  (comm:start-up-server :function 'make-apache-instream 
                        :service +apache-port+)
  #+cmu
  (mp:make-process #'(lambda () (make-apache-listener +apache-port+)))
)




(start-apache-listener)

;;-------------------------------------------------------------------------
;;
;; RESPONDING TO REQUESTS FROM THE WEB
;;
;;-------------------------------------------------------------------------

;; Decode-HTTP-Request is called by Apache-Listen when there is a 
;; request pending on the *apache-socket*.

(defun decode-http-request ()
  "Return a list where the Request-URL is the first element, and the
   Request-Parameters (passed either with a GET or a POST method) are
   the other elements. For instance:
   REQUEST> [..]/run?id=123&k=30 ==> RESULT> (RUN :ID 123 :K 30) ."
  (ignore-errors
    ; Should any HTTP parameter not exist, ignore-errors will return NIL
    (let* ((headers (loop for key = (read-line *apache-socket* nil nil)
			 while (and key (string-not-equal key "end"))
			 for value = (read-line *apache-socket* nil nil)
			 collect (cons key value)))
	  (method (cdr (assoc "method" headers :test #'equalp)))
          (content-length (cdr (assoc "content-length" headers 
                                      :test #'equalp)))
          (uri (cdr (assoc "url" headers :test #'equalp)))
          (routine-start (1+ (position #\/ uri :from-end t)))
          (routine-end (position #\? uri))
          (routine (if routine-end
                       (subseq uri routine-start routine-end)
                     (subseq uri routine-start)))
          (requests-string 
           (cond ((equalp method "post")
                  ; POST method: requests are passed in the content
                  (let ((buffer 
                         (make-string (parse-integer content-length))))
                    (read-sequence buffer *apache-socket*)
                    buffer))
                 ((equalp method "get")
                  ; GET method: requests are passed in the url
                  (when routine-end
                    (subseq uri (1+ routine-end))))))
          (requests-list 
           (when requests-string
             (string->list (concatenate 'string ":"
              (substitute #\SPACE #\= 
                         (replace-substring requests-string "&" " :")))))))
      (when routine
        (if (search "index" routine) 'index
          (append (string->list routine) 
                  (if (oddp (length requests-list))
                      (butlast requests-list) requests-list)))))))
         
(defun eval-http-request (request)
  "Return the result of applying the Request-URL to the Request-Parameters,
   as if Request-URL were a LISP function and Request-Parameters its para-
   meters. If this is not the case, or if any error happens, return NIL.
   Also, writes on *apache-socket* the resulting web-page, whether HTML in
   case of error, or XML in case of success."
  (multiple-value-bind (result error)
      (if (eql request 'index)
          (show-default-content)
          (handler-case 
              (apply (first request) (rest request))
            (error (condition)
                   (values (format nil "~a" condition) t))))
    (write-header-line "Status" 
                       (if error "400 Bad Request" 
                         (if result "200 OK" "204 No Content")))
    (write-header-line "Content-Type"
                       (cond ((search "<?xml" result) "text/xml")
                             ((search "<html" result) "text/html")
                             (t "text/plain")))
    (write-header-line "Content-Length" 
                       (format nil "~d" (length result)))
    (write-string "end" *apache-socket*)
    (write-char #\NewLine *apache-socket*)
    (when result (write-string result *apache-socket*))
    (setf *close-apache-socket* nil))) 


;;-------------------------------------------------------------------------
;;
;; AUXILIAR FUNCTIONS
;;
;;-------------------------------------------------------------------------

(defun write-header-line (key value)
  (write-string key *apache-socket*)
  (write-char #\NewLine *apache-socket*)
  (write-string value *apache-socket*)
  (write-char #\NewLine *apache-socket*))

(defun show-default-content ()
  (read-file-to-string +default-page+))


                    

