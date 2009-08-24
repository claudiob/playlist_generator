#-linux
(setf *DEFAULT-PATHNAME-DEFAULTS* (pathname "/Users/claudiob/Sites/lisp/"))
#+linux
(setf *DEFAULT-PATHNAME-DEFAULTS* (pathname "/home/lisp/www/"))
(compile-file "aux")
(compile-file "mod-lisp")
(compile-file "cbr-play-rec")
(load "aux")
(load "mod-lisp")
(load "cbr-play-rec")
