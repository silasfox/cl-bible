;;;; cl-bible.lisp

(in-package #:cl-bible)

(defun start ()
  (initialize #'c:on-new-window)
  (open-browser))

(defun main ()
  (start)
  (loop))

(defun build ()
  (mapc (lambda (bible) (mapc (lambda (verse) (setf (cl-bible.verse:notes verse) nil)) bible)) (list cl-bible.data::*mng* cl-bible.data::*kjv* cl-bible.data::*vul* cl-bible.data::*grb*))
  #+sbcl (sb-ext:save-lisp-and-die "bible" :executable t :toplevel #'main))
