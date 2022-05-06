;;;; cl-bible.lisp

(in-package #:cl-bible)

(defun start ()
  (initialize #'c:on-new-window)
  (open-browser))

(defun main ()
  (start)
  (loop))

(defun build ()
  (sb-ext:save-lisp-and-die "bible" :executable t :toplevel #'main))
