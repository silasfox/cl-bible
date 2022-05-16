;;;; cl-bible.lisp

(in-package #:cl-bible)

(defun start ()
  (initialize #'c:on-new-window)
  (open-browser))

(defun main ()
  (start)
  (loop))

(defun build ()
  (mapc (lambda (verse)
          (setf (cl-bible.verse:notes verse) nil))
        cl-bible.data:*bible*)
  #+sbcl (sb-ext:save-lisp-and-die "bible" :executable t :toplevel #'main))
