;;;; search.lisp

(in-package #:cl-bible.verse)

(defclass verse ()
  ((%bname :initarg :bname
           :reader bname)
   (%bsname :initarg :bsname
            :reader bsname)
   (%chapter :initarg :chapter
             :reader chapter)
   (%number :initarg :number
            :reader vnumber)
   (%text :initarg :text
          :reader text)))

(defmethod print-object ((verse verse) stream)
  (format stream (verse-to-string verse " ")))

(defun string-to-verse (string)
  (let ((verse (uiop:split-string string :separator '(#\Tab))))
    (make-instance 'verse :bname (car verse)
                          :bsname (nth 1 verse)
                          :chapter (nth 3 verse)
                          :number (nth 4 verse)
                          :text (nth 5 verse))))

(defgeneric verse-to-string (verse &optional separator))
(defmethod verse-to-string (verse &optional (separator " "))
  (format nil "~A ~A:~A~A~A" (cadr verse)
          (nth 3 verse)
          (nth 4 verse)
          separator
          (nth 5 verse)))
(defmethod verse-to-string ((verse verse) &optional (separator "<br/>"))
  (format nil "~A ~A:~A~A ~A" (bsname verse)
          (chapter verse)
          (vnumber verse)
          separator
          (text verse)))
