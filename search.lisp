;;;; search.lisp

(in-package #:cl-bible)

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

(defun string->verse (string)
  (uiop:split-string string :separator '(#\Tab)))

(defun verse-to-string (verse)
  (format nil "~A ~A:~A<br/> ~A" (cadr verse)
          (nth 3 verse)
          (nth 4 verse)
          (nth 5 verse)))

(defun find-in-bible (bible phrase)
  (remove-if-not (lambda (verse) (search phrase (nth 5 verse))) bible))

(defun find-book (bible book)
  (remove-if-not (lambda (verse) (search book (car verse))) bible))

(defun find-chapter (book chapter)
  (remove-if-not (lambda (verse) (string= chapter (nth 3 verse))) book))
